{-# LANGUAGE DeriveFunctor #-}

-- Minimal Viable Tidal

module MVT where

import           Control.Applicative (liftA2)
import           Data.Maybe          (fromMaybe)
-- So we can overload *> and <* as alternatives to <*>
import           Data.List           (intercalate)
import           Data.Ratio
import           Prelude             hiding ((*>), (<*))

-- ********
-- | Time *
-- ********
type Time = Rational

sam :: Time -> Time
sam s = toRational (floor s :: Int)

-- | The start of the next cycle
nextSam :: Time -> Time
nextSam = (+1) . sam

-- ***********************
-- | Arc (aka time span) *
-- ***********************
data Arc = Arc { aBegin :: Time, aEnd :: Time}
  deriving (Eq, Ord, Show)

-- | Intersection of two arcs
sect :: Arc -> Arc -> Arc
sect (Arc b e) (Arc b' e') = Arc (max b b') (min e e')

-- | Returns the whole cycle arc that the given time is in
timeToCycle :: Time -> Arc
timeToCycle t = Arc (sam t) (nextSam t)

-- | Splits a timespan at cycle boundaries
splitArcs :: Arc -> [Arc]
splitArcs (Arc s e) | s == e = [Arc s e] -- support zero-width arcs
                    | otherwise = splitArcs' (Arc s e) -- otherwise, recurse
  where splitArcs' (Arc s' e') | e' <= s' = []
                               | sam s' == sam e' = [Arc s' e']
                               | otherwise
          = Arc s' (nextSam s') : splitArcs' (Arc (nextSam s') e')

-- | Similar to 'fmap' but time is relative to the cycle (i.e. the
-- sam of the start of the arc)
mapCycle :: (Time -> Time) -> Arc -> Arc
mapCycle f (Arc s e) = Arc (sam' + f (s - sam')) (sam' + f (e - sam'))
         where sam' = sam s

-- *********
-- | Event *
-- *********
-- A discrete value with a whole timespan, or a continuous one without
-- It might be a fragment of an event, in which case its 'active' arc
-- will smaller than its 'whole'.

data Event a = Event {whole :: Maybe Arc, active :: Arc, value :: a}
  deriving (Functor, Eq, Ord)

instance (Show a) => Show (Event a) where
  show e = show (whole e) ++ " " ++ show (active e) ++ " " ++ show (value e)

-- ***********
-- | Pattern *
-- ***********
-- A type class for patterns
class (Functor p, Applicative p, Monad p) => Pattern p where
  duration :: p a -> Time
  withTime :: (Time -> Time) -> (Time -> Time) -> p a -> p a
  innerBind, outerBind :: p a -> (a -> p b) -> p b
  bindParameter :: p a -> (a -> p b) -> p b
  cat :: [p a] -> p a
  timeCat :: [(Time, p a)] -> p a
  stack :: [p a] -> p a
  toSignal :: p a -> Signal a

silence :: (Monoid (p a), Pattern p) => p a
silence = mempty

innerJoin, outerJoin :: Pattern p => p (p b) -> p b
innerJoin s = innerBind s id
outerJoin s = outerBind s id

-- Patternification

-- Turns functions with non-patterned parameters into fully patternified ones

-- patternify the first parameter
patternify :: Pattern p => (a -> b -> p c) -> p a -> b -> p c
patternify f apat pat                 = apat `bindParameter` \a -> f a pat

-- patternify the first two parameters
patternify_p_p :: (Pattern p) => (a -> b -> c -> p d) -> p a -> p b -> c -> p d
patternify_p_p f apat bpat pat        = apat `bindParameter` \a -> (bpat `bindParameter` \b -> f a b pat)

-- patternify the first but not the second parameters
patternify_p_n :: Pattern p => (a -> b -> c -> p d) -> p a -> b -> c -> p d
patternify_p_n f apat b pat           = apat `bindParameter` \a -> f a b pat

-- patternify the first three parameters
patternify_p_p_p :: Pattern p => (a -> b -> c -> d -> p e) -> p a -> p b -> p c -> d -> p e
patternify_p_p_p f apat bpat cpat pat = apat `bindParameter` \a -> (bpat `bindParameter` \b -> (cpat `bindParameter` \c -> f a b c pat))

-- **********
-- | Signal *
-- **********
-- A pattern that's a function from a timespan to events active during
-- that timespan. A continuous signal, that can nonetheless contain
-- discrete events.
data Signal a = Signal {query :: Arc -> [Event a]}
  deriving (Functor)

instance Semigroup (Signal a) where a <> b = cat [a,b]
instance Monoid (Signal a)    where mempty = Signal $ const []
instance Monad Signal         where (>>=) = sigBindWith $ liftA2 sect
                                    return = pure
-- Define applicative from monad
instance Applicative Signal where
  pure v = Signal $ \q -> map (\arc -> Event (Just $ timeToCycle $ aBegin arc) arc v) $ splitArcs q
  pf <*> px = pf >>= \f -> px >>= \x -> pure $ f x

instance Pattern Signal where
  -- We always work with signals as if they have a duration of 1
  -- cycle, even though successive cycles very often differ
  duration _ = 1
  withTime fa fb pat = withEventTime fa $ withQueryTime fb pat
  -- | Alternative binds
  innerBind = sigBindWith $ flip const
  outerBind = sigBindWith const
  bindParameter = innerBind
  -- | Concatenate a list of signals, interleaving cycles.
  cat pats = splitQueries $ Signal $ \a -> query (_late (offset a) (pats !! mod (floor $ aBegin a) n)) a
    where offset arc = sam (aBegin arc) - sam (aBegin arc / toRational n)
          n = length pats
  timeCat tps = stack $ map (\(s,e,p) -> _compressArc (Arc (s/total) (e/total)) p) $ arrange 0 tps
    where total = sum $ map fst tps
          arrange :: Time -> [(Time, Signal a)] -> [(Time, Time, Signal a)]
          arrange _ []            = []
          arrange t ((t',p):tps') = (t,t+t',p) : arrange (t+t') tps'
  stack pats = Signal $ \a -> concatMap (`query` a) pats
  toSignal = id

-- | Split queries at sample boundaries. An internal function that
-- makes other functions easier to define, as events that cross cycle
-- boundaries don't need to be considered then.
splitQueries :: Signal a -> Signal a
splitQueries pat = Signal $ concatMap (query pat) . splitArcs

(<*), (*>) :: Pattern p => p (t -> b) -> p t -> p b
pf <* px = pf `innerBind` \f -> px `innerBind` \x -> pure $ f x
pf *> px = pf `outerBind` \f -> px `outerBind` \x -> pure $ f x
infixl 4 <*, *>

-- TODO - early/late don't work for sequences - define on instances
_early, _late, _fast, _slow :: Pattern p => Time -> p a -> p a
_early t = withTime (subtract t) (+ t)
_late t = withTime (+ t) (subtract t)
_fast t = withTime (/ t) (* t)
_slow t = withTime (* t) (/ t)

early, late, fast, slow :: Pattern p => p Time -> p a -> p a
early = patternify _early
late = patternify _late
fast = patternify _fast
slow = patternify _slow

withArcTime :: (Time -> Time) -> Arc -> Arc
withArcTime timef (Arc b e) = Arc (timef b) (timef e)

-- | @withEvents f p@ returns a new @Signal@ with f applied to the
-- resulting list of events for each query function @f@.
withEvents :: ([Event a] -> [Event b]) -> Signal a -> Signal b
withEvents f p = p {query = f . query p}

-- | @withEvent f p@ returns a new @Signal@ with f applied to each
-- event queried function @f@.
withEvent :: (Event a -> Event b) -> Signal a -> Signal b
withEvent f = withEvents (map f)

withEventTime :: (Time -> Time) -> Signal a -> Signal a
withEventTime timef = withEvent $ \e -> e {active = withArcTime timef $ active e,
                                           whole = withArcTime timef <$> whole e
                                          }

withEventArc :: (Arc -> Arc) -> Signal a -> Signal a
withEventArc arcf = withEvent $ \e -> e {active = arcf $ active e,
                                         whole = arcf <$> whole e
                                        }

withQuery :: (Arc -> Arc) -> Signal a -> Signal a
withQuery arcf sig = Signal $ \arc -> query sig $ arcf arc

withQueryMaybe :: (Arc -> Maybe Arc) -> Signal a -> Signal a
withQueryMaybe qf pat = Signal $ \q -> fromMaybe [] $ qf q >>= Just . query pat

withQueryTime :: (Time -> Time) -> Signal a -> Signal a
withQueryTime timef = withQuery $ withArcTime timef

-- Makes a signal bind, given a function of how to calculate the 'whole' timespan
sigBindWith :: (Maybe Arc -> Maybe Arc -> Maybe Arc) -> Signal a -> (a -> Signal b) -> Signal b
sigBindWith chooseWhole pv f = Signal $ \q -> concatMap match $ query pv q
  where match event = map (withWhole event) $ query (f $ value event) (active event)
        withWhole event event' = event' {whole = chooseWhole (whole event) (whole event')}

_zoomArc :: Arc -> Signal a -> Signal a
_zoomArc (Arc s e) p = splitQueries $ withEventArc (mapCycle ((/d) . subtract s)) $ withQuery (mapCycle ((+s) . (*d))) p
     where d = e-s

-- TODO - why is this function so long?
_fastGap :: Time -> Signal a -> Signal a
_fastGap factor pat = splitQueries $ withEvent ef $ withQueryMaybe qf pat
  -- A bit fiddly, to drop zero-width queries at the start of the next cycle
  where qf (Arc b e) | bpos < 1 = Just $ Arc (cyc + bpos) (cyc + epos)
                     | otherwise = Nothing
          where cyc = sam b
                bpos = min 1 $ (b - cyc) * factor
                epos = min 1 $ (e - cyc) * factor
        -- Also fiddly, to maintain the right 'whole' relative to the part
        ef ev = ev {whole = w', active = a'}
          where a = active ev
                b = aBegin a
                e = aEnd a
                a' = Arc (cyc + bpos) (cyc + epos)
                  where cyc = sam b
                        bpos = min 1 $ (b - cyc) / factor
                        epos = min 1 $ (e - cyc) / factor
                w' = do w <- whole ev
                        let b' = aBegin a' - ((b - aBegin w) / factor)
                            e' = aEnd a' + ((aEnd w - e) / factor)
                        return $ Arc b' e'

_compressArc :: Arc -> Signal a -> Signal a
_compressArc (Arc b e) pat | b > e || b > 1 || e > 1 || b < 0 || e < 0 = silence
                           | otherwise = _late b $ _fastGap (1/(e-b)) pat

-- | Similar to @fastCat@, but each signal is given a relative duration
sigTimeCat :: [(Time, Signal a)] -> Signal a
sigTimeCat tps = stack $ map (\(s,e,p) -> _compressArc (Arc (s/total) (e/total)) p) $ arrange 0 tps
    where total = sum $ map fst tps
          arrange :: Time -> [(Time, Signal a)] -> [(Time, Time, Signal a)]
          arrange _ []            = []
          arrange t ((t',p):tps') = (t,t+t',p) : arrange (t+t') tps'

-- ************
-- | Sequence *
-- ************
-- A pattern as a discrete, contiguous, finite sequence, that's
-- structured to support polyphonic stacks, and embedded subsequences
instance Functor Sequence where
  fmap f (Atom d i o v) = Atom d i o (f <$> v)
  fmap f (Cat xs)       = Cat $ map (fmap f) xs
  fmap f (Stack xs)     = Stack $ map (fmap f) xs

data Sequence a = Atom {atomDuration :: Time,
                        atomInset    :: Time,
                        atomOutset   :: Time,
                        atomValue    :: Maybe a
                       }
                | Cat [Sequence a]
                | Stack [Sequence a]
                deriving (Eq, Ord)

prettyRatio :: Rational -> String
prettyRatio r | denominator r == 1 = show $ numerator r
              | otherwise = show (numerator r) ++ "/" ++ show (denominator r)

instance (Show a) => Show (Sequence a) where
  show (Atom d _ _ Nothing) = "~" ++ "×" ++ prettyRatio d
  show (Atom d i o (Just v)) = show v ++ "×" ++ prettyRatio d ++ showio
    where showio | i == 0 && o == 0 = ""
                 | otherwise = "(" ++ prettyRatio i ++ "," ++ prettyRatio o ++ ")"
  show (Cat xs) = "[" ++ unwords (map show xs) ++ "]"
  show (Stack xs) = "[" ++ intercalate ", " (map show xs) ++ "]"

gap :: Time -> Sequence a
gap t = Atom t 0 0 Nothing

step :: Time -> a -> Sequence a
step t v = Atom t 0 0 $ Just v

seqJoinWith :: (Time -> Sequence a -> Sequence a) -> Sequence (Sequence a) -> Sequence a
seqJoinWith f (Atom d i o (Just seq)) = f (d + i + o) seq
seqJoinWith _ (Atom d i o Nothing)    = Atom d i o Nothing
seqJoinWith f (Cat xs)                = Cat $ map (seqJoinWith f) xs
seqJoinWith f (Stack xs)              = Stack $ map (seqJoinWith f) xs

-- Flatten, using outer duration as relative duration for inner
seqJoin :: Sequence (Sequence a) -> Sequence a
seqJoin = seqJoinWith _slow

-- Flatten, expanding inner to outer duration
seqOuterJoin :: Sequence (Sequence a) -> Sequence a
seqOuterJoin = seqJoinWith (\t seq -> _fast (duration seq / t) seq)

-- Flatten, repeating inner to total duration of outer
seqInnerJoin :: Sequence (Sequence a) -> Sequence a
seqInnerJoin = seqJoinWith seqTakeLoop

seqTakeLoop :: Time -> Sequence a -> Sequence a
seqTakeLoop 0 _ = gap 0
seqTakeLoop t pat@(Atom d i _ v) | t > d = seqTakeLoop t $ Cat $ repeat pat
                                 | otherwise = Atom t i (max 0 $ d - t) v
seqTakeLoop t (Stack ss) = Stack $ map (seqTakeLoop t) ss
-- TODO - raise an error?
seqTakeLoop t (Cat []) = Cat []
seqTakeLoop t (Cat ss) = Cat $ loop t $ cycle ss
  where loop :: Time -> [Sequence a] -> [Sequence a]
        loop t' (s:ss') | t' <= 0 = []
                        | t' <= stepDur = [seqTakeLoop t' s]
                        | otherwise = seqTakeLoop stepDur s : loop (t' - stepDur) ss'
          where stepDur = duration s

withAtom :: (Sequence a -> Sequence a) -> Sequence a -> Sequence a
withAtom f a@Atom {}  = f a
withAtom f (Cat xs)   = Cat $ map (withAtom f) xs
withAtom f (Stack xs) = Stack $ map (withAtom f) xs

withAtomTime :: (Time -> Time) -> Sequence a -> Sequence a
withAtomTime f = withAtom (\(Atom d i o v) -> Atom (f d) (f i) (f o) v)

instance Semigroup (Sequence a) where
  a <> b = cat [a,b]

instance Monoid (Sequence a) where
  mempty = gap 1

instance Monad Sequence where
  return = pure
  seqv >>= f = seqJoin $ fmap f seqv

instance Applicative Sequence where
  pure = step 1
  pf <*> px = pf >>= \f -> px >>= \x -> pure $ f x

instance Pattern Sequence where
  withTime f _ pat = withAtomTime f pat
  cat = Cat   -- TODO - shallow cat?
  stack = Stack
  duration (Atom d _ _ _) = d
  duration (Cat xs)       = sum $ map duration xs
  duration (Stack [])     = 0
  duration (Stack (x:_))  = duration x
  timeCat seqs = seqJoin $ Cat $ map (uncurry step) seqs
  seqv `outerBind` f = seqOuterJoin $ fmap f seqv
  seqv `innerBind` f = seqInnerJoin $ fmap f seqv
  bindParameter = (>>=)
  -- One beat per cycle..
  toSignal pat = _slow (duration pat) $ toSignal' pat
    where
      -- One sequence per cycle
      toSignal' (Atom d i o (Just v)) | d == 0 = error "whoops"
                                      | otherwise = _zoomArc (Arc (i/t) (1-(o/t))) $ pure v
        where t = d + i + o
      toSignal' (Atom _ _ _ Nothing) = silence
      toSignal' (Cat xs) = timeCat timeseqs
        where timeseqs = map (\x -> (duration x, toSignal' x)) xs
      toSignal' (Stack xs) = stack $ map toSignal' xs


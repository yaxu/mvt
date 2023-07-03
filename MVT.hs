{-# LANGUAGE DeriveFunctor #-}

-- Minimal Viable Tidal

module MVT where

import           Control.Applicative
import           Data.Maybe          (fromMaybe)
-- ********
-- | Time *
-- ********
type Time = Rational

sam :: Time -> Time
sam s = (toRational :: Int -> Time) $ floor s

-- | The start of the next cycle
nextSam :: Time -> Time
nextSam s = sam s + 1

-- ***********************
-- | Arc (aka time span) *
-- ***********************
data Arc = Arc { aBegin :: Time, aEnd   :: Time}
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
  manipTime :: (Time -> Time) -> (Time -> Time) -> p a -> p a
  innerBind, outerBind :: p a -> (a -> p b) -> p b
  cat :: [p a] -> p a
  stack :: [p a] -> p a
  toSignal :: p a -> Signal a
  timeCat :: [(Time, p a)] -> p a

silence :: (Monoid (p a), Pattern p) => p a
silence = mempty

-- **********
-- | Signal *
-- **********
-- A pattern that's a function from a timespan to events active during
-- that timespan
data Signal a = Signal {query :: Arc -> [Event a]}
  deriving (Functor)

instance Semigroup (Signal a) where
  a <> b = cat [a,b]

instance Monoid (Signal a) where
  mempty = Signal $ const []

instance Monad Signal where
  (>>=) = sigBind $ liftA2 sect

instance Applicative Signal where
  pure = return
  pf <*> px = pf >>= \f -> px >>= \x -> pure $ f x

instance Pattern Signal where
  duration _ = 1
  manipTime fa fb pat = withEventTime fa $ withQueryTime fb pat
  -- | Alternative binds
  innerBind = sigBind $ flip const
  outerBind = sigBind const
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

_early, _late, _fast, _slow :: Pattern p => Time -> p a -> p a
_early t = manipTime (subtract t) (+ t)
_late t = manipTime (+ t) (subtract t)
_fast t = manipTime (* t) (/ t)
_slow t = manipTime (/ t) (* t)

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

sigBind :: (Maybe Arc -> Maybe Arc -> Maybe Arc) -> Signal a -> (a -> Signal b) -> Signal b
sigBind chooseWhole pv f = Signal $ \q -> concatMap match $ query pv q
  where match event = map (withWhole event) $ query (f $ value event) (active event)
        withWhole event event' = event' {whole = chooseWhole (whole event) (whole event')}

_zoomArc :: Arc -> Signal a -> Signal a
_zoomArc (Arc s e) p = splitQueries $ withEventArc (mapCycle ((/d) . subtract s)) $ withQuery (mapCycle ((+s) . (*d))) p
     where d = e-s

-- -- | Repeat discrete value once per cycle
-- sigAtom :: a -> Signal a
-- sigAtom v = Signal $ \q -> map (\arc -> Event (Just $ timeToCycle $ aBegin arc) arc v) $ splitArcs q

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

gap :: Time -> Sequence a
gap t = Atom t 0 0 Nothing

step :: Time -> a -> Sequence a
step t v = Atom t 0 0 $ Just v

-- Flatten, using outer duration as relative duration for inner
seqJoin :: Sequence (Sequence a) -> Sequence a
seqJoin (Atom d i o (Just seq)) =  _slow (d + i + o) seq
seqJoin (Atom d i o Nothing)    = Atom d i o Nothing
seqJoin (Cat xs)                = Cat $ map seqJoin xs
seqJoin (Stack xs)              = Stack $ map seqJoin xs

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
  return = step 1
  seqv >>= f = seqJoin $ fmap f seqv

instance Applicative Sequence where
  pure = return
  pf <*> px = pf >>= \f -> px >>= \x -> pure $ f x

instance Pattern Sequence where
  manipTime f _ pat = withAtomTime f pat
  cat = Cat   -- TODO - shallow cat?
  stack = Stack
  duration (Atom d _ _ _) = d
  duration (Cat xs)       = sum $ map duration xs
  duration (Stack [])     = 0
  duration (Stack (x:_))  = duration x
  timeCat seqs = seqJoin $ Cat $ map (uncurry step) seqs
  -- innerBind =
  -- outerBind =
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


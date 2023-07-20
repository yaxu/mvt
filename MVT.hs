{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes    #-}

-- Minimal Viable Tidal

module MVT where

import           Control.Applicative (liftA2)
import           Data.Maybe          (fromMaybe)
-- So we can overload *> and <* as alternatives to <*>
import           Data.Fixed          (mod')
import           Data.List           (intercalate, intersperse)
import           Data.Ratio
import           Data.Tuple          (swap)
import           Debug.Trace         (trace)
import           Prelude             hiding ((*>), (<*))

-- ********
-- | Time *
-- ********
type Time = Rational

-- | The start of the cycle
sam :: Time -> Time
sam s = toRational (floor s :: Int)

-- | The start of the next cycle
nextSam :: Time -> Time
nextSam = (+1) . sam

-- | Lowest common multiple
lcmTime :: Time -> Time -> Time
lcmTime a b = lcm (f a) (f b) % d
  where d = lcm (denominator a) (denominator b)
        f x = numerator x * (d `div` denominator x)

-- ***********************
-- | Span (aka time arc) *
-- ***********************
data Span = Span { aBegin :: Time, aEnd :: Time}
  deriving (Eq, Ord, Show)

-- | Intersection of two arcs
sect :: Span -> Span -> Span
sect (Span b e) (Span b' e') = Span (max b b') (min e e')

-- | Returns the whole cycle arc that the given time is in
timeToCycle :: Time -> Span
timeToCycle t = Span (sam t) (nextSam t)

-- | Splits a timespan at cycle boundaries
splitSpans :: Span -> [Span]
splitSpans (Span s e) | s == e = [Span s e] -- support zero-width arcs
                      | otherwise = splitSpans' (Span s e) -- otherwise, recurse
  where splitSpans' (Span s' e') | e' <= s' = []
                                 | sam s' == sam e' = [Span s' e']
                                 | otherwise = Span s' (nextSam s') : splitSpans' (Span (nextSam s') e')

-- | Similar to 'fmap' but time is relative to the cycle (i.e. the
-- sam of the start of the arc)
mapCycle :: (Time -> Time) -> Span -> Span
mapCycle f (Span s e) = Span (sam' + f (s - sam')) (sam' + f (e - sam'))
         where sam' = sam s

-- *********
-- | Event *
-- *********
-- A discrete value with a whole timespan, or a continuous one without
-- It might be a fragment of an event, in which case its 'active' arc
-- will be a smaller subsection of its 'whole'.
data Event a = Event {whole :: Maybe Span, active :: Span, value :: a}
  deriving (Functor, Eq, Ord)

instance (Show a) => Show (Event a) where
  show e = show (whole e) ++ " " ++ show (active e) ++ " " ++ show (value e)

eventWithSpan :: (Span -> Span) -> Event a -> Event a
eventWithSpan f e = e {active = f $ active e,
                       whole  = f <$> whole e
                      }
-- ***********
-- | Pattern *
-- ***********
-- A type class for patterns

class (Functor p, Applicative p, Monad p) => Pattern p where
  duration :: p a -> Time
  withTime :: (Time -> Time) -> (Time -> Time) -> p a -> p a
  innerBind, outerBind :: p a -> (a -> p b) -> p b
  cat :: [p a] -> p a
  timeCat :: [(Time, p a)] -> p a
  stack :: [p a] -> p a
  _early :: Time -> p a -> p a
  rev :: p a -> p a
  toSignal :: p a -> Signal a

silence :: (Monoid (p a), Pattern p) => p a
silence = mempty

innerJoin, outerJoin :: Pattern p => p (p b) -> p b
innerJoin s = innerBind s id
outerJoin s = outerBind s id

-- Patternification

-- Turns functions with non-patterned parameters into fully patternified ones

alignify :: Alignment x => (a -> b -> Sequence c) -> x a -> b -> Sequence c
alignify f alignSeq seqb = aseq `aBind` \a -> f a seqb
  where aseq = aSequence alignSeq
        strat = aStrategy alignSeq
        -- TODO choose different binds based on strategy.
        aBind = innerBind

-- patternify the first parameter
patternify :: Pattern p => (a -> b -> p c) -> p a -> b -> p c
patternify f apat pat                 = apat `innerBind` \a -> f a pat

-- patternify the first two parameters
patternify_p_p :: (Pattern p) => (a -> b -> c -> p d) -> p a -> p b -> c -> p d
patternify_p_p f apat bpat pat        = apat `innerBind` \a -> (bpat `innerBind` \b -> f a b pat)

-- patternify the first but not the second parameters
patternify_p_n :: Pattern p => (a -> b -> c -> p d) -> p a -> b -> c -> p d
patternify_p_n f apat b pat           = apat `innerBind` \a -> f a b pat

-- patternify the first three parameters
patternify_p_p_p :: Pattern p => (a -> b -> c -> d -> p e) -> p a -> p b -> p c -> d -> p e
patternify_p_p_p f apat bpat cpat pat = apat `innerBind` \a -> (bpat `innerBind` \b -> (cpat `innerBind` \c -> f a b c pat))

-- **********
-- | Signal *
-- **********
-- A pattern that's a function from a timespan to events active during
-- that timespan. A continuous signal, that can nonetheless contain
-- discrete events.
data Signal a = Signal {query :: Span -> [Event a]}
  deriving (Functor)

instance Semigroup (Signal a) where a <> b = cat [a,b]
instance Monoid (Signal a)    where mempty = Signal $ const []
instance Monad Signal         where (>>=) = sigBindWith $ liftA2 sect
                                    return = pure
-- Define applicative from monad
instance Applicative Signal where
  pure v = Signal $ \q -> map (\arc -> Event (Just $ timeToCycle $ aBegin arc) arc v) $ splitSpans q
  pf <*> px = pf >>= \f -> px >>= \x -> pure $ f x

instance Pattern Signal where
  -- We always work with signals as if they have a duration of 1
  -- cycle, even though successive cycles very often differ
  duration _ = 1
  withTime fa fb pat = withEventTime fa $ withQueryTime fb pat
  -- | Alternative binds
  innerBind = sigBindWith $ flip const
  outerBind = sigBindWith const
  -- | Concatenate a list of signals, interleaving cycles.
  cat pats = splitQueries $ trace "hmm" $ Signal $ \a -> query (_late (offset a) (pats !! mod (floor $ aBegin a) n)) (trace (show $ a) a)
    where offset arc = sam (aBegin arc) - sam (aBegin arc / toRational (trace (show n) $ n))
          n = length $ trace (show $ length pats) $ pats
  timeCat tps = stack $ map (\(s,e,p) -> _compressSpan (Span (s/total) (e/total)) p) $ arrange 0 tps
    where total = sum $ map fst tps
          arrange :: Time -> [(Time, Signal a)] -> [(Time, Time, Signal a)]
          arrange _ []            = []
          arrange t ((t',p):tps') = (t,t+t',p) : arrange (t+t') tps'
  stack pats = Signal $ \a -> concatMap (`query` a) pats
  _early t = withTime (subtract t) (+ t)
  rev pat = splitQueries $ Signal f
    where f a = eventWithSpan reflect <$> (query pat $ reflect a)
            where cyc = sam $ aBegin a
                  next_cyc = nextSam cyc
                  reflect (Span b e) = Span (cyc + (next_cyc - e)) (cyc + (next_cyc - b))
  toSignal = id

-- | Split queries at sample boundaries. An internal function that
-- makes other functions easier to define, as events that cross cycle
-- boundaries don't need to be considered then.
splitQueries :: Signal a -> Signal a
splitQueries pat = Signal $ concatMap (query pat) . splitSpans

(<*), (*>) :: Pattern p => p (t -> b) -> p t -> p b
pf <* px = pf `innerBind` \f -> px `innerBind` \x -> pure $ f x
pf *> px = pf `outerBind` \f -> px `outerBind` \x -> pure $ f x
infixl 4 <*, *>

_fast, _slow, _late :: Pattern p => Time -> p a -> p a
_fast t = withTime (/ t) (* t)
_slow t = withTime (* t) (/ t)
_late = _early . (0-)

-- patternify parameters
fast, slow, early, late :: Pattern p => p Time -> p a -> p a
fast  = patternify _fast
slow  = patternify _slow
early = patternify _early
late  = patternify _late

withSpanTime :: (Time -> Time) -> Span -> Span
withSpanTime timef (Span b e) = Span (timef b) (timef e)

-- | @withEvents f p@ returns a new @Signal@ with f applied to the
-- resulting list of events for each query function @f@.
withEvents :: ([Event a] -> [Event b]) -> Signal a -> Signal b
withEvents f p = p {query = f . query p}

-- | @withEvent f p@ returns a new @Signal@ with f applied to each
-- event queried function @f@.
withEvent :: (Event a -> Event b) -> Signal a -> Signal b
withEvent f = withEvents (map f)

withEventTime :: (Time -> Time) -> Signal a -> Signal a
withEventTime timef = withEvent $ \e -> e {active = withSpanTime timef $ active e,
                                           whole = withSpanTime timef <$> whole e
                                          }

withEventSpan :: (Span -> Span) -> Signal a -> Signal a
withEventSpan arcf = withEvent $ \e -> e {active = arcf $ active e,
                                          whole = arcf <$> whole e
                                         }

withQuery :: (Span -> Span) -> Signal a -> Signal a
withQuery arcf sig = Signal $ \arc -> query sig $ arcf arc

withQueryMaybe :: (Span -> Maybe Span) -> Signal a -> Signal a
withQueryMaybe qf pat = Signal $ \q -> fromMaybe [] $ qf q >>= Just . query pat

withQueryTime :: (Time -> Time) -> Signal a -> Signal a
withQueryTime timef = withQuery $ withSpanTime timef

-- Makes a signal bind, given a function of how to calculate the 'whole' timespan
sigBindWith :: (Maybe Span -> Maybe Span -> Maybe Span) -> Signal a -> (a -> Signal b) -> Signal b
sigBindWith chooseWhole pv f = Signal $ \q -> concatMap match $ query pv q
  where match event = map (withWhole event) $ query (f $ value event) (active event)
        withWhole event event' = event' {whole = chooseWhole (whole event) (whole event')}

_zoomSpan :: Span -> Signal a -> Signal a
_zoomSpan (Span s e) p = splitQueries $ withEventSpan (mapCycle ((/d) . subtract s)) $ withQuery (mapCycle ((+s) . (*d))) p
     where d = e-s

-- TODO - why is this function so long?
_fastGap :: Time -> Signal a -> Signal a
_fastGap factor pat = splitQueries $ withEvent ef $ withQueryMaybe qf pat
  -- A bit fiddly, to drop zero-width queries at the start of the next cycle
  where qf (Span b e) | bpos < 1 = Just $ Span (cyc + bpos) (cyc + epos)
                      | otherwise = Nothing
          where cyc = sam b
                bpos = min 1 $ (b - cyc) * factor
                epos = min 1 $ (e - cyc) * factor
        -- Also fiddly, to maintain the right 'whole' relative to the part
        ef ev = ev {whole = w', active = a'}
          where a = active ev
                b = aBegin a
                e = aEnd a
                a' = Span (cyc + bpos) (cyc + epos)
                  where cyc = sam b
                        bpos = min 1 $ (b - cyc) / factor
                        epos = min 1 $ (e - cyc) / factor
                w' = do w <- whole ev
                        let b' = aBegin a' - ((b - aBegin w) / factor)
                            e' = aEnd a' + ((aEnd w - e) / factor)
                        return $ Span b' e'

_compressSpan :: Span -> Signal a -> Signal a
_compressSpan (Span b e) pat | b > e || b > 1 || e > 1 || b < 0 || e < 0 = silence
                           | otherwise = _late b $ _fastGap (1/(e-b)) pat

-- | Similar to @fastCat@, but each signal is given a relative duration
sigTimeCat :: [(Time, Signal a)] -> Signal a
sigTimeCat tps = stack $ map (\(s,e,p) -> _compressSpan (Span (s/total) (e/total)) p) $ arrange 0 tps
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

-- | Removes duplication, zero-width steps etc.
-- TODO - do we really want to use this internally? E.g. a stack of
-- stacks might represent structure rather than being redundant.
normalise :: Sequence a -> Sequence a
normalise (Cat [x]) = normalise x
normalise (Cat xs) = listToCat $ loop $ map normalise xs
  where listToCat [x] = x
        listToCat xs' = Cat xs'
        loop []                   = []
        loop (Atom 0 _ _ _:xs') = loop xs'
        loop (Atom t _ _ Nothing:Atom t' _ _ Nothing:xs') = loop $ gap (t + t'):xs'
        loop (Cat xs':xs'')       = loop $ xs' ++ xs''
        loop (x:xs')              = normalise x:loop xs'
normalise (Stack [x]) = normalise x
normalise (Stack xs) = listToStack $ loop xs
  where listToStack [x] = x
        listToStack xs' = Stack xs'
        loop (Stack xs':xs'') = loop $ xs' ++ xs''
        loop (x:xs')          = normalise x:loop xs'
        loop []               = []
normalise x = x

seqJoinWith :: (Time -> Sequence a -> Sequence a) -> Sequence (Sequence a) -> Sequence a
seqJoinWith f (Atom d i o (Just seq)) = f (d + i + o) seq
seqJoinWith _ (Atom d i o Nothing)    = Atom d i o Nothing
seqJoinWith f (Cat xs)                = Cat $ map (seqJoinWith f) xs
seqJoinWith f (Stack xs)              = Stack $ map (seqJoinWith f) xs

seqJoinWithSpan :: (Span -> Time -> Sequence a -> Sequence a) -> Sequence (Sequence a) -> Sequence a
seqJoinWithSpan f pat = loop 0 pat
  where patd = duration pat
        -- Pass d rather than d + i + o ?
        loop pos (Atom d i o (Just seq)) = f (Span (pos/patd) ((pos + d)/patd)) d seq
        loop pos (Atom d i o Nothing)    = Atom d i o Nothing
        loop pos (Cat xs)                = Cat $ loop' pos xs
          where loop' pos []     = []
                loop' pos (x:xs) = (loop pos x):(loop' (pos + duration x) xs)
        loop pos (Stack xs)              = Stack $ map (loop pos) xs

-- Flatten, using outer duration as relative duration for inner
seqJoin :: Sequence (Sequence a) -> Sequence a
seqJoin = seqJoinWith _slow

-- Flatten, expanding inner to outer duration
seqExpandJoin :: Sequence (Sequence a) -> Sequence a
seqExpandJoin = seqJoinWith (\t seq -> _fast (duration seq / t) seq)

-- Flatten, repeating inner to total duration of outer
seqLoopJoin :: Sequence (Sequence a) -> Sequence a
seqLoopJoin = seqJoinWith seqTakeLoop

-- Flatten, changing duration of outer to fit inner
seqInnerJoin :: Sequence (Sequence a) -> Sequence a
seqInnerJoin seq = seqJoinWithSpan f seq
  where f (Span b e) d seq = seqTakeLoop ((e-b)*d') $ seqDrop (b*d') seq
          where d' = duration seq

-- Flatten, changing duration of inner to fit outer
seqOuterJoin :: Sequence (Sequence a) -> Sequence a
seqOuterJoin seq = _fast (duration inner / duration seq) inner
  where inner = seqInnerJoin seq

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

seqDrop :: Time -> Sequence a -> Sequence a
seqDrop 0 s = s
-- The mod makes this 'safe' but is probably a bad idea..
seqDrop t s | t > duration s = seqDrop' (t `mod'` duration s) s
            | otherwise = seqDrop' t s
  where seqDrop' :: Time -> Sequence a -> Sequence a
        seqDrop' t (Atom d i o v) | t == d = gap 0
                                  | otherwise = Atom (d - t) (i + t) o v
        seqDrop' t (Stack ss) = Stack $ map (seqDrop' t) ss
        seqDrop' t (Cat ss) = Cat $ loop t ss
          where loop :: Time -> [Sequence a] -> [Sequence a]
                loop _ []  = []
                loop t' (s:ss') | t' <= 0 = []
                                | t' == stepDur = ss'
                                | t' <= stepDur = seqDrop' t' s : ss'
                                | otherwise = loop (t' - stepDur) ss'
                  where stepDur = duration s

seqSplitAt :: Time -> Sequence a -> (Sequence a, Sequence a)
seqSplitAt t s = (seqTakeLoop t s, seqDrop t s)

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
  stack = expands
  -- duration of 'part', not whole
  duration (Atom d _ _ _) = d
  duration (Cat xs)       = sum $ map duration xs
  duration (Stack [])     = 0
  duration (Stack (x:_))  = duration x
  timeCat seqs = seqJoin $ Cat $ map (uncurry step) seqs
  seqv `outerBind` f = seqOuterJoin $ fmap f seqv
  seqv `innerBind` f = seqInnerJoin $ fmap f seqv
  _early t = (\(a, b) -> cat [a,b]) . seqSplitAt t
  rev (Stack xs) = Stack $ map rev xs
  rev (Cat xs)   = withAtom swapio $ Cat $ reverse $ map rev xs
    where swapio (Atom d i o v) = Atom d o i v
          swapio x              = x -- shouldn't happen
  rev x          = x
  -- One beat per cycle..
  toSignal pat = _slow (duration pat) $ toSignal' pat
    where
      -- One sequence per cycle
      toSignal' (Atom d i o (Just v)) | d == 0 = error "whoops"
                                      | otherwise = _zoomSpan (Span (i/t) (1-(o/t))) $ pure v
        where t = d + i + o
      toSignal' (Atom _ _ _ Nothing) = silence
      toSignal' (Cat xs) = timeCat timeseqs
        where timeseqs = map (\x -> (duration x, toSignal' x)) xs
      toSignal' (Stack xs) = stack $ map toSignal' xs

  -- **********************
-- | Sequence alignment *
-- **********************

-- | Strategies for aligning two sequences or patterns over time (horizontally)
data Strategy = JustifyLeft
              | JustifyRight
              | JustifyBoth
              | Expand
              | TruncateLeft
              | TruncateRight
              | TruncateRepeat
              | Repeat
              | Centre
              | SqueezeIn
              | SqueezeOut
              deriving (Eq, Ord, Show)

class Alignment a where
  aSequence :: a b -> Sequence b
  aStrategy :: a b -> Strategy

instance Alignment Sequence where
  aSequence a = a
  aStrategy _ = Expand -- default strategy

data SeqStrategy a = SeqStrategy {sStrategy :: Strategy,
                                  sSequence :: Sequence a
                                 }

instance Alignment SeqStrategy where
  aSequence = sSequence
  aStrategy = sStrategy

seqPadBy :: ([Sequence a] -> Sequence a -> [Sequence a]) -> Time -> Sequence a -> Sequence a
seqPadBy by t x = f x
  where f (Cat xs) | t < 0 = error "Can't do negative pad"
                   | t == 0 = x
                   | otherwise = Cat $ by xs $ gap t
        -- wrap in Cat for padding
        f x' = seqPadBy by t $ Cat [x']

seqPadRightBy :: Time -> Sequence a -> Sequence a
seqPadRightBy = seqPadBy $ \xs x -> xs ++ [x]

seqPadLeftBy :: Time -> Sequence a -> Sequence a
seqPadLeftBy = seqPadBy $ flip (:)

seqPadBothBy :: Time -> Sequence a -> Sequence a
seqPadBothBy = seqPadBy (\xs x -> (x:xs) ++ [x])

seqSpaceOutBy :: Time -> Sequence a -> Sequence a
seqSpaceOutBy t (Cat ss) | t < 0 = error "Can't do negative pad"
                         | t == 0 = Cat ss
                         | otherwise = Cat $ intersperse g ss
  where g = gap (t / toRational (length ss - 1))
seqSpaceOutBy t s = seqSpaceOutBy t $ Cat [s]

seqRepeatTo :: Time -> Sequence a -> Sequence a
seqRepeatTo t (Cat ss) = seqTakeLoop t $ Cat $ cycle ss
seqRepeatTo t s        = seqRepeatTo t $ Cat [s]

-- requires RankNTypes?
withSmallest :: (forall x. Sequence x -> Sequence x) -> Sequence a -> Sequence b -> (Sequence a, Sequence b)
withSmallest f a b | o == LT = (f a, b)
                   | o == GT = (a, f b)
                   | otherwise = (a, b)
  where o = compare (duration a) (duration b)

withLargest :: (forall x. Sequence x -> Sequence x) -> Sequence a -> Sequence b -> (Sequence a, Sequence b)
withLargest f a b | o == LT = (a, f b)
                  | o == GT = (f a, b)
                  | otherwise = (a, b)
  where o = compare (duration a) (duration b)

align :: Strategy -> Sequence a -> Sequence b -> (Sequence a, Sequence b)
align Repeat a b = (rep a, rep b)
  where d = lcmTime (duration a) (duration b)
        rep x = seqReplicate (floor $ d / duration x) x
        seqReplicate :: Int -> Sequence a -> Sequence a
        seqReplicate n (Cat xs) = Cat $ concat $ replicate n xs
        seqReplicate n x        = Cat $ replicate n x

align JustifyLeft a b = withSmallest (seqPadRightBy by) a b
  where by = abs $ duration a - duration b

align JustifyRight a b = withSmallest (seqPadLeftBy by) a b
  where by = abs $ duration a - duration b

align JustifyBoth a b = withSmallest (seqSpaceOutBy by) a b
  where by = abs $ duration a - duration b

align Centre a b = withSmallest (seqPadBothBy by) a b
  where by = abs $ (duration a - duration b) / 2

align Expand a b = withSmallest (_fast by) a b
  where ratio = duration a / duration b
        by | ratio < 1 = ratio
           | otherwise = 1/ratio

align TruncateLeft a b = withLargest (seqTakeLoop $ min (duration a) (duration b)) a b

-- align TruncateRight a b = withLargest (seqDrop' $ abs $ duration a - duration b) a b

align TruncateRepeat a b = withSmallest (seqRepeatTo to) a b
  where to = max (duration a) (duration b)

align SqueezeIn (Cat xs) b = (Cat xs, squeezed)
  where squeezed = Cat $ map (\x -> squash (duration x) b) xs
        squash t x = _fast (duration x / t) x
align SqueezeIn x b = align SqueezeIn (Cat [x]) b

align SqueezeOut a b = swap $ align SqueezeIn b a

align strategy _ _ = error $ show strategy ++ " not implemented for sequences."

-- **********************
-- | Sequence combining *
-- **********************

-- | Once we've aligned two patterns, where does the structure come from?
data Direction = Inner
               | Outer
               | Mix
               deriving (Eq, Ord, Show)

pairAligned :: Direction -> (Sequence a, Sequence b) -> Sequence (a, b)
-- TODO - vertical alignments
-- TODO - 'Mixed' direction
pairAligned Mix _ = error "TODO !!"

pairAligned direction (Stack as, b) = Stack $ map (\a -> pairAligned direction (a, b)) as
pairAligned direction (a, Stack bs) = Stack $ map (\b -> pairAligned direction (a, b)) bs

-- TODO - should the value be Nothing if one/both are nothings?
pairAligned Inner (Atom d i o v, Atom _ _ _ v')
  = Atom d i o $ do a <- v
                    b <- v'
                    return (a,b)

pairAligned Inner (Cat xs, Cat ys) = Cat $ loop xs ys
  where loop :: [Sequence a] -> [Sequence b] -> [Sequence (a, b)]
        loop [] _ = []
        loop _ [] = []
        loop (a:as) (b:bs) = case cmp of
                               LT -> pairAligned Inner (a, b')
                                     : loop as (b'':bs)
                               GT -> pairAligned Inner (a', b)
                                     : loop (a'':as) bs
                               EQ -> pairAligned Inner (a, b)
                                     : loop as bs
          where adur = duration a
                bdur = duration b
                cmp = compare adur bdur
                (a', a'') = seqSplitAt bdur a
                (b', b'') = seqSplitAt adur b

pairAligned Inner (Cat xs, y) = loop 0 xs y
  where loop _ [] _     = gap 0
        loop _ [a] b    = pairAligned Inner (a, b)
        loop t (a:as) b = cat [pairAligned Inner (a, b'), loop t' as b'']
          where t' = t + duration a
                (b', b'') = seqSplitAt t' b

pairAligned Inner (x, Cat ys) = loop 0 x ys
  where loop :: Time -> Sequence a -> [Sequence b] -> Sequence (a,b)
        loop _ _ []     = gap 0
        loop _ a [b]    = pairAligned Inner (a, b)
        loop t a (b:bs) = cat [pairAligned Inner (a', b), loop t' a'' bs]
          where t' = t + duration b
                (a', a'') = seqSplitAt t' a

pairAligned Outer (a, b) = swap <$> pairAligned Inner (b, a)

pairAlign :: Strategy -> Direction -> Sequence a -> Sequence b -> Sequence (a, b)
pairAlign s d a b = pairAligned d $ align s a b

alignF :: Strategy -> Direction -> (a -> b -> c) -> Sequence a -> Sequence b -> Sequence c
alignF s d f a b = uncurry f <$> pairAlign s d a b

-- | Stacks

alignStack :: Strategy -> [Sequence a] -> Sequence a
alignStack strat xs = normalise $ loop xs
  where loop []      = silence
        loop [x]     = x
        loop (x:xs') = Stack [a,b]
          where (a, b) = align strat x $ alignStack strat xs'

polys :: [Sequence a] -> Sequence a
polys = alignStack Repeat

centres :: [Sequence a] -> Sequence a
centres = alignStack Centre

lefts :: [Sequence a] -> Sequence a
lefts = alignStack JustifyLeft

rights :: [Sequence a] -> Sequence a
rights = alignStack JustifyRight

truncs :: [Sequence a] -> Sequence a
truncs = alignStack TruncateRepeat

expands :: [Sequence a] -> Sequence a
expands = alignStack Expand

{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}

module MVT where

import           Control.Applicative

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
-- A class of patterns

class (Functor p, Applicative p, Monad p) => Pattern p where
  atom :: a -> p a
  manipTime :: (Time -> Time) -> (Time -> Time) -> p a -> p a
  innerBind, outerBind :: p a -> (a -> p b) -> p b
  cat :: [p a] -> p a
  stack :: [p a] -> p a
  toSignal :: p a -> Signal a

-- **********
-- | Signal *
-- **********
-- A pattern that's a function from a timespan to events active during
-- that timespan
data Signal a = Signal {query :: Arc -> [Event a]}
  deriving (Functor)

instance Pattern Signal where
  atom    = sigAtom
  manipTime fa fb pat = withEventTime fa $ withQueryTime fb pat
  -- | Alternative binds
  innerBind = sigBind $ flip const
  outerBind = sigBind const
  -- | Concatenate a list of signals, interleaving cycles.
  cat pats = _splitQueries $ Signal $ \a -> query (_late (offset a) (pats !! mod (floor $ aBegin a) n)) a
    where offset arc = sam (aBegin arc) - sam (aBegin arc / toRational n)
          n = length pats
  stack pats = Signal $ \a -> concatMap (`query` a) pats
  toSignal = id

-- | Split queries at sample boundaries. An internal function that
-- makes other functions easier to define, as events that cross cycle
-- boundaries don't need to be considered then.
_splitQueries :: Signal a -> Signal a
_splitQueries pat = Signal $ concatMap (query pat) . splitArcs

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

withEventTime :: (Time -> Time) -> Signal a -> Signal a
withEventTime timef sig = Signal $ map (\e -> e {active = withArcTime timef $ active e,
                                                 whole = withArcTime timef <$> whole e
                                                }) . query sig

withQuery :: (Arc -> Arc) -> Signal a -> Signal a
withQuery arcf sig = Signal $ \arc -> query sig $ arcf arc

withQueryArc :: (Arc -> Arc) -> Signal a -> Signal a
withQueryArc arcf = withQuery arcf

withQueryTime :: (Time -> Time) -> Signal a -> Signal a
withQueryTime timef = withQueryArc (withArcTime timef)

instance Applicative Signal where
  pure = atom
  pf <*> px = pf >>= \f -> px >>= \x -> pure $ f x

instance Monad Signal where
  (>>=) = sigBind $ liftA2 sect

sigBind :: (Maybe Arc -> Maybe Arc -> Maybe Arc) -> Signal a -> (a -> Signal b) -> Signal b
sigBind chooseWhole pv f = Signal $ \q -> concatMap match $ query pv q
  where match event = map (withWhole event) $ query (f $ value event) (active event)
        withWhole event event' = event' {whole = chooseWhole (whole event) (whole event')}

-- | Repeat discrete value once per cycle
sigAtom :: a -> Signal a
sigAtom v = Signal $ \q -> map (\arc -> Event (Just $ timeToCycle $ aBegin arc) arc v) $ splitArcs q

-- ************
-- | Sequence *
-- ************

data Sequence a = Atom {atomDuration :: Time,
                        atomInset :: Time, atomOutset :: Time,
                        atomValue :: Maybe a
                       }
                | Cat [Sequence a]
                | Stack [Sequence a]
                deriving (Eq, Ord)

{-# LANGUAGE DeriveFunctor #-}

module Sound.Tidal.Types where

type Time = Rational

-- | A type class for patterns
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

data Span = Span { aBegin :: Time, aEnd :: Time}
  deriving (Eq, Ord, Show)

-- A discrete value with a whole timespan, or a continuous one without
-- It might be a fragment of an event, in which case its 'active' arc
-- will be a smaller subsection of its 'whole'.
data Event a = Event {whole :: Maybe Span, active :: Span, value :: a}
  deriving (Functor, Eq, Ord)

-- A pattern that's a function from a timespan to events active during
-- that timespan. A continuous signal, that can nonetheless contain
-- discrete events.
data Signal a = Signal {query :: Span -> [Event a]}
  deriving (Functor)

-- A pattern as a discrete, contiguous, finite sequence, that's
-- structured to support polyphonic stacks, and embedded subsequences
data Sequence a = Atom {atomDuration :: Time,
                        atomInset    :: Time,
                        atomOutset   :: Time,
                        atomValue    :: Maybe a
                       }
                | Cat [Sequence a]
                | Stack [Sequence a]
                deriving (Eq, Ord)

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
  toSeqStrategy :: a b -> SeqStrategy b


data SeqStrategy a = SeqStrategy {sStrategy  :: Strategy,
                                  sDirection :: Direction,
                                  sSequence  :: Sequence a
                                 }

-- | Once we've aligned two patterns, where does the structure come from?
data Direction = Inner
               | Outer
               | Mix
               deriving (Eq, Ord, Show)

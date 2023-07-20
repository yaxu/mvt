module Sound.Tidal.Pattern where

import           Prelude           hiding ((*>), (<*))
import           Sound.Tidal.Types

silence :: (Monoid (p a), Pattern p) => p a
silence = mempty

innerJoin, outerJoin :: Pattern p => p (p b) -> p b
innerJoin s = innerBind s id
outerJoin s = outerBind s id

-- Patternification

-- Turns functions with non-patterned parameters into fully patternified ones

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

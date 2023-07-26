module Sound.Tidal.Metadata where

import Sound.Tidal.Types

instance Semigroup Metadata where
  (<>) a b = Metadata (metaSrcPos a ++ metaSrcPos b) (metaStrategy a)

instance Monoid Metadata where
  mempty = Metadata [] Nothing

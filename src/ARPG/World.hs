module ARPG.World where

import SDL.Vect
import Apecs
import Apecs.TH
import Data.Int
import Data.Semigroup

newtype MousePosition = MousePosition (Option (Last (Point V2 Int32)))
  deriving (Show, Read, Eq, Ord, Semigroup, Monoid)

instance Component MousePosition where
  type Storage MousePosition = Global MousePosition

makeWorld "World" [''MousePosition]
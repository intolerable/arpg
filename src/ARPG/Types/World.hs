module ARPG.Types.World where

import SDL.Vect
import Apecs
import Apecs.TH
import Data.Int
import Data.Semigroup

newtype WindowSize = WindowSize (Maybe (V2 Int32))
  deriving (Show, Eq, Ord)

instance Semigroup WindowSize where
  WindowSize _ <> WindowSize (Just y) = WindowSize (Just y)
  WindowSize (Just x) <> WindowSize _ = WindowSize (Just x)
  WindowSize Nothing <> WindowSize Nothing = WindowSize Nothing

instance Monoid WindowSize where
  mappend = (<>)
  mempty = WindowSize Nothing

instance Component WindowSize where
  type Storage WindowSize = Global WindowSize

newtype MousePosition = MousePosition (Maybe (Point V2 Int32))
  deriving (Show, Eq, Ord)

instance Semigroup MousePosition where
  MousePosition _ <> MousePosition (Just y) = MousePosition (Just y)
  MousePosition (Just x) <> MousePosition _ = MousePosition (Just x)
  MousePosition Nothing <> MousePosition Nothing = MousePosition Nothing

instance Monoid MousePosition where
  mappend = (<>)
  mempty = MousePosition Nothing

instance Component MousePosition where
  type Storage MousePosition = Global MousePosition

newtype Ability = Ability Int
  deriving (Show, Read, Eq, Ord, Num)

newtype ActiveAbility = ActiveAbility [Ability]
  deriving (Show, Read, Eq, Ord, Semigroup, Monoid)

instance Component ActiveAbility where
  type Storage ActiveAbility = Global ActiveAbility

newtype UnitPosition = UnitPosition (Point V2 Int32)
  deriving (Show, Eq, Ord)

instance Component UnitPosition where
  type Storage UnitPosition = Map UnitPosition

data Enemy = Enemy
  deriving (Show, Read, Eq, Ord)

instance Flag Enemy where flag = Enemy
instance Component Enemy where
  type Storage Enemy = Set Enemy

makeWorld "World" [''WindowSize, ''MousePosition, ''ActiveAbility, ''UnitPosition, ''Enemy]

initializeWorld :: IO World
initializeWorld =
  World <$> initStore
        <*> initStore
        <*> initStore
        <*> initStore
        <*> initStore
        <*> initStore

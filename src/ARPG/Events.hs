module ARPG.Events where

import Apecs hiding (Map)
import Data.Map (Map)
import Data.Semigroup
import SDL hiding (quit)
import qualified Data.List as List
import qualified Data.Map as Map

import ARPG.Types.World

data EventResult =
  EventResult
    { shouldQuit :: Any }
  deriving (Show, Eq, Ord)

instance Semigroup EventResult where
  EventResult q <> EventResult r = EventResult (q <> r)

instance Monoid EventResult where
  mappend = (<>)
  mempty = EventResult mempty

type EventHandler a = a -> System World EventResult

quit :: EventResult
quit = EventResult (Any True)

noAction :: EventResult
noAction = mempty

handleEvent :: EventHandler EventPayload
handleEvent = \case
  MouseMotionEvent ev -> handleMouseMoveEvent ev
  MouseButtonEvent ev -> handleMouseButtonEvent ev
  KeyboardEvent ev -> handleKeyboardEvent ev
  WindowResizedEvent ev -> handleWindowResizedEvent ev
  WindowClosedEvent _ -> return quit
  _ -> return noAction

handleMouseMoveEvent :: EventHandler MouseMotionEventData
handleMouseMoveEvent (MouseMotionEventData _ _ _ pos _) = do
  writeGlobal $ MousePosition $ Just pos
  return noAction

handleMouseButtonEvent :: EventHandler MouseButtonEventData
handleMouseButtonEvent ev = do
  case ev of
    MouseButtonEventData _ Pressed _ button _ _ -> do
      case Map.lookup (Right button) controlsMap of
        Just x -> modifyGlobal $ \(ActiveAbility xs) ->
          ActiveAbility (x:xs)
        Nothing -> return ()
    MouseButtonEventData _ Released _ button _ _ -> do
      case Map.lookup (Right button) controlsMap of
        Just x -> modifyGlobal $ \(ActiveAbility xs) ->
          ActiveAbility $ List.delete x xs
        Nothing -> return ()
  return noAction

handleKeyboardEvent :: EventHandler KeyboardEventData
handleKeyboardEvent = \case
  KeyboardEventData _ Pressed False key -> do
    case Map.lookup (Left $ keysymKeycode key) controlsMap of
      Just x -> modifyGlobal $
        \(ActiveAbility xs) -> ActiveAbility (x:xs)
      Nothing -> return ()
    case key of
      Keysym _ KeycodeEscape _ -> return quit
      _ -> return noAction
  KeyboardEventData _ Released False key -> do
    case Map.lookup (Left $ keysymKeycode key) controlsMap of
      Just x ->
        modifyGlobal $
          \(ActiveAbility xs) -> ActiveAbility $ List.delete x xs
      Nothing -> return ()
    return noAction
  KeyboardEventData _ _ True _ -> return noAction

controlsMap :: Map (Either Keycode MouseButton) Ability
controlsMap = Map.fromList
  [ Right ButtonLeft =. 0
  , Right ButtonMiddle =. 1
  , Right ButtonRight =. 2
  , Left KeycodeQ =. 3
  , Left KeycodeW =. 4
  , Left KeycodeE =. 5
  , Left KeycodeR =. 6 ]
  where (=.) = (,)

controlsLabelMap :: Map (Either Keycode MouseButton) String
controlsLabelMap = Map.fromList
  [ Right ButtonLeft =. "Left Mouse Button"
  , Right ButtonMiddle =. "Middle Mouse Button"
  , Right ButtonRight =. "Right Mouse Button"
  , Left KeycodeQ =. "Q"
  , Left KeycodeW =. "W"
  , Left KeycodeE =. "E"
  , Left KeycodeR =. "R" ]
  where (=.) = (,)

handleAbilityPress :: Keysym -> System World ()
handleAbilityPress = undefined

handleWindowResizedEvent :: EventHandler WindowResizedEventData
handleWindowResizedEvent (WindowResizedEventData _ newSize) = do
  writeGlobal $ WindowSize $ Just newSize
  return noAction

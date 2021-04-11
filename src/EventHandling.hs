module EventHandling(updateProgramState) where

import Data.Foldable (foldl')
import State
import qualified SDL -- Importiert das ganze SDL-Packet, mit dem SDL-Prefix
import SDL.Vect -- V2

-- | Updates the program state.
--
-- The incoming SDL events are first mapped to 'Intents' and then applied using
-- applyIntent
updateProgramState :: ProgramState -> [SDL.Event] -> ProgramState
updateProgramState ps
  = foldl' (flip applyIntent) ps
  . fmap (payloadToIntent . SDL.eventPayload)

-- | Converts a SDL.EventPayload to an Intent
payloadToIntent :: SDL.EventPayload -> Intent
payloadToIntent (SDL.KeyboardEvent k) = keyEventToIntent k
payloadToIntent (SDL.MouseWheelEvent m) = mouseWheelEventToIntent m
payloadToIntent SDL.QuitEvent         = Quit
payloadToIntent _                     = Idle

-- | How much to move per frame
moved :: Double
moved = 0.01

-- | Converts a SDL Keyboard event to an Intent
keyEventToIntent :: SDL.KeyboardEventData -> Intent
keyEventToIntent (SDL.KeyboardEventData _ SDL.Pressed _ keysym) =
  case SDL.keysymKeycode keysym of
    SDL.KeycodeEscape -> Quit
    SDL.KeycodeA -> Move (negate moved) 0.0
    SDL.KeycodeD -> Move moved 0.0
    SDL.KeycodeW -> Move 0.0 moved
    SDL.KeycodeS -> Move 0.0 (negate moved)
    SDL.KeycodeI -> IncreaseZoom
    SDL.KeycodeK -> DecreaseZoom
    SDL.KeycodeO -> IncreaseIterations
    SDL.KeycodeL -> DecreaseIterations
    SDL.KeycodeF1 -> SwitchFractal 0
    SDL.KeycodeF2 -> SwitchFractal 1
    SDL.KeycodeF3 -> SwitchFractal 2
    SDL.KeycodeF4 -> SwitchFractal 3
    SDL.KeycodeF5 -> SwitchFractal 4
    SDL.KeycodeF6 -> SwitchFractal 5
    SDL.Keycode1 -> SwitchFractal 10
    SDL.Keycode2 -> SwitchFractal 11
    SDL.Keycode3 -> SwitchFractal 12
    _ -> Idle
keyEventToIntent _ = Idle

-- | Converts a SDL mousewheel event to an Intent
mouseWheelEventToIntent :: SDL.MouseWheelEventData -> Intent
mouseWheelEventToIntent (SDL.MouseWheelEventData _ _ (V2 x y) _) =
  if y > 0 then IncreaseZoom else DecreaseZoom

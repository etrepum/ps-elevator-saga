module Main where

import Debug.Trace
import Data.Array
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Data.Foreign.OOFFI

foreign import data Elevator :: *
foreign import data El :: !
foreign import data Floor :: *
foreign import data Fl :: !

type FloorNum = Number

goToFloor :: forall e. Elevator -> FloorNum -> Eff (elEff :: El | e) Unit
goToFloor = method1Eff "goToFloor"

stop :: forall e. Elevator -> Eff (elEff :: El | e) Unit
stop = method0Eff "stop"

currentFloor :: forall e. Elevator -> Eff (elEff :: El | e) FloorNum
currentFloor = method0Eff "currentFloor"

getGoingUpIndicator :: forall e. Elevator -> Eff (elEff :: El | e) Boolean
getGoingUpIndicator = method0Eff "goingUpIndicator"

getGoingDownIndicator :: forall e. Elevator -> Eff (elEff :: El | e) Boolean
getGoingDownIndicator = method0Eff "goingDownIndicator"

setGoingUpIndicator :: forall e. Elevator -> Boolean -> Eff (elEff :: El | e) Unit
setGoingUpIndicator = method1Eff "goingUpIndicator"

setGoingDownIndicator :: forall e. Elevator -> Boolean -> Eff (elEff :: El | e) Unit
setGoingDownIndicator = method1Eff "goingDownIndicator"

elevatorOn :: forall e f. Elevator -> String -> f -> Eff (elEff :: El | e) Unit
elevatorOn = method2Eff "on"

floorNum :: Floor -> FloorNum
floorNum = method0 "floorNum"

floorOn :: forall e f. Floor -> String -> f -> Eff (flEff :: Fl | e) Unit
floorOn = method2Eff "on"

type Elevators = Array Elevator
type Floors = Array Floor

data State = State

forI :: forall m a b. (Monad m) => Array a -> (Number -> a -> m b) -> m Unit
forI arr f = void (foldM (\i x -> const (i + 1) <$> f i x) 0 arr)

initFn :: forall e. RefVal State -> Elevators -> Floors -> Eff (trace :: Trace, elEff :: El, flEff :: Fl, ref :: Ref | e) Unit
initFn s elevators floors = forI elevators \i e ->
  elevatorOn e "idle" $ forI floors \i f ->
    goToFloor e (floorNum f)

updateFn :: forall e. RefVal State -> Number -> Elevators -> Floors -> Eff (elEff :: El, flEff :: Fl, ref :: Ref | e) Unit
updateFn s dt elevators floors = pure unit

foreign import data Cb2 :: * -> * -> * -> *
foreign import mkCb2
  "function mkCb2(f) { return function(a, b) { return f(a)(b)(); }\
  \}" :: forall f a b c eff. (a -> b -> Eff eff c) -> Cb2 a b c

foreign import data Cb3 :: * -> * -> * -> * -> *
foreign import mkCb3
  "function mkCb3(f) { return function(a, b) { return f(a)(b)(); }\
  \}" :: forall f a b c d eff. (a -> b -> c -> Eff eff d) -> Cb3 a b c d


main = do
  s <- newRef State
  return {init: mkCb2 (initFn s), update: mkCb3 (updateFn s)}

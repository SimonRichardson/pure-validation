module Main where

import Data.Maybe
import Data.Array (length)
import Data.Either
import Data.Foreign
import Data.Foreign.Null
import Data.Foreign.Class
import Data.JSON
import Data.Traversable
import Data.AddressBook
import Data.AddressBook.UI

import Control.Monad.Eff
import Control.Monad.Eff.DOM (DOM())
import Control.Monad.Eff.Alert
import Control.Monad.Eff.Storage

import Debug.Trace

newtype FormData = FormData
  { firstName   :: String
  , lastName    :: String
  
  , street      :: String
  , city        :: String
  , state       :: String
  
  , homePhone   :: String
  , cellPhone   :: String
  }

instance formDataIsForeign :: IsForeign FormData where
  read value = do
    firstName   <- readProp "firstName" value
    lastName    <- readProp "lastName"  value

    street      <- readProp "street"    value
    city        <- readProp "city"      value
    state       <- readProp "state"     value

    homePhone   <- readProp "homePhone" value
    cellPhone   <- readProp "cellPhone" value

    return $ FormData
      { firstName  : firstName
      , lastName   : lastName

      , street     : street
      , city       : city
      , state      : state

      , homePhone  : homePhone
      , cellPhone  : cellPhone
      } 

main :: forall eff. Eff (trace :: Trace, dom :: DOM | eff) Unit
main = do
  trace "Attaching event handlers"
  setupEventHandlers


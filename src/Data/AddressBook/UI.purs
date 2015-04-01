module Data.AddressBook.UI where

import Data.Maybe
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.AddressBook
import Data.AddressBook.Validation

import Data.Traversable

import Control.Bind

import Control.Monad.Eff
import Control.Monad.Eff.DOM

import Debug.Trace

valueOf :: forall eff. String -> Eff (dom :: DOM | eff) String
valueOf sel = do
  maybeEl <- querySelector sel
  case maybeEl of
    Nothing -> return ""
    Just el -> do
      value <- getValue el
      return $ case read value of
        Right s -> s
        _ -> ""

displayValidationErrors :: forall eff. [ValidationError] -> Eff (dom :: DOM | eff) Unit
displayValidationErrors errs = do
  foreachE errs $ \err -> do
    Just parent <- querySelector (getFormId err)
    container <- parentNode parent

    div <- createElement "div"
    x <- (getName err) `setText` div
    y <- "alert" `addClass` x
    z <- "alert-danger" `addClass` y
    z `appendChild` container

    return unit

  return unit
  where
    getName :: ValidationError -> String
    getName (ValidationError a _) = a

    getFormId :: ValidationError -> String
    getFormId (ValidationError _ a) = fieldId a

    fieldId :: Field -> String
    fieldId FirstNameField = "#inputFirstName"
    fieldId LastNameField  = "#inputLastName"
    fieldId StreetField    = "#inputStreet"
    fieldId CityField      = "#inputCity"
    fieldId StateField     = "#inputState"
    fieldId (PhoneField a) = phoneFieldId a

    phoneFieldId :: PhoneType -> String
    phoneFieldId HomePhone = "#inputHomePhone"
    phoneFieldId CellPhone = "#inputCellPhone"
    phoneFieldId WorkPhone = "#inputWorkPhone"

validateControls :: forall eff. Eff (trace :: Trace, dom :: DOM | eff) (Either [ValidationError] Person)
validateControls = do
  trace "Running Validators"

  p <- person <$> valueOf "#inputFirstName"
              <*> valueOf "#inputLastName"
              <*> sequence (Just (address <$> valueOf "#inputStreet"
                                 <*> valueOf "#inputCity"
                                 <*> valueOf "#inputState"))
              <*> sequence [ phoneNumber HomePhone <$> valueOf "#inputHomePhone"
                           , phoneNumber CellPhone <$> valueOf "#inputCellPhone"
                           , phoneNumber WorkPhone <$> valueOf "#inputWorkPhone"
                           ]

  return $ validatePerson' p

validateAndUpdateUI :: forall eff. Eff (trace :: Trace, dom :: DOM | eff) Unit
validateAndUpdateUI = do
  Just validationErrors <- querySelector "#validationErrors"
  setInnerHTML "" validationErrors

  errorsOrResult <- validateControls

  case errorsOrResult of
    Left errs -> displayValidationErrors errs
    Right result -> print result

  return unit

setupEventHandlers :: forall eff. Eff (trace :: Trace, dom :: DOM | eff) Unit
setupEventHandlers = do
  body >>= addEventListener "change" validateAndUpdateUI

  return unit


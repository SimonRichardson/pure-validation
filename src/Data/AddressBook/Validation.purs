module Data.AddressBook.Validation where

import Data.Array
import Data.Either
import Data.Validation
import Data.AddressBook
import Data.Traversable

import qualified Data.String as S
import qualified Data.String.Regex as R 

import Control.Apply

type Errors = [ValidationError]

nonEmpty :: Field -> String -> V Errors Unit
nonEmpty field "" = invalid [ValidationError ("Field '" ++ show field ++ "' cannot be empty") field]
nonEmpty _     _  = pure unit

nonEmpty' :: Field -> String -> V Errors Unit
nonEmpty' field value = matches field whiteSpaceRegex value

arrayNonEmpty :: forall a. Field -> [a] -> V Errors Unit
arrayNonEmpty field [] = invalid [ValidationError ("Field '" ++ show field ++ "' must contain at least one value") field]
arrayNonEmpty _     _  = pure unit

lengthIs :: Field -> Number -> String -> V Errors Unit
lengthIs field len value | S.length value /= len = invalid [ValidationError ("Field '" ++ show field ++ "' must have length " ++ show len) field]
lengthIs _     _   _     = pure unit

regexpFlags :: R.RegexFlags
regexpFlags = { unicode:    false
              , sticky:     false
              , multiline:  false
              , ignoreCase: false
              , global:     false
              } 

phoneNumberRegex :: R.Regex
phoneNumberRegex =
  R.regex
  "^\\d{3}-\\d{3}-\\d{4}$"
  regexpFlags

stateRegex :: R.Regex
stateRegex = 
  R.regex
  "^[A-Z]{2}$"
  regexpFlags

whiteSpaceRegex :: R.Regex
whiteSpaceRegex =
  R.regex
  "^\\S+.*(\\S+)$"
  regexpFlags

matches :: Field -> R.Regex -> String -> V Errors Unit
matches _     regex value | R.test regex value = pure unit
matches field _     _ = invalid [ValidationError ("Field '" ++ show field ++ "' did not match the required format") field]

validateState :: String -> V Errors String
validateState s = (matches StateField stateRegex s *> pure s)

validateAddress :: Address -> V Errors Address
validateAddress (Address a) =
  address <$> (nonEmpty' StreetField  a.street *> pure a.street)
          <*> (nonEmpty' CityField    a.city   *> pure a.city)
          <*> validateState a.state

validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber (PhoneNumber p) =
  phoneNumber <$> pure p."type"
              <*> (matches (PhoneField p."type") phoneNumberRegex p.number *> pure p.number)

validatePerson :: Person -> V Errors Person
validatePerson (Person p) =
  person <$> (nonEmpty' FirstNameField p.firstName *> pure p.firstName)
         <*> (nonEmpty' LastNameField p.lastName *> pure p.lastName)
         <*> traverse validateAddress p.address
         <*> (arrayNonEmpty PhoneNumbersField p.phones *> traverse validatePhoneNumber p.phones)

validatePerson' :: Person -> Either Errors Person
validatePerson' p = runV Left Right $ validatePerson p


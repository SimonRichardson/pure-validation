module Data.AddressBook where

import Data.Maybe

newtype Address = Address
  { street :: String
  , city   :: String
  , state  :: String
  }

address :: String -> String -> String -> Address
address street city state = Address
  { street: street
  , city:   city
  , state:  state
  }

instance showAddress :: Show Address where
  show (Address a) = "Address " ++
    "{ street: " ++ show a.street ++
    ", city: "   ++ show a.city ++
    ", state: "  ++ show a.state ++
    " }"

data ValidationError = ValidationError String Field

data Field = FirstNameField
           | LastNameField
           | StreetField
           | CityField
           | StateField
           | PhoneField PhoneType
           | PhoneNumbersField

instance showField :: Show Field where
  show FirstNameField    = "FirstNameField"
  show LastNameField     = "LastNameField"
  show StreetField       = "StreetField"
  show CityField         = "CityField"
  show StateField        = "StateField"
  show (PhoneField a)    = "PhoneField " ++
    "{ type: " ++ show a ++
    " }"
  show PhoneNumbersField = "PhoneNumbersField"

data PhoneType
  = HomePhone
  | WorkPhone
  | CellPhone
  | OtherPhone

instance showPhoneType :: Show PhoneType where
  show HomePhone  = "HomePhone"
  show WorkPhone  = "WorkPhone"
  show CellPhone  = "CellPhone"
  show OtherPhone = "OtherPhone"

newtype PhoneNumber = PhoneNumber 
  { "type" :: PhoneType
  , number :: String
  }

phoneNumber :: PhoneType -> String -> PhoneNumber
phoneNumber t number = PhoneNumber
  { "type": t
  , number: number
  }

instance showPhoneNumber :: Show PhoneNumber where
  show (PhoneNumber p) = "PhoneNumber " ++
    "{ type: "   ++ show p."type" ++
    ", number: " ++ show p.number ++
    " }"

newtype Person = Person
  { firstName :: String
  , lastName  :: String
  , address   :: Maybe Address
  , phones    :: [PhoneNumber]
  }

person :: String -> String -> Maybe Address -> [PhoneNumber] -> Person
person firstName lastName address phones = Person
  { firstName: firstName
  , lastName:  lastName
  , address:   address
  , phones:    phones
  }

examplePerson :: Person
examplePerson = 
  person "John" "Smith"
         (Just $ address "123 Fake St." "Fake Town" "CA")
         [ phoneNumber HomePhone "555-555-5555"
         , phoneNumber CellPhone "555-555-0000"
         ]

exampleEmptyPersonAddress :: Person
exampleEmptyPersonAddress =
  person "Authur" "Guinness"
         Nothing
         [ phoneNumber HomePhone "555-555-0001"
         ]

instance showPerson :: Show Person where
  show (Person p) = "Person " ++
    "{ firstName: " ++ show p.firstName ++
    ", lastName: "  ++ show p.lastName ++
    ", address: "   ++ show p.address ++
    ", phones: "    ++ show p.phones ++
    " }"


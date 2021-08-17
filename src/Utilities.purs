module Utilities where

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Show (class Show)
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Eq, Unit, discard, show, (+), (<>))

const :: forall a b. a -> b -> a
const x _ = x

flip :: forall a b c. (a -> b -> c) -> b -> a -> c
flip f x y = f y x

apply :: forall a b. (a -> b) -> a -> b
apply f x = f x

applyFlipped :: forall a b. a -> (a -> b) -> b
applyFlipped = flip apply

infixr 0 apply as $

infixl 1 applyFlipped as #

singleton :: forall a. a -> List a
singleton x = x : Nil

null :: forall a. List a -> Boolean
null Nil = true
null _ = false

snoc :: forall a. List a -> a -> List a
snoc (y : ys) x = y : snoc ys x
snoc Nil x = singleton x

length :: forall a. List a -> Int
length l = go 0 l where
  go :: Int -> List a -> Int
  go acc Nil = acc
  go acc (_ : xs) = go (1 + acc) xs

head :: forall a. List a -> Maybe a
head Nil = Nothing
head (x : _) = Just x

tail :: forall a. List a -> Maybe (List a)
tail Nil = Nothing
tail (_ : xs) = Just xs

last :: forall a. List a -> Maybe a
last Nil = Nothing
last (x : Nil) = Just x
last (_ : xs) = last xs

init :: forall a. List a -> Maybe (List a)
init l = go Nil l where
  go :: List a -> List a -> Maybe (List a)
  go _ Nil = Nothing
  go acc (_ : Nil) = Just acc
  go acc (x : xs) = go (x : acc) xs

newtype Address = Address
  { street :: String
  , city :: String
  , state :: String
  , zipCode :: String
  }

newtype Person = Person
  { firstName :: String
  , lastName :: String
  , address :: Maybe Address
  }

newtype Customer = Customer Person

newtype Employee = Employee Person

derive instance newtypeAddress :: Newtype Address _
derive instance newtypePerson :: Newtype Person _
derive instance eqAddress :: Eq Address
derive instance eqPerson :: Eq Person
derive instance newtypeCustomer :: Newtype Customer _
derive instance newtypeEmployee :: Newtype Employee _

class ProcessAddress a where
  getAddress :: a -> Maybe Address

genericGetAddress :: forall a. Newtype a Person => a -> Maybe Address
genericGetAddress wrappedPerson = getAddress $ unwrap wrappedPerson

instance processAddressPerson :: ProcessAddress Person where
  getAddress (Person p) = p.address

derive newtype instance personAddressCustomer :: ProcessAddress Customer

instance showAddress :: Show Address where
  show (Address x) = x.street <> ", " <> x.city <> ", " <> x.state <> " " <> x.zipCode

test :: Effect Unit
test = do
  log $ show $ flip const 1 2
  flip const 1 2 # show # log
  log $ show $ singleton $ "xyz"
  log $ show $ null Nil
  log $ show $ null ("abc" : Nil)
  log $ show $ snoc (1 : 2 : Nil) 3
  log $ show $ length (1 : 2 : 3 : 4 : 5 : Nil)
  log $ show (head Nil :: Maybe Unit)
  log $ show $ head ("abc" : "123" : Nil)
  log $ show $ (tail Nil :: Maybe (List Unit))
  log $ show $ tail ("abc" : "123" : "xyz" : Nil)
  log $ show $ (last Nil :: Maybe (List Unit))
  log $ show $ last ("abc" : "123" : "xyz" : Nil)
  log $ show $ (init Nil :: Maybe (List Unit))
  log $ show $ init (1 : Nil)
  log $ show $ init (1 : 2 : 3 : Nil)
  log $ show $ getAddress (Person { firstName: "Dan", lastName: "Sheikh", address: (Just (Address { street: "Aviation Blvd", city: "Redondo Beach", state: "CA", zipCode: "90278"})) })
  log $ show $ getAddress (Person { firstName: "Dan", lastName: "Sheikh", address: Nothing })
  log $ show $ getAddress (Customer (Person { firstName: "Dan", lastName: "Sheikh", address: (Just (Address { street: "Artesia Blvd", city: "Redondo Beach", state: "CA", zipCode: "90278"})) }))

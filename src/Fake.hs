module Fake ( runFaker
            , Fake(..)
            ) where

import Control.Monad

import           Faker.Utils    (Faker(..), runFaker, randomInt)
import qualified Faker.Name     as F
import qualified Faker.Internet as F
import Data.Text

import Models

class Fake a where
  fake :: Faker a

instance Fake a => Fake [a] where
  fake = do
    i <- randomInt (0, 25)
    replicateM i fake

p :: Functor f => f String -> f Text
p = fmap pack

instance Fake User where
  fake = User <$> p F.email <*> p F.name <*> p F.userName

instance Fake Contest where
  fake = Contest <$> p F.name <*> fake

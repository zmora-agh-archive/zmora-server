module Fake ( runFaker
            , Fake(..)
            ) where

import Control.Monad

import           Faker.Utils    (Faker(..), runFaker, randomInt)
import qualified Faker.Name     as F
import qualified Faker.Internet as F

import Models

class Fake a where
  fake :: Faker a

instance Fake a => Fake [a] where
  fake = do
    i <- randomInt (0, 25)
    replicateM i fake

instance Fake User where
  fake = User <$> F.userName <*> F.name <*> F.email

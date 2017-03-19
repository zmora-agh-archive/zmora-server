{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module Fake where

import Control.Monad.IO.Class

import           Faker.Utils    (Faker(..), toGen, randomInt)
import qualified Faker.Name     as F
import qualified Faker.Internet as F
import qualified Faker.Avatar   as F

import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen(..), choose)
import Data.Text
import Database.Persist.Sql

import Models

p :: Functor f => f String -> f Text
p = fmap pack

instance ToBackendKey SqlBackend a => Arbitrary (Key a) where
  arbitrary = toSqlKey <$> choose (0, 100)

instance (Arbitrary a, PersistEntity a, ToBackendKey SqlBackend a)
            => Arbitrary (Entity a) where
  arbitrary = Entity <$> arbitrary <*> arbitrary

instance Arbitrary User where
  arbitrary = toGen $ User <$> p F.email
                           <*> p F.userName
                           <*> p F.name
                           <*> p F.image

instance Arbitrary Contest where
  arbitrary = toGen $ Contest <$> p F.name

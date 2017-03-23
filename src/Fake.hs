{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module Fake where

import Control.Monad.IO.Class

import           Faker.Utils    (Faker(..), toGen, randomInt)
import qualified Faker.Name     as F
import qualified Faker.Internet as F
import qualified Faker.Avatar   as F
import qualified Faker.Lorem    as F

import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen(..), choose)
import Data.Text
import Data.Time.Clock
import Data.Time.Calendar
import Database.Persist.Sql

import Models

p :: Functor f => f String -> f Text
p = fmap pack

instance ToBackendKey SqlBackend a => Arbitrary (Key a) where
  arbitrary = toSqlKey <$> choose (0, 100)

instance (Arbitrary a, PersistEntity a, ToBackendKey SqlBackend a)
            => Arbitrary (Entity a) where
  arbitrary = Entity <$> arbitrary <*> arbitrary

randomTime :: Faker UTCTime
randomTime = do
  d <- fromGregorian <$> (fromIntegral <$> randomInt (2000, 2050))
                     <*> randomInt (1, 12)
                     <*> randomInt (1, 28)
  t <- secondsToDiffTime <$> fromIntegral <$> randomInt (0, 86401)
  return $ UTCTime d t

instance Arbitrary User where
  arbitrary = toGen $ User <$> p F.userName
                           <*> p F.name
                           <*> p F.image
                           <*> p F.paragraph

instance Arbitrary Contest where
  arbitrary = toGen $ Contest <$> p F.sentence
                              <*> p F.paragraph
                              <*> randomTime
                              <*> randomInt (3600, 360000)
                              <*> randomInt (3600, 360000)

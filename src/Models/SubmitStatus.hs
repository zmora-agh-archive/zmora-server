{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.SubmitStatus where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Hashable
import           Database.Persist
import           Database.Persist.TH
import           GHC.Generics

data SubmitStatus = OK | QUE | ERR
                    deriving (Show, Read, Eq, Generic)

derivePersistField "SubmitStatus"
makeLenses ''SubmitStatus
deriveJSON defaultOptions ''SubmitStatus
instance Hashable SubmitStatus

{-# LANGUAGE TemplateHaskell #-}
module Models.Group where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Database.Persist
import Database.Persist.TH

data Group = AdminGroup | ManagementGroup | UserGroup
             deriving (Show, Read, Eq)

derivePersistField "Group"
makeLenses ''Group
deriveJSON defaultOptions ''Group

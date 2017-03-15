{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import Database.Persist
import Database.Persist.TH
import Data.Text

share
  [ mkPersist sqlSettings { mpsGenerateLenses = True }
  , mkMigrate "migrateAll"
  ] [persistLowerCase|
User json
  email Text
  nick Text
  name Text
  UniqueNick nick
  UniqueEmail email
  deriving Show

Contest json
  name Text
  owner User
  deriving Show  
|]

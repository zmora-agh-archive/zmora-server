{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Queue.Serialization where

import           GHC.Generics
import           Models.Task
import qualified Data.ByteString.Lazy as BS
import           Data.MessagePack

deriving instance Generic File
deriving instance Generic Test
deriving instance Generic Task
deriving instance Generic TestResult
deriving instance Generic TaskResult
instance MessagePack TaskResult
instance MessagePack File
instance MessagePack Test
instance MessagePack Task
instance MessagePack TestResult

defaultSerializer :: MessagePack a => a -> BS.ByteString
defaultSerializer = pack

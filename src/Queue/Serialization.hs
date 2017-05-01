{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}

module Queue.Serialization where

import qualified Data.ByteString.Lazy   as BS
import           Data.MessagePack
import           GHC.Generics
import           Models.Task

deriving instance Generic File
deriving instance Generic Test
deriving instance Generic Task
deriving instance Generic TestResult
deriving instance Generic TaskResult
deriving instance Generic Status
instance MessagePack TaskResult
instance MessagePack File
instance MessagePack Test
instance MessagePack Task
instance MessagePack TestResult
instance MessagePack Status

defaultSerializer :: MessagePack m => m -> BS.ByteString
defaultSerializer = pack

defaultDeserializer :: MessagePack a => BS.ByteString -> IO a
defaultDeserializer = unpack

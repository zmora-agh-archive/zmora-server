{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MockAPI where

import Servant
import GHC.TypeLits             (KnownSymbol(..))
import Servant.API.ContentTypes (AllCTRender(..))
import Control.Monad.IO.Class
import Control.Applicative
import Fake

class HasServer api '[] => HasMock api where
  mock :: Proxy api -> Server api

instance (KnownSymbol path, HasMock rest) => HasMock (path :> rest) where
  mock _ = mock (Proxy :: Proxy rest)

instance (HasMock a, HasMock b) => HasMock (a :<|> b) where
  mock _ = mock (Proxy :: Proxy a) :<|> mock (Proxy :: Proxy b)

instance (Fake a, AllCTRender ctypes a) => HasMock (Get ctypes a) where
  mock _ = liftIO (runFaker fake)

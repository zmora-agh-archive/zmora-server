{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds            #-}
module Utils.ResourceAPI (Resource(..), ResourceAPI(..)) where

import GHC.Types
import Servant
import Data.Int (Int64(..))

data Resource :: Symbol -> [*] -> [*] -> *

type family (++) (as :: [k]) (bs :: [k]) :: [k] where
  (++) a '[] = a
  (++) '[] b = b
  (++) (a ': as) bs = a ': (as ++ bs)

type family ResPaths lvl res where
  ResPaths lvl (Resource path a '[]) = '[('(a, lvl ++ '[path]))]
  ResPaths lvl (Resource path a (x ': xs)) =
    ResPaths (lvl ++ '[path]) x ++ ResPaths lvl (Resource path a xs)

type family ResPathAPI acts paths where
  ResPathAPI (a ': '[]) (path ': '[]) = path :> a
  ResPathAPI (a ':  as) (path ': '[]) = path :> a :<|> ResPathAPI as (path ': '[])
  ResPathAPI (a ': '[]) (path ':  ps) = path :> Capture "id" Int64 :> ResPathAPI (a ': '[]) ps
  ResPathAPI (a ':  as) (path ':  ps) = path :> Capture "id" Int64 :> ResPathAPI (a ': '[]) ps
                                    :<|> ResPathAPI as (path ': ps)
type family DeclMap l where
  DeclMap (x ': '[]) = ResPaths '[] x
  DeclMap (x ':  xs) = ResPaths '[] x ++ DeclMap xs

type family MkResourceApi spec where
  MkResourceApi ('(acts, pths) ': '[]) = ResPathAPI acts pths
  MkResourceApi ('(acts, pths) ':  xs) = ResPathAPI acts pths :<|> MkResourceApi xs

type ResourceAPI decl = MkResourceApi (DeclMap decl)

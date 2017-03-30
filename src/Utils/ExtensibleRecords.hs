{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Utils.ExtensibleRecords
  ( module Utils.ExtensibleRecords
  , module Data.Type.Map
  ) where

import Prelude as P

import Control.Monad (mzero, mplus)
import Data.Aeson
import Data.Int (Int64)
import Data.Aeson.Types
import Data.HashMap.Strict as HM
import Database.Persist.Quasi (nullable)
import Data.Proxy
import Data.Reflection
import Data.Text as T
import Database.Persist.Types
import GHC.TypeLits (KnownSymbol)
import GHC.Types
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.Type.Map

type Rec = Map

type family UnRec r where
  UnRec (Rec a) = a

type family (:%) rec field where
  Rec '[]                  :% _            = Rec '[]
  Rec ((name :-> a) ': xs) :% (name :-> b) = Rec ((name :-> b) ': xs)
  Rec (x ': xs)            :% b            = Rec (x ': UnRec (Rec xs :% b))
infixr 3 :%

type family (:+) rec field where
  Rec x :+ a = Rec (a ': x)
infixl 3 :+

class RecGetProp name a b | name a -> b where
  rGet :: Var name -> Rec a -> b

class RecSetProp name x a where
  rSet :: Var name -> x -> Rec a -> Rec a :% name :-> x

instance RecGetProp name ((name :-> a) ': as) a where
  rGet _   (Ext k v s)  = v

instance RecSetProp name x ((name :-> a) ': as) where
  rSet _ x (Ext k _ s) = Ext k x s

instance {-# OVERLAPS #-} RecGetProp n1 as b => RecGetProp n1 ((n2 :-> a) ': as) b where
  rGet p (Ext _ _ s) = rGet p s

instance {-# OVERLAPS #-}
         ( RecSetProp n1 x as
         , (n2 :-> a) ~ orig, (n1 :-> x) ~ sub
         , (Rec (orig : as) :% sub) ~ Rec (orig : UnRec (Rec as :% sub))
         , (Rec as :% sub) ~ Rec (UnRec (Rec as :% sub))
         ) => RecSetProp n1 x ((n2 :-> a) ': as) where
  rSet p x (Ext k v s) = Ext k v (rSet p x s)

rOver :: (RecSetProp name x a, RecGetProp name a t)
      => Var name -> (t -> x) -> Rec a -> Rec a :% name :-> x
rOver p f rec = rSet p (f (rGet p rec)) rec

class RecExplode m where
  type AsRec m = r | r -> m
  explode :: AsRec m ~ Rec rec => m -> AsRec m

class RecImplode m where
  type ImplCtx m (a :: [Mapping Symbol *]) :: Constraint
  implode :: ImplCtx m a => Rec a -> m

instance ToJSON (Rec '[]) where
  toJSON Empty = emptyObject

instance FromJSON (Rec '[]) where
  parseJSON (Object v) = if HM.null v then return Empty else mzero
--
-- If anyone has idea how to write family of kind :: [Constraint] -> Constraint
-- This can be redone to avoid repacking object every time (better performance)
--
instance ( ToJSON a, ToJSON (Rec as), KnownSymbol name
         ) => ToJSON (Rec ((name :-> a) : as)) where
  toJSON (Ext _ x xs) = object $ property : extractObj (toJSON xs)
    where property = pack (reflect (Proxy :: Proxy name)) .= toJSON x
          extractObj (Object l) = toList l

instance ( FromJSON (Rec as), FromJSON a, KnownSymbol name
         ) => FromJSON (Rec ((name :-> a) : as)) where
  parseJSON (Object v) = do
    let propName = pack (reflect (Proxy :: Proxy name))
    property <- v .: propName
    case fromJSON (Object $ delete propName v) of
      Success a -> return $ Ext (Var :: Var name) property a
      Error _   -> mzero

mkExtensibleRecords :: [EntityDef] -> Q [Dec]
mkExtensibleRecords = mapM $ \def -> do
  let nameToStr = unpack . unHaskellName
      typeName = nameToStr (entityHaskell def)
      modelName = mkName typeName

      recField name typ = constructor `appT` namearg `appT` typ
        where constructor = promotedT '(:->)
              namearg = litT (strTyLit name)

      promList = P.foldr (\x -> appT (promotedConsT `appT` x)) promotedNilT

      -- Copied from Database.Persist.TH
      maybeNullable fd = nullable (fieldAttrs fd) /= NotNullable

      idType :: FieldDef -> TypeQ
      idType fd = case foreignReference fd of
        Just typ -> let res = conT ''Key `appT` conT (mkName $ nameToStr typ)
                    in if maybeNullable fd then conT ''Maybe `appT` res
                                           else res
        Nothing -> ftToType $ fieldType fd

      ftToType :: FieldType -> TypeQ
      ftToType (FTTypeCon Nothing t) = conT $ mkName $ unpack t
      ftToType (FTTypeCon (Just "Data.Int") "Int64") = conT ''Int64
      ftToType (FTTypeCon (Just m) t) = conT $ mkName $ unpack $ T.concat [m, ".", t]
      ftToType (FTApp x y) = ftToType x `appT` ftToType y
      ftToType (FTList x) = listT `appT` ftToType x

      foreignReference field = case fieldReference field of
        ForeignRef ref _ -> Just ref
        _              -> Nothing

      -- End of copied fragment

      fieldNames = P.map (nameToStr . fieldHaskell) $ entityFields def
      fieldTypes = P.map idType $ entityFields def

      recFields = P.zipWith recField fieldNames fieldTypes
      recType = appT (conT ''Rec) $ promList recFields

  patArgs <- mapM newName fieldNames

  let buildRecord (fn, tn) = appE (appE (appE (conE 'Ext) (sigE (conE 'Var) (appT (conT ''Var) (litT (strTyLit fn))))) (varE tn))
      patBody = P.foldr buildRecord (conE 'Empty) (P.zip fieldNames patArgs)

  instanceD (return []) (appT (conT ''RecExplode) (conT modelName)) [
        tySynInstD ''AsRec (tySynEqn [conT modelName] recType)
      , funD 'explode [clause [conP modelName (varP <$> patArgs)] (normalB patBody) []]
    ]

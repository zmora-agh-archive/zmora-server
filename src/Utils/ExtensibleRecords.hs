{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Utils.ExtensibleRecords where

import Prelude as P

import Data.Aeson
import Data.Int (Int64)
import Data.Aeson.Types
import Data.HashMap.Strict
import Data.Proxy
import Data.Reflection
import Data.Text as T
import Database.Persist.Types
import GHC.TypeLits (KnownSymbol)
import GHC.Types
import Language.Haskell.TH

data RecField :: Symbol -> a -> * where
  RecField :: a -> RecField name a

instance Show a => Show (RecField name a) where
  show (RecField a) = show a

data Rec :: [*] -> * where
  (:&) :: !a -> !(Rec as) -> Rec (RecField name a ': as)
  RNil :: Rec '[]
infixr 7 :&

type family UnRec r where
  UnRec (Rec a) = a

type family (:%) rec field where
  Rec '[]                     :% _                 = Rec '[]
  Rec (RecField name a ': xs) :% (RecField name b) = Rec (RecField name b ': xs)
  Rec (x ': xs)               :% b                 = Rec (x ': UnRec (Rec xs :% b))

infixr 7 :%

instance Show (Rec '[]) where
  show RNil = "[]"

instance ( Show a, Show (Rec as)
         ) => Show (Rec (RecField name a : as)) where
  show (x :& xs) = show x ++ " : " ++ show xs


instance ToJSON (Rec '[]) where
    toJSON RNil = emptyObject
--
-- If anyone has idea how to write family of kind :: [Constraint] -> Constraint
-- This can be redone to avoid repacking object every time (better performance)
--
instance ( ToJSON a, ToJSON (Rec as), KnownSymbol name
         ) => ToJSON (Rec (RecField name a : as)) where
    toJSON (x :& xs) = object $ [property] ++ extractObj (toJSON xs)
      where property = pack (reflect (Proxy :: Proxy name)) .= toJSON x
            extractObj (Object l) = toList l

mkExtensibleRecords :: [EntityDef] -> Q [Dec]
mkExtensibleRecords = mapM $ \def -> do
  let nameToStr = unpack . unHaskellName
      typeName = nameToStr (entityHaskell def)
      recName = mkName (typeName ++ "Rec")

      recField name typ = constructor `appT` namearg `appT` typ
        where constructor = conT (mkName "RecField")
              namearg = litT (strTyLit name)

      promList [] = promotedNilT
      promList (x:xs) = promotedConsT `appT` x `appT` (promList xs)

      fieldToRecField x = recField (nameToStr $ fieldHaskell x) $ idType x
        where typeName (FTTypeCon _ a) = unpack a

              foreignReference field = case fieldReference field of
                  ForeignRef ref _ -> Just ref
                  _              -> Nothing

              -- Copied from Database.Persist.TH
              idType :: FieldDef -> TypeQ
              idType fd = case foreignReference fd of
                Just typ -> conT ''Key `appT` (conT $ mkName $ nameToStr typ)
                Nothing -> ftToType $ fieldType fd

              ftToType :: FieldType -> TypeQ
              ftToType (FTTypeCon Nothing t) = conT $ mkName $ unpack t
              ftToType (FTTypeCon (Just "Data.Int") "Int64") = conT ''Int64
              ftToType (FTTypeCon (Just m) t) = conT $ mkName $ unpack $ T.concat [m, ".", t]
              ftToType (FTApp x y) = ftToType x `appT` ftToType y
              ftToType (FTList x) = listT `appT` ftToType x

      recFields = P.map fieldToRecField $ entityFields def

      recType = appT (conT (mkName "Rec")) $ promList recFields

  tySynD recName [] recType

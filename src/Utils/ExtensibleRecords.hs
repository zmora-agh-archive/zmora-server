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

module Utils.ExtensibleRecords where

import Prelude as P

import Data.Aeson
import Data.Int (Int64)
import Data.Aeson.Types
import Data.HashMap.Strict
import Database.Persist.Quasi (nullable)
import Data.Proxy
import Data.Reflection
import Data.Text as T
import Database.Persist.Types
import GHC.TypeLits (KnownSymbol)
import GHC.Types
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data RecField :: Symbol -> a -> * where

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

class RecGetProp name a b | name a -> b where
  rGet :: Proxy name -> Rec a -> b

class RecSetProp name x a where
  rSet :: Proxy name -> x -> Rec a -> Rec a :% RecField name x

instance RecGetProp name (RecField name a ': as) a where
  rGet _   (a :& _)  = a

instance RecSetProp name x (RecField name a ': as) where
  rSet _ x (a :& as) = x :& as

instance {-# OVERLAPS #-} RecGetProp n1 as b => RecGetProp n1 (RecField n2 a ': as) b where
  rGet p (_ :& as) = rGet p as

instance {-# OVERLAPS #-}
         ( RecSetProp n1 x as
         , RecField n2 a ~ orig, RecField n1 x ~ sub
         , (Rec (orig : as) :% sub) ~ Rec (orig : UnRec (Rec as :% sub))
         , (Rec as :% sub) ~ Rec (UnRec (Rec as :% sub))
         ) => RecSetProp n1 x (RecField n2 a ': as) where
  rSet p x (a :& as) = a :& rSet p x as

rOver :: (RecSetProp name x a, RecGetProp name a t)
      => Proxy name -> (t -> x) -> Rec a -> Rec a :% RecField name x
rOver p f rec = rSet p (f (rGet p rec)) rec

instance Show (Rec '[]) where
  show RNil = "[]"

instance ( Show a, Show (Rec as)
         ) => Show (Rec (RecField name a : as)) where
  show (x :& xs) = show x ++ " : " ++ show xs

class RecExplode m where
  type AsRec m
  explode :: (AsRec m ~ Rec k) => m -> AsRec m

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
      modelName = mkName typeName

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
              maybeNullable fd = nullable (fieldAttrs fd) /= NotNullable

              idType :: FieldDef -> TypeQ
              idType fd = case foreignReference fd of
                Just typ -> let res = conT ''Key `appT` (conT $ mkName $ nameToStr typ)
                            in if maybeNullable fd then conT ''Maybe `appT` res
                                                   else res
                Nothing -> ftToType $ fieldType fd

              ftToType :: FieldType -> TypeQ
              ftToType (FTTypeCon Nothing t) = conT $ mkName $ unpack t
              ftToType (FTTypeCon (Just "Data.Int") "Int64") = conT ''Int64
              ftToType (FTTypeCon (Just m) t) = conT $ mkName $ unpack $ T.concat [m, ".", t]
              ftToType (FTApp x y) = ftToType x `appT` ftToType y
              ftToType (FTList x) = listT `appT` ftToType x

      recFields = P.map fieldToRecField $ entityFields def
      recType = appT (conT ''Rec) $ promList recFields

  patArgs <- mapM (newName . nameToStr . fieldHaskell) $ entityFields def

  let buildInfix a acc = infixE (Just $ varE a) (conE '(:&)) (Just acc)
      patBody = P.foldr buildInfix (conE 'RNil) patArgs

  instanceD (return []) (appT (conT ''RecExplode) (conT modelName)) [
        tySynInstD ''AsRec (tySynEqn [conT modelName] recType)
      , funD 'explode [clause [conP modelName (varP <$> patArgs)] (normalB patBody) []]
    ]

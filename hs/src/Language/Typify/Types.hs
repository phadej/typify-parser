{-# LANGUAGE OverloadedStrings #-}
module Language.Typify.Types where

import Control.Applicative
import qualified Test.QuickCheck as QC
import Data.Aeson
import Data.Aeson.Types (Pair)

-- | Names are simply strings
type Name = String

-- Type
data Type  = TyTrue | TyFalse | TyUnit
           | TyNumber Integer
           | TyString String
           | TyBool Bool
           | TyIdentifier Name
           --  Combinators
           | TyNamed Type Type
           | TyConjunction [Type]
           | TyDisjunction [Type]
           | TyProduct [Type]
           | TyOptional Type
           | TyVariadic Type
           | TySquare Type
           | TyCurly Type
           | TyApplication Type [Type]
           | TyFunction Type Type
  deriving (Eq, Ord, Show)

tyOptional :: Type -> Type
tyOptional (TyOptional t) = TyOptional t
tyOptional t              = TyOptional t

tyVariadic :: Type -> Type
tyVariadic (TyVariadic t) = TyVariadic t
tyVariadic t              = TyVariadic t

tyConjunction :: [Type] -> Type
tyConjunction = g . concatMap f
  where  f (TyConjunction ts)  = ts
         f t                   = [t]
         g []                  = TyTrue
         g [t]                 = t
         g ts                  = TyConjunction ts

tyDisjunction :: [Type] -> Type
tyDisjunction = g . concatMap f
  where  f (TyDisjunction ts)  = ts
         f t                   = [t]
         g []                  = TyFalse
         g [t]                 = t
         g ts                  = TyDisjunction ts

tyProduct :: [Type] -> Type
tyProduct = g . concatMap f
  where  f (TyProduct ts)      = ts
         f t                   = [t]
         g []                  = TyFalse
         g [t]                 = t
         g ts                  = TyProduct ts

tyApplication :: Type -> [Type] -> Type
tyApplication rator [] = rator
tyApplication rator rs = TyApplication rator rs

-- Generator instance
arbitraryName :: QC.Gen Name
arbitraryName = QC.elements ["a", "b", "c", "x", "y", "z", "foo", "bar"]

arbitraryMaybeName :: QC.Gen (Maybe Name)
arbitraryMaybeName = QC.oneof [pure Nothing, Just <$> arbitraryName]

arbitraryType :: Int -> QC.Gen Type
arbitraryType 0 = QC.oneof [
  pure TyTrue,
  pure TyFalse,
  pure TyUnit,
  (TyNumber . abs) <$> QC.arbitrary,
  TyString <$> QC.arbitrary,
  TyBool <$> QC.arbitrary,
  TyIdentifier <$> arbitraryName
  ]
arbitraryType n = QC.oneof [
  arbitraryType 0,
  TyNamed <$> arbitraryType' <*> arbitraryType',
  tyConjunction <$> QC.listOf arbitraryType',
  tyDisjunction <$> QC.listOf arbitraryType',
  tyProduct <$> QC.listOf arbitraryType',
  tyOptional <$> arbitraryType',
  tyVariadic <$> arbitraryType',
  TySquare <$> arbitraryType',
  TyCurly <$> arbitraryType',
  TyApplication <$> arbitraryType' <*> QC.listOf1 arbitraryType',
  TyFunction <$> arbitraryType' <*> arbitraryType'
  ]
  where arbitraryType' = arbitraryType $ n - 1

shrinkType :: Type -> [Type]
shrinkType (TyDisjunction xs) = map tyDisjunction $ QC.shrinkList shrinkType xs
shrinkType (TyConjunction xs) = map tyConjunction $ QC.shrinkList shrinkType xs
shrinkType (TyProduct xs)     = map tyProduct     $ QC.shrinkList shrinkType xs
shrinkType (TyFunction x y)   = [ TyFunction x'' y | x'' <- x' ] ++ [ TyFunction x y'' | y'' <- y' ] ++ [ TyFunction x'' y'' | x'' <- x', y'' <- y' ]
  where x' = shrinkType x
        y' = shrinkType y
shrinkType (TyApplication x ys) = [ tyApplication x'' ys | x'' <- x' ] ++ [ tyApplication x ys'' | ys'' <- ys' ] ++ [ tyApplication x'' ys'' | x'' <- x', ys'' <- ys' ]
  where x' = shrinkType x
        ys' = QC.shrinkList shrinkType ys
shrinkType _ = []

instance QC.Arbitrary Type where
  arbitrary = QC.sized arbitraryType'
    where arbitraryType' n = arbitraryType $ min n 2

  shrink = shrinkType

-- Aeson serializing
typeObject :: String -> [Pair] -> Value
typeObject ty ps = object $ ("type" .= ty) : ps

instance ToJSON Type where
  toJSON TyTrue = typeObject "true" []
  toJSON TyFalse = typeObject "false" []
  toJSON TyUnit = typeObject "unit" []
  toJSON (TyNumber n) = typeObject "number" [ "value" .= n ]
  toJSON (TyString s) = typeObject "string" [ "value" .= s ]
  toJSON (TyBool b)   = typeObject "bool" [ "value" .= b ]
  toJSON (TyIdentifier i) = typeObject "ident" [ "value" .= i ]
  toJSON (TyNamed n t)    = typeObject "named" [ "name" .= n, "arg" .= t ]
  toJSON (TyConjunction ts) = typeObject "conjunction" [ "args" .= ts ]
  toJSON (TyDisjunction ts) = typeObject "disjunction" [ "args" .= ts ]
  toJSON (TyProduct ts) = typeObject "product" [ "args" .= ts ]
  toJSON (TyOptional t) = typeObject "optional" [ "arg" .= t ]
  toJSON (TyVariadic t) = typeObject "variadic" [ "arg" .= t ]
  toJSON (TySquare t) = typeObject "square" [ "arg" .= t ]
  toJSON (TyCurly t) = typeObject "curly" [ "arg" .= t ]
  toJSON (TyApplication x ys) = typeObject "application" [ "callee" .= x, "args" .= ys ]
  toJSON (TyFunction a b) = typeObject "function" [ "arg" .= a, "result" .= b ]
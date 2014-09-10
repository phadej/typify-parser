module Typify where

import Data.Tuple
import qualified Data.Map as Map
import Data.Maybe

import Control.Alt
import Control.Plus
import Control.Alternative

import Debug.Trace

-- | Names are simply strings
type Name = String

-- Type
data Type  = TyTrue | TyFalse | TyUnit
           | TyNumber Number
           | TyString String
           | TyBool Boolean
           | TyRecord (Map.Map Name Type)
           | TyIdentifier Name
           | TyNamed Name Type
           | TyConjunction [Type]
           | TyDisjunction [Type]
           | TyProduct [Type]
           | TyOptional Type
           | TyVariadic Type
           | TyBrackets Type
           | TyApplication Type [Type]
           | TyFunction Type Type

-- Javascript interface
foreign import data JsType :: *

foreign import jsTrue
  "var jsTrue = { type: 'true' }" :: JsType

foreign import jsFalse
  "var jsFalse = { type: 'false' }" :: JsType

-- Arrow
second :: forall a b c. (a -> b) -> Tuple c a -> Tuple c b
second f (Tuple x y) = Tuple x (f y)

-- Parser framework
data Parser tok a = Parser ([tok] -> Maybe (Tuple [tok] a))

instance functorParser :: Functor (Parser tok) where
  (<$>) f (Parser p) = Parser (\s -> second f <$> p s)

applyImpl :: forall a b tok. ([tok] -> Maybe (Tuple [tok] (a -> b))) -> ([tok] -> Maybe (Tuple [tok] a)) -> ([tok] -> Maybe (Tuple [tok] b))
applyImpl f x s =
  do
    Tuple s1 f1 <- f s
    Tuple s2 x1 <- x s1
    return (Tuple s2 (f1 x1))

instance applyParser :: Apply (Parser tok) where
  (<*>) (Parser f) (Parser x) = Parser (applyImpl f x)

instance applicativeParser :: Applicative (Parser tok) where
  pure x = Parser (\s -> Just (Tuple s x))

altImpl :: forall a tok. ([tok] -> Maybe (Tuple [tok] a)) -> ([tok] -> Maybe (Tuple [tok] a)) -> ([tok] -> Maybe (Tuple [tok] a))
altImpl p q s = p s <|> q s

instance altParser :: Alt (Parser tok) where
  (<|>) (Parser p) (Parser q) = Parser (altImpl p q)

instance plusParser :: Plus (Parser tok) where
  empty = Parser (\_ -> Nothing)

instance alternativeParser :: Alternative (Parser tok)

eof :: forall tok. Parser tok Unit
eof = Parser eofImpl
  where eofImpl [] = Just (Tuple [] unit)
        eofImpl _  = Nothing

tokenWith :: forall tok. (tok -> Boolean) -> Parser tok tok
tokenWith f = Parser p
  where p (t:ts) | f t = Just (Tuple ts t)
        p _            = Nothing

token :: forall tok. (Eq tok) => tok -> Parser tok tok
token t = tokenWith ((==) t)

-- Lexer


parse :: String -> JsType
parse _ = jsFalse

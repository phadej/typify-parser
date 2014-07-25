module Language.Typify where

import Control.Applicative hiding ((<|>), many, optional)

import Text.Parsec
import Text.Parsec.String

import Data.List (intercalate)

import Data.Map (Map)

import qualified Test.QuickCheck as QC

-- | Names are simply strings
type Name = String

-- | Function parameters (arguments)
data Parameter = Parameter {
  parameterName :: Maybe Name,
  parameterType :: Type,
  parameterVariadic :: Bool,
  parameterOptional :: Bool
}
  deriving (Eq, Ord, Show)

-- Type
data Type  = TyTrue | TyFalse
           | TyNumber Integer
           | TyString String
           | TyBool Bool
           | TyRecord (Map Name Type)
           | TyIdentifier Name
           | TyConjunction [Type]
           | TyDisjunction [Type]
           | TyOptional Type
           | TyApplication Type [Type]
           | TyFunction [Parameter] Type
  deriving (Eq, Ord, Show)

tyOptional :: Type -> Type
tyOptional (TyOptional t) = TyOptional t
tyOptional t              = TyOptional t

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

tyApplication :: Type -> [Type] -> Type
tyApplication rator [] = rator
tyApplication rator rs = TyApplication rator rs

-- Pretty printing
data PrettySpec = PrettySpec {
  pTrue        :: String,
  pFalse       :: String,
  pNumber      :: Integer -> String,
  pString      :: String -> String,
  pBool        :: Bool -> String,
  pIdentifier  :: Name -> String,
  pParameterName  :: Name -> String,
  pRecordName  :: Name -> String,
  pOptional    :: String -> String,
  pRecord      :: String -> String,
  pQMark       :: String,
  pConj        :: String,
  pDisj        :: String,
  pEllipsis    :: String,
  pTo          :: String,
  pTimes       :: String,
  pColon       :: String,
  pRecColon    :: String,
  pSemicolon   :: String
}

defaultPrettySpec :: PrettySpec
defaultPrettySpec = PrettySpec {
  pTrue        = "*",
  pFalse       = "_|_",
  pNumber      = show,
  pString      = show,
  pBool        = \b -> if b then "true" else "false",
  pIdentifier  = id,
  pRecordName  = id,
  pParameterName = id,
  pOptional    = \x -> '[' : x ++ "]",
  pRecord      = \x -> '{' : x ++ "}",
  pQMark       = "?",
  pConj        = " & ",
  pDisj        = " | ",
  pEllipsis    = "...",
  pTo          = " -> ",
  pTimes       = ", ",
  pColon       = " : ",
  pRecColon    = ": ",
  pSemicolon   = "; "
}

class Pretty a where
  prettyPrec :: PrettySpec -> Int -> a -> String

pretty :: Pretty a => a -> String
pretty = prettyPrec defaultPrettySpec 0

prettyParens :: Bool -> String -> String
prettyParens False  x  = x
prettyParens True   x  = '(' : x ++ ")"

instance Pretty Parameter where
  prettyPrec p _ (Parameter n t v True)  = pOptional p $ n' ++ prettyPrec p 0 t ++ v'
    where  n' = case n of
                  Nothing  -> ""
                  Just x   -> pParameterName p x ++ pColon p
           v' = if v then pEllipsis p else ""
  prettyPrec p _ (Parameter n t True False) = n' ++ prettyPrec p 3 t ++ pEllipsis p
    where  n' = case n of
                  Nothing  -> ""
                  Just x   -> pParameterName p x ++ pColon p
  prettyPrec p _ (Parameter n t False False) = n' ++ prettyPrec p 4 t
    where  n' = case n of
                  Nothing  -> ""
                  Just x   -> pParameterName p x ++ pColon p


instance Pretty Type where
  prettyPrec p _ TyTrue                = pTrue p
  prettyPrec p _ TyFalse               = pFalse p
  prettyPrec p _ (TyNumber x)          = pNumber p x
  prettyPrec p _ (TyString x)          = pString p x
  prettyPrec p _ (TyBool x)            = pBool p x
  prettyPrec p _ (TyRecord x)          = undefined p x
  prettyPrec p _ (TyIdentifier x)      = pIdentifier p x
  prettyPrec p d (TyConjunction xs)    = prettyParens (d > 6) $ intercalate (pConj p) $ map (prettyPrec p 6) xs
  prettyPrec p d (TyDisjunction xs)    = prettyParens (d > 5) $ intercalate (pDisj p) $ map (prettyPrec p 5) xs
  prettyPrec p d (TyOptional x)        = prettyParens (d > 8) (prettyPrec p 8 x) ++ pQMark p
  prettyPrec p d (TyApplication x ys)  = prettyParens (d > 7) $ intercalate " " $ map (prettyPrec p 8) $ x : ys
  prettyPrec p d (TyFunction xs y)     = prettyParens (d > 1) $ intercalate (pTimes p) (map (prettyPrec p 0) xs) ++ pTo p ++ prettyPrec p 1 y

-- @(a... -> b)... -> c@
test0 :: Type
test0 = TyFunction [Parameter Nothing f True False] c
  where c  = TyIdentifier "c"
        f  = TyFunction [Parameter Nothing (TyIdentifier "a") True False] (TyIdentifier "b")

-- @a, ys : b..., c, d -> d -> e@
test1 :: Type
test1 = TyFunction [a, b, c, d] (TyFunction [d] e)
  where  a  = Parameter Nothing (TyIdentifier "a") False False
         b  = Parameter (Just "ys") (TyIdentifier "b") True False
         c  = Parameter Nothing (TyIdentifier "c") False False
         d  = Parameter Nothing (TyIdentifier "d") False False
         e  = TyIdentifier "e"

-- @a | b? & c d... -> e@
test2 :: Type
test2 = TyFunction [Parameter Nothing abcd True False] e
  where  abcd  = TyDisjunction [a, bcd]
         bcd   = TyConjunction [b, cd]
         a     = TyIdentifier "a"
         b     = TyOptional (TyIdentifier "b")
         cd    = TyApplication (TyIdentifier "c") [TyIdentifier "d"]
         e     = TyIdentifier "e"

-- @a, ys : b -> c@
test3 :: Type
test3 = TyFunction [Parameter Nothing a False False, Parameter (Just "y") b False False] c
  where  a  = TyIdentifier "a"
         b  = TyIdentifier "b"
         c  = TyIdentifier "c"

-- Generator instance
arbitraryName :: QC.Gen Name
arbitraryName = QC.elements ["a", "b", "c", "x", "y", "z", "foo", "bar"]

arbitraryMaybeName :: QC.Gen (Maybe Name)
arbitraryMaybeName = QC.oneof [pure Nothing, Just <$> arbitraryName]

arbitraryParameter :: Int -> QC.Gen Parameter
arbitraryParameter n = Parameter <$> arbitraryMaybeName <*> arbitraryType n <*> QC.arbitrary <*> QC.arbitrary

arbitraryType :: Int -> QC.Gen Type
arbitraryType 0 = QC.oneof [
  pure TyTrue,
  pure TyFalse,
  {-
  (TyNumber . abs) <$> QC.arbitrary,
  TyString <$> QC.arbitrary,
  -}
  TyBool <$> QC.arbitrary,
  TyIdentifier <$> arbitraryName
  ]
arbitraryType n = QC.oneof [
  arbitraryType 0,
  tyConjunction <$> QC.listOf arbitraryType',
  tyDisjunction <$> QC.listOf arbitraryType',
  tyOptional <$> arbitraryType',
  TyApplication <$> arbitraryType' <*> QC.listOf1 arbitraryType',
  TyFunction <$> QC.listOf (arbitraryParameter $ n - 1) <*> arbitraryType'
  ]
  where arbitraryType' = arbitraryType $ n - 1

shrinkParameter :: Parameter -> [Parameter]
shrinkParameter (Parameter n t v a) = [ Parameter n t' v' a' | t' <- shrinkType t, v' <- QC.shrink v, a' <- QC.shrink a ]

shrinkType :: Type -> [Type]
shrinkType (TyDisjunction xs) = map tyDisjunction $ QC.shrinkList shrinkType xs
shrinkType (TyConjunction xs) = map tyConjunction $ QC.shrinkList shrinkType xs
shrinkType (TyFunction xs y)  = [ TyFunction xs' y' | xs' <- QC.shrinkList shrinkParameter xs , y' <- shrinkType y ]
shrinkType (TyApplication x ys) = [ tyApplication x'' ys | x'' <- x' ] ++ [ tyApplication x ys'' | ys'' <- ys' ] ++ [ tyApplication x'' ys'' | x'' <- x', ys'' <- ys' ]
  where x' = shrinkType x
        ys' = QC.shrinkList shrinkType ys
shrinkType _ = []

instance QC.Arbitrary Type where
  arbitrary = QC.sized arbitraryType'
    where arbitraryType' n = arbitraryType $ min n 2

  shrink = shrinkType

-- Parser

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

openingBrace :: Parser ()
openingBrace = const () <$> lexeme (char '(') <?> "opening brace"

closingBrace :: Parser ()
closingBrace = const () <$> lexeme (char ')') <?> "closing brace"

openingBracket :: Parser ()
openingBracket = const () <$> lexeme (char '[') <?> "opening bracket"

closingBracket :: Parser ()
closingBracket = const () <$> lexeme (char ']') <?> "closing bracket"

braces :: Parser a -> Parser a
braces p = openingBrace *> p <* closingBrace

brakets :: Parser a -> Parser a
brakets p = openingBracket *> p <* closingBracket

firstNameLetter :: Parser Char
firstNameLetter = oneOf "@$_" <|> letter

restNameLetter :: Parser Char
restNameLetter = oneOf "@$_" <|> alphaNum

nameP :: Parser Name
nameP = lexeme ((:) <$> firstNameLetter <*> many restNameLetter)

tyTrueP :: Parser Type
tyTrueP = const TyTrue <$> lexeme (oneOf "*⊤")

tyFalseP :: Parser Type
tyFalseP = const TyFalse <$> lexeme (string "⊥" <|> try (string "_|_"))

tyNumberP :: Parser Type
tyNumberP = fail "can't parse numbers"

tyStringP :: Parser Type
tyStringP = fail "can't parse strings"

tyRecordP :: Parser Type
tyRecordP = fail "can't parse records"

tyIdentifierP :: Parser Type
tyIdentifierP = f <$> nameP
  where f "true"  = TyBool True
        f "false" = TyBool False
        f n       = TyIdentifier n

typeParser :: Parser Type
typeParser = tyFunctionP

totalTypeParser :: Parser Type
totalTypeParser = spaces *> typeParser <* eof

tyApplicationP :: Parser Type
tyApplicationP = f <$> many1 tyOptionalP
  where f (x:xs)  = tyApplication x xs
        f []      = error "applicationP: empty many1 result"

tyOptionalP :: Parser Type
tyOptionalP = f <$> terminalP <*> optionMaybe (lexeme (char '?'))
  where f t Nothing  = t
        f t (Just _) = TyOptional t

tyConjunctionP :: Parser Type
tyConjunctionP = tyConjunction <$> tyApplicationP `sepBy1` lexeme (oneOf "∧&")

tyDisjunctionP :: Parser Type
tyDisjunctionP = tyDisjunction <$> tyConjunctionP `sepBy1` lexeme (oneOf "∨|")

parameterP :: Parser Parameter
parameterP = (f <$> brakets (namedParameterP typeParser)) <|> namedParameterP tyDisjunctionP
  where f p = p { parameterOptional = True }

ellipsisP :: Parser ()
ellipsisP = const () <$> lexeme (string "...")

namedParameterP :: Parser Type -> Parser Parameter
namedParameterP p = f <$> optionMaybe (try $ nameP <* lexeme (char ':')) <*> p <*> optionMaybe ellipsisP
  where f n t Nothing  = Parameter n t False False
        f n t (Just _) = Parameter n t True False

parametersP :: Parser [Parameter]
parametersP = p
  where p = parameterP `sepBy` lexeme (oneOf ",×")

arrowP :: Parser ()
arrowP = const () <$> lexeme (string "->" <|> string "→")

tyFunctionP :: Parser Type
tyFunctionP = try (TyFunction <$> parametersP <* arrowP <*> tyFunctionP) <|> tyDisjunctionP

terminalP :: Parser Type
terminalP = tyTrueP <|> tyFalseP <|> tyIdentifierP <|> braces typeParser
-- tyNumberP <|> tyStringP <|> tyRecordP

module Language.Typify where

import Control.Applicative hiding ((<|>), many, optional)

import Text.Parsec
import Text.Parsec.String

import Data.List (intercalate)

import Data.Map (Map)

import qualified Test.QuickCheck as QC

-- | Names are simply strings
type Name = String

-- Type
data Type  = TyTrue | TyFalse | TyUnit
           | TyNumber Integer
           | TyString String
           | TyBool Bool
           | TyRecord (Map Name Type)
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

-- Pretty printing
data PrettySpec = PrettySpec {
  pTrue        :: String,
  pFalse       :: String,
  pUnit        :: String,
  pNumber      :: Integer -> String,
  pString      :: String -> String,
  pBool        :: Bool -> String,
  pIdentifier  :: Name -> String,
  pParameterName  :: Name -> String,
  pRecordName  :: Name -> String,
  pBrackets    :: String -> String,
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
  pUnit        = "()",
  pNumber      = show,
  pString      = show,
  pBool        = \b -> if b then "true" else "false",
  pIdentifier  = id,
  pRecordName  = id,
  pParameterName = id,
  pBrackets    = \x -> '[' : x ++ "]",
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

instance Pretty Type where
  prettyPrec p _ TyTrue                = pTrue p
  prettyPrec p _ TyFalse               = pFalse p
  prettyPrec p _ TyUnit                = pUnit p
  prettyPrec p _ (TyNumber x)          = pNumber p x
  prettyPrec p _ (TyString x)          = pString p x
  prettyPrec p _ (TyBool x)            = pBool p x
  prettyPrec p _ (TyRecord x)          = undefined p x
  prettyPrec p _ (TyIdentifier x)      = pIdentifier p x
  prettyPrec p _ (TyBrackets x)        = pBrackets p $ prettyPrec p 0 x
  prettyPrec p d (TyVariadic x)        = prettyParens (d > 4) $ prettyPrec p 4 x ++ pEllipsis p
  prettyPrec p d (TyNamed n x)         = prettyParens (d > 3) $ n ++ pColon p ++ prettyPrec p 3 x
  prettyPrec p d (TyProduct xs)        = prettyParens (d > 2) $ intercalate (pTimes p) $ map (prettyPrec p 2) xs
  prettyPrec p d (TyConjunction xs)    = prettyParens (d > 6) $ intercalate (pConj p) $ map (prettyPrec p 6) xs
  prettyPrec p d (TyDisjunction xs)    = prettyParens (d > 5) $ intercalate (pDisj p) $ map (prettyPrec p 5) xs
  prettyPrec p d (TyOptional x)        = prettyParens (d > 8) (prettyPrec p 8 x) ++ pQMark p
  prettyPrec p d (TyApplication x ys)  = prettyParens (d > 7) $ intercalate " " $ map (prettyPrec p 8) $ x : ys
  prettyPrec p d (TyFunction x y)      = prettyParens (d > 1) $ prettyPrec p 0 x ++ pTo p ++ prettyPrec p 1 y

-- @(a... -> b)... -> c@
test0 :: Type
test0 = TyFunction (TyVariadic f) c
  where c  = TyIdentifier "c"
        f  = TyFunction (TyVariadic $ TyIdentifier "a") (TyIdentifier "b")

-- @a, ys : b..., c, d -> d -> e@
test1 :: Type
test1 = TyFunction (tyProduct [a, b, c, d]) (TyFunction d e)
  where  a  = TyIdentifier "a"
         b  = TyNamed "ys" $ tyVariadic $ TyIdentifier "b"
         c  = TyIdentifier "c"
         d  = TyIdentifier "d"
         e  = TyIdentifier "e"

-- @a | b? & c d... -> e@
test2 :: Type
test2 = TyFunction (tyVariadic abcd) e
  where  abcd  = TyDisjunction [a, bcd]
         bcd   = TyConjunction [b, cd]
         a     = TyIdentifier "a"
         b     = TyOptional (TyIdentifier "b")
         cd    = TyApplication (TyIdentifier "c") [TyIdentifier "d"]
         e     = TyIdentifier "e"

-- @a, ys : b -> c@
test3 :: Type
test3 = TyFunction (tyProduct [a, TyNamed "ys" b]) c
  where  a  = TyIdentifier "a"
         b  = TyIdentifier "b"
         c  = TyIdentifier "c"

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
  {-
  (TyNumber . abs) <$> QC.arbitrary,
  TyString <$> QC.arbitrary,
  -}
  TyBool <$> QC.arbitrary,
  TyIdentifier <$> arbitraryName
  ]
arbitraryType n = QC.oneof [
  arbitraryType 0,
  TyNamed <$> arbitraryName <*> arbitraryType',
  tyConjunction <$> QC.listOf arbitraryType',
  tyDisjunction <$> QC.listOf arbitraryType',
  tyProduct <$> QC.listOf arbitraryType',
  tyOptional <$> arbitraryType',
  tyVariadic <$> arbitraryType',
  TyBrackets <$> arbitraryType',
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

brackets :: Parser a -> Parser a
brackets p = openingBracket *> p <* closingBracket

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

tyUnitP :: Parser Type
tyUnitP = const TyUnit <$> lexeme (string "𝟙" <|> try (string "()"))

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
        f t (Just _) = tyOptional t

tyConjunctionP :: Parser Type
tyConjunctionP = tyConjunction <$> tyApplicationP `sepBy1` lexeme (oneOf "∧&")

tyDisjunctionP :: Parser Type
tyDisjunctionP = tyDisjunction <$> tyConjunctionP `sepBy1` lexeme (oneOf "∨|")

colonP :: Parser ()
colonP = const () <$> lexeme (char ':')

tyVariadicP :: Parser Type
tyVariadicP = f <$> tyDisjunctionP <*> optionMaybe ellipsisP
  where f t Nothing  = t
        f t (Just _) = tyVariadic t

tyNamedP :: Parser Type
tyNamedP = (TyNamed <$> try (nameP <* colonP) <*> tyNamedP) <|> tyVariadicP

tyProductP :: Parser Type
tyProductP = tyProduct <$> tyNamedP `sepBy1` lexeme (oneOf "×,")

ellipsisP :: Parser ()
ellipsisP = const () <$> lexeme (string "...")

arrowP :: Parser ()
arrowP = const () <$> lexeme (string "->" <|> string "→")

tyFunctionP :: Parser Type
tyFunctionP = f <$> tyProductP <*> optionMaybe (arrowP *> tyFunctionP)
  where f a Nothing  = a
        f a (Just b) = TyFunction a b

terminalP :: Parser Type
terminalP = choice [
  tyTrueP, tyFalseP, tyUnitP,
--  tyNumberP, tyStringP, tyRecordP
  tyIdentifierP,
  braces typeParser,
  brackets (TyBrackets <$> typeParser)
  ]

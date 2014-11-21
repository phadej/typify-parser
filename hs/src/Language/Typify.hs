module Language.Typify where

import Language.Typify.Types

import Control.Applicative hiding ((<|>), many, optional)

import Text.Parsec
import Text.Parsec.String

import Numeric (showHex, readHex)

import Data.List (intercalate)

-- Pretty printing
data PrettySpec = PrettySpec {
  pTrue        :: String,
  pFalse       :: String,
  pUnit        :: String,
  pNumber      :: Integer -> String,
  pString      :: String -> String,
  pBool        :: Bool -> String,
  pIdentifier  :: Name -> String,
  pSquare      :: String -> String,
  pCurly       :: String -> String,
  pQMark       :: String,
  pConjunction :: String,
  pDisjunction :: String,
  pEllipsis    :: String,
  pTo          :: String,
  pTimes       :: String,
  pColon       :: String,
  pRecColon    :: String,
  pSemicolon   :: String
}

escapeString :: String -> String
escapeString = concatMap f
  where f '\'' = "\\'"
        f '\n' = "\\n"
        f '\\' = "\\\\"
        f c
          | code < 16 = "\\x0" ++ showHex code ""
          | code < 32 = "\\x" ++ showHex code ""
          | otherwise = [c]
          where code = fromEnum c

defaultPrettySpec :: PrettySpec
defaultPrettySpec = PrettySpec {
  pTrue        = "*",
  pFalse       = "_|_",
  pUnit        = "()",
  pNumber      = show,
  pString      = \s -> '\'' : escapeString s ++ "'",
  pBool        = \b -> if b then "true" else "false",
  pIdentifier  = id,
  pSquare      = \x -> '[' : x ++ "]",
  pCurly       = \x -> '{' : x ++ "}",
  pQMark       = "?",
  pConjunction = " & ",
  pDisjunction = " | ",
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
  prettyPrec p _ (TyIdentifier x)      = pIdentifier p x
  prettyPrec p _ (TySquare x)          = pSquare p $ prettyPrec p 0 x
  prettyPrec p _ (TyCurly x)           = pCurly p $ prettyPrec p 0 x
  prettyPrec p d (TyVariadic x)        = prettyParens (d > 4) $ prettyPrec p 4 x ++ pEllipsis p
  prettyPrec p d (TyNamed n x)         = prettyParens (d > 3) $ prettyPrec p 3 n ++ pColon p ++ prettyPrec p 4 x
  prettyPrec p d (TyProduct xs)        = prettyParens (d > 2) $ intercalate (pTimes p) $ map (prettyPrec p 2) xs
  prettyPrec p d (TyConjunction xs)    = prettyParens (d > 6) $ intercalate (pConjunction p) $ map (prettyPrec p 6) xs
  prettyPrec p d (TyDisjunction xs)    = prettyParens (d > 5) $ intercalate (pDisjunction p) $ map (prettyPrec p 5) xs
  prettyPrec p d (TyOptional x)        = prettyParens (d > 8) (prettyPrec p 8 x) ++ pQMark p
  prettyPrec p d (TyApplication x ys)  = prettyParens (d > 7) $ intercalate " " $ map (prettyPrec p 8) $ x : ys
  prettyPrec p d (TyFunction x y)      = prettyParens (d > 1) $ prettyPrec p 2 x ++ pTo p ++ prettyPrec p 1 y

-- @(a... -> b)... -> c@
test0 :: Type
test0 = TyFunction (TyVariadic f) c
  where c  = TyIdentifier "c"
        f  = TyFunction (TyVariadic $ TyIdentifier "a") (TyIdentifier "b")

-- @a, ys : b..., c, d -> d -> e@
test1 :: Type
test1 = TyFunction (tyProduct [a, b, c, d]) (TyFunction d e)
  where  a  = TyIdentifier "a"
         b  = TyNamed (TyIdentifier "ys") $ tyVariadic $ TyIdentifier "b"
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
test3 = TyFunction (tyProduct [a, TyNamed (TyIdentifier "ys") b]) c
  where  a  = TyIdentifier "a"
         b  = TyIdentifier "b"
         c  = TyIdentifier "c"

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

openingCurly :: Parser ()
openingCurly = const () <$> lexeme (char '{') <?> "opening curly brace"

closingCurly :: Parser ()
closingCurly = const () <$> lexeme (char '}') <?> "closing curly brace"

braces :: Parser a -> Parser a
braces p = openingBrace *> p <* closingBrace

brackets :: Parser a -> Parser a
brackets p = openingBracket *> p <* closingBracket

curlyBraces :: Parser a -> Parser a
curlyBraces p = openingCurly *> p <* closingCurly

firstNameLetter :: Parser Char
firstNameLetter = oneOf "@$_" <|> letter

restNameLetter :: Parser Char
restNameLetter = oneOf "@$_" <|> alphaNum

nameP :: Parser Name
nameP = lexeme ((:) <$> firstNameLetter <*> many restNameLetter)

numP :: Parser Integer
numP = lexeme (f <$> many1 digit)
  where f = foldl g 0 . map h
        g x y = x * 10 + y
        h c = fromIntegral (fromEnum c - fromEnum '0')

singleStringP :: Parser String
singleStringP = char '\'' *> many (stringCharP '\'') <* char '\''

doubleStringP :: Parser String
doubleStringP = char '"' *> many (stringCharP '"') <* char '"'

readHex' :: String -> Int
readHex' s = case readHex s of
               [(n, "")] -> n
               _         -> 0

stringCharP :: Char -> Parser Char
stringCharP c = escaped <|> others
  where escaped = char '\\' *> ((f <$> oneOf ['\\', 'n', c]) <|> hexescaped)
        f 'n'   = '\n'
        f c'     = c'
        hexescaped = char 'x' *> (g <$> hexDigit <*> hexDigit)
        g x y = toEnum $ readHex' [x, y]
        others  = noneOf ['\\', c]


tyTrueP :: Parser Type
tyTrueP = const TyTrue <$> lexeme (oneOf "*⊤")

tyFalseP :: Parser Type
tyFalseP = const TyFalse <$> lexeme (string "⊥" <|> try (string "_|_"))

tyUnitP :: Parser Type
tyUnitP = const TyUnit <$> lexeme (string "𝟙" <|> try (string "()"))

tyNumberP :: Parser Type
tyNumberP = TyNumber <$> numP

tyStringP :: Parser Type
tyStringP = TyString <$> lexeme (singleStringP <|> doubleStringP)

recordPairP :: Parser (Name, Type)
recordPairP = (,) <$> nameP <* colonP <*> tyFunctionP

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

semiColonP :: Parser ()
semiColonP = const () <$> lexeme (char ';')

tyVariadicP :: Parser Type
tyVariadicP = f <$> tyDisjunctionP <*> optionMaybe ellipsisP
  where f t Nothing  = t
        f t (Just _) = tyVariadic t

tyNamedP :: Parser Type
tyNamedP = f <$> tyVariadicP <*> many (colonP *> tyVariadicP)
  where f a bs = foldl TyNamed a bs

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
  tyNumberP,
  tyStringP,
  tyIdentifierP,
  braces typeParser,
  brackets (TySquare <$> typeParser),
  curlyBraces (TyCurly <$> typeParser)
  ]

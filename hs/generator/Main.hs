module Main where

import Test.QuickCheck
import Data.Aeson
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.ByteString.Lazy as B
import Data.List (sort)
import Control.Applicative
import Language.Typify

limit :: Int
limit = 1000

insertUniq :: Eq a => a -> [a] -> [a]
insertUniq x xs | x `elem` xs = xs
                | otherwise   = x : xs

generateUniq :: Eq a => Gen a -> IO [a]
generateUniq g = generateUniq' []
  where generateUniq' ls
          | length ls >= limit = return ls
          | otherwise        = do
            x <- generate g
            generateUniq' (x `insertUniq` ls)
       
showIndex :: Int -> String
showIndex n = replicate (max 0 $ 3 - length s) '0' ++ s
  where s = show n
 
stringToByteString :: String -> B.ByteString
stringToByteString = E.encodeUtf8 . T.pack

outputType :: Int -> Type -> IO ()
outputType idx ty = B.writeFile filename contents
  where filename = "fixture-" ++ showIndex idx
        contents = B.concat [ stringToByteString $ pretty ty
                           , stringToByteString "\n"
                           , encode ty
                           , stringToByteString "\n"
                           ]

main :: IO ()
main = (zip [0..] . sort <$> generateUniq (arbitrary :: Gen Type)) >>= mapM_ (uncurry outputType)

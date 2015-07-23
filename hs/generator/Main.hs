module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           Data.List (sort)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Random
import System.Random as R

import           Language.Typify

fixtureCount :: Int
fixtureCount = 1000

fixtureMaxSize :: Int
fixtureMaxSize = 100

magicSeed :: Int
magicSeed = 1337

insertUniq :: Eq a => a -> [a] -> [a]
insertUniq x xs | x `elem` xs = xs
                | otherwise   = x : xs

generateUniq :: Eq a => Gen a -> QCGen -> [a]
generateUniq (MkGen g) = go []
  where go ls r
          | length ls >= fixtureCount = ls
          | otherwise                 = do
             let (ra, rb) = R.split r
                 x        = g ra (1 + length ls * fixtureMaxSize `div` fixtureCount)
             go (x `insertUniq` ls) rb

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
main = do
  let values = generateUniq (arbitrary :: Gen Type) (mkQCGen magicSeed)
  mapM_ (uncurry outputType) . zip [0..] . sort $ values

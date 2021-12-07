module Day07 where

import Prelude
import Data.Array (fromFoldable, (!!))
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.String.CodeUnits (fromCharArray)
import Data.Ord (abs)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Async as FS
import Partial.Unsafe (unsafeCrashWith)
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS
import Text.Parsing.Parser.Token as PT

numberParser :: P.Parser String Int
numberParser = do
  PS.skipSpaces
  digits <- PC.many1 PT.digit
  pure $ fromFoldable digits
    # fromCharArray
    # fromString
    # fromMaybe 0

type Input
  = Array Int

inputParser :: P.Parser String Input
inputParser = do
  crabs <- numberParser `PC.sepBy` PS.string ","
  pure
    $ crabs
    # fromFoldable
    # A.sort

parse :: forall t. Either t String -> Input
parse = case _ of
  Left _ -> unsafeCrashWith ":("
  Right i -> case P.runParser i inputParser of
    Left err -> unsafeCrashWith $ P.parseErrorMessage err
    Right o -> o

calcFuelA :: Array Int -> Int -> Int
calcFuelA cs t = cs <#> (sub t >>> abs) # sum

medianOnSorted :: Array Int -> Int
medianOnSorted cs = cs !! ((A.length cs) / 2) # fromMaybe (-1)

calcFuelB :: Array Int -> Int -> Int
calcFuelB cs t = cs <#> (sub t >>> abs >>> (\n -> n * (n + 1) / 2)) # sum

average :: Array Int -> Int
average cs = (sum cs) / (A.length cs)

solve :: Input -> Effect Unit
solve i = do
      log "Day 07"
      log $ show $ medianOnSorted i # calcFuelA i
      log $ show $ average i # calcFuelB i

main :: Effect Unit
main = do
  FS.readTextFile UTF8 "input/d07.txt" (parse >>> solve)

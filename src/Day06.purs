module Day06 where

import Prelude
import Data.Array (fromFoldable, (:), filter, length, (..))
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (sum, minimum, maximum)
import Data.Number (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (fromCharArray)
import Data.HashMap as HM
import Data.Ord (lessThanOrEq, abs)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Async as FS
import Partial.Unsafe (unsafeCrashWith)
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS
import Text.Parsing.Parser.Token as PT
import Data.Tuple (Tuple(..))
import Debug

numberParser :: P.Parser String Number
numberParser = do
  PS.skipSpaces
  digits <- PC.many1 PT.digit
  pure $ fromFoldable digits
    # fromCharArray
    # fromString
    # fromMaybe 0.0

type Input
  = HM.HashMap Number Number

inputParser :: P.Parser String Input
inputParser = do
  fishes <- numberParser `PC.sepBy` PS.string ","
  pure
    $ fishes
    # fromFoldable
    # A.foldr (\k -> HM.upsert (add 1.0) k 1.0) HM.empty

parse :: _ -> Input
parse = case _ of
  Left _ -> unsafeCrashWith ":("
  Right i -> case P.runParser i inputParser of
    Left err -> unsafeCrashWith $ P.parseErrorMessage err
    Right o -> o

step :: Input -> Input
step i =
  let
    stepFish (Tuple a b) = Tuple (a - 1.0) b

    nextUnfiltered =
      i
        # HM.toArrayBy Tuple
        <#> stepFish
        # HM.fromArray

    numNew =
      nextUnfiltered
        # HM.lookup (-1.0)
        # fromMaybe 0.0

    next =
      nextUnfiltered
        # HM.insertWith add 8.0 numNew
        # HM.insertWith add 6.0 numNew
        # HM.filterKeys (\k -> k >= 0.0)
  in
    next

doN :: forall a. Int -> (a -> a) -> a -> a
doN n _ a | n <= 0 = a
doN n f a = doN (n - 1) f a # f

count i = HM.values i # A.foldr add 0.0

solve :: Input -> Effect Unit
solve i =
  let
      after80 = i # doN 80 step
                  # count

      after256 = i # doN 256 step
                  # count
   in do
      log "Day 06"
      log $ show after80
      log $ show after256

main :: Effect Unit
main = do
  FS.readTextFile UTF8 "input/d06.txt" (parse >>> solve)

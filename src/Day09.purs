module Day09 where

import Prelude
import Data.Array (fromFoldable, (!!), (:))
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Map as M
import Data.Set as S
import Data.String.CodeUnits (singleton)
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
import Debug

type Input = M.Map (Tuple Int Int) Int

parseLine :: P.Parser String (Array (Tuple Int Int))
parseLine = do
  digits <- PC.many1 PT.digit
  pure $ fromFoldable digits
    <#> singleton
    <#> fromString
    <#> fromMaybe 0
    # A.mapWithIndex Tuple

inputParser :: P.Parser String Input
inputParser = do
  lines <- (PC.try parseLine) `PC.sepBy` PS.string "\n"
  pure $ lines
      <#> (A.mapWithIndex \y (Tuple x n) -> Tuple (Tuple x y) n)
      # A.fromFoldable
      # A.concat
      # M.fromFoldable

parse :: forall t. Either t String -> Input
parse = case _ of
  Left _ -> unsafeCrashWith ":("
  Right i -> case P.runParser i inputParser of
    Left err -> unsafeCrashWith $ P.parseErrorMessage err
    Right o -> o

solve :: Input -> Effect Unit
solve i =
  let
    _ = spy "_" i
  in
    do
      log "Day 09"

main :: Effect Unit
main = do
  FS.readTextFile UTF8 "input/d09.txt" (parse >>> solve)

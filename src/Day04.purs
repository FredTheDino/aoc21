module Day04 where

import Prelude
import Data.Array (catMaybes, mapWithIndex, slice, (!!), fromFoldable, (:), drop, (..))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (sum, minimum, maximum)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (fromCharArray)
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

repeatParser :: forall p. Int -> P.Parser String p -> P.Parser String (Array p)
repeatParser n parser =
  if n > 0 then do
    r <- parser
    rest <- repeatParser (n - 1) parser
    pure $ r : rest
  else do
    pure []

boardParser :: P.Parser String (Array Int)
boardParser = PC.try $ repeatParser (5 * 5) numberParser

inputParser :: P.Parser String Input
inputParser = do
  numbers <- numberParser `PC.sepBy` PS.string ","
  boards <- PC.many1 boardParser
  pure
    $ { numbers: numbers # fromFoldable
      , boards: boards # fromFoldable
      }

type Input
  = { numbers :: Array Int
    , boards :: Array (Array Int)
    }

parse :: _ -> Input
parse = case _ of
  Left _ -> unsafeCrashWith ":("
  Right i -> case P.runParser i inputParser of
    Left err -> unsafeCrashWith $ P.parseErrorMessage err
    Right o -> o

takeEvery :: Int -> Array Int -> Array Int
takeEvery n =
  let
    multipleOrNothing index value =
      if 0 == mod index n then
        Just value
      else
        Nothing
  in
    mapWithIndex multipleOrNothing >>> catMaybes

allLines :: Array Int -> Array (Array Int)
allLines b =
  let
    sliceN n = slice n5 (n5 + 5) b
      where
      n5 = n * 5

    takeN n = takeEvery 5 (drop n b)
  in
    (0 .. 4 <#> sliceN) <> (0 .. 4 <#> takeN)

getIndex :: Array Int -> Int -> Maybe Int
getIndex l e = Array.elemIndex e l

score :: Input -> Array Int -> Int -> Int
score i boardsBingoTurn turn =
  fromMaybe 0 do
    number <- i.numbers !! turn
    let
      picked = Array.take (turn + 1) i.numbers
    index <- getIndex boardsBingoTurn turn
    board <- i.boards !! index
    let
      boardAfter = board # Array.filter (\e -> Array.notElem e picked)
    pure $ (boardAfter # sum) * number

largest :: Array Int -> Int
largest = maximum >>> fromMaybe 0

smallest :: Array Int -> Int
smallest = minimum >>> fromMaybe 0

-- Too low: 734
solve :: Input -> Effect Unit
solve i =
  let
    boardsBingoTurn :: Array Int
    boardsBingoTurn =
      i.boards
        <#> allLines
        <#> map (Array.mapMaybe $ getIndex i.numbers)
        <#> map largest
        <#> smallest

    winning = score i boardsBingoTurn (smallest boardsBingoTurn)

    loosing = score i boardsBingoTurn (largest boardsBingoTurn)
  in
    do
      log "Day 04"
      log $ show winning
      log $ show loosing

main :: Effect Unit
main = do
  FS.readTextFile UTF8 "input/d04.txt" (parse >>> solve)

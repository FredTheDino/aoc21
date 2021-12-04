module Day04 where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Node.FS.Async as FS
import Node.Encoding (Encoding(UTF8))
import Partial.Unsafe (unsafeCrashWith)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Utils (lines, words, toCharArray)
import Data.String.Common (joinWith)
import Data.Int (fromString)
import Data.Array (catMaybes, mapWithIndex, slice, (!!), fromFoldable, (:), drop)
import Data.Array as Array
import Data.Tuple (fst, snd, Tuple(..))
import Data.String.CodeUnits (fromCharArray)
import Data.Function (identity)
import Data.Foldable (sum)
import Debug (spy)
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS
import Text.Parsing.Parser.Token as PT

number :: P.Parser String Int
number = do
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

board :: P.Parser String (Array Int)
board = PC.try $ repeatParser (5 * 5) number

inputParser :: P.Parser String Input
inputParser = do
  numbers <- number `PC.sepBy` PS.string ","
  boards <- PC.many1 board
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
takeEvery n = mapWithIndex (\i b -> if 0 == mod i n then Just b else Nothing) >>> catMaybes

allLines :: Array Int -> Array (Array Int)
allLines b =
  [ slice 0 5 b
  , slice 5 10 b
  , slice 10 15 b
  , slice 15 20 b
  , slice 20 25 b
  ]
    <> [ takeEvery 5 (drop 0 b)
      , takeEvery 5 (drop 1 b)
      , takeEvery 5 (drop 2 b)
      , takeEvery 5 (drop 3 b)
      , takeEvery 5 (drop 4 b)
      ]

getIndex :: Array Int -> Int -> Maybe Int
getIndex l e = Array.elemIndex e l

score :: Input -> Array Int -> Int -> Int
score i boardsBingoTurn turn =
      let
        number = i.numbers !! turn # fromMaybe 0
        picked = Array.take (turn + 1) i.numbers
        index = getIndex boardsBingoTurn turn # fromMaybe 0
        board = i.boards !! index # fromMaybe []
        boardAfter = board # Array.filter (\e -> Array.notElem e picked)
        totalSum = boardAfter # sum
      in
        totalSum * number

largest = Array.foldl max 0

smallest = Array.foldl min 99999 -- :/

-- Too low: 734
solve :: Input -> Effect _
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

main :: _
main = do
  FS.readTextFile UTF8 "input/d04.txt" (parse >>> solve)

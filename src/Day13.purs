module Day13 where

import Prelude
import Data.Array (fromFoldable, (!!))
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (maximum, sum)
import Data.Int (fromString)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as S
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Async as FS
import Partial.Unsafe (unsafeCrashWith)
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS
import Text.Parsing.Parser.Token as PT

charsToInt = fromFoldable >>> fromCharArray >>> fromString >>> fromMaybe 0

data Fold = X Int | Y Int

parseTuple :: P.Parser String (Tuple Int Int)
parseTuple = do
  a <- PC.many1 PT.digit
  _ <- PS.string ","
  b <- PC.many1 PT.digit
  pure $ Tuple (a # charsToInt) (b # charsToInt)

parseFold :: P.Parser String _
parseFold = do
  _ <- PS.string "fold along "
  f <- PC.choice [ PS.string "x", PS.string "y" ]
  _ <- PS.string "="
  b <- PC.many1 PT.digit
  let i = (b # charsToInt)
  pure $ (case f of
    "x" -> X i
    "y" -> Y i
    _ -> unsafeCrashWith "nonono")

inputParser :: P.Parser String _
inputParser = do
  points <- PC.endBy (PC.try parseTuple) (PS.string "\n")
  _ <- PS.string "\n"
  folds <- PC.endBy (parseFold) (PS.string "\n")
  pure
    $ { points: points # S.fromFoldable, folds: folds # fromFoldable }

parse :: forall t. Either t String -> _
parse = case _ of
  Left _ -> unsafeCrashWith ":("
  Right i -> case P.runParser i inputParser of
    Left err -> unsafeCrashWith $ show err
    Right o -> o

foldNum i x = if x > i then 2 * i - x else x

doFold (X i) = S.map (\(Tuple x y) -> Tuple (foldNum i x) y)
doFold (Y i) = S.map (\(Tuple x y) -> Tuple x (foldNum i y))

printGrid xMax yMax s =
  let
    print at@(Tuple x y) = let
        newline = if x > xMax then "\n" else ""
        next = if x > xMax then Tuple 0 (y + 1) else Tuple (x + 1) y
        end = y > yMax
        c = if S.member at s then "#" else " "
      in
        if end then ""
        else c <> newline <> print next
  in
    print (Tuple 0 0)


solve :: _ -> Effect Unit
solve i =
  let
    numAfterFold = (i.folds !! 0 # fromMaybe (X 0) # doFold) i.points
    folded = A.foldl (\a b -> doFold b a) i.points i.folds

    yMax = folded # fromFoldable <#> (\(Tuple _ y) -> y) # maximum # fromMaybe 0
    xMax = folded # fromFoldable <#> (\(Tuple x _) -> x) # maximum # fromMaybe 0

    grid = printGrid xMax yMax folded
  in
    do
      log "Day 13"
      log $ show $ S.size numAfterFold
      log grid

main :: Effect Unit
main = do
  FS.readTextFile UTF8 "input/d13.txt" (parse >>> solve)

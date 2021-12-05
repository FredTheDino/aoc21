module Day05 where

import Prelude
import Data.Array (fromFoldable, (:), filter, length, (..))
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (sum, minimum, maximum)
import Data.Int (fromString)
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

type Vent
  = { ax :: Int
    , ay :: Int
    , bx :: Int
    , by :: Int
    }

numberParser :: P.Parser String Int
numberParser = do
  PS.skipSpaces
  digits <- PC.many1 PT.digit
  pure $ fromFoldable digits
    # fromCharArray
    # fromString
    # fromMaybe 0

ventParser :: P.Parser String Vent
ventParser =
  PC.try do
    ax <- numberParser
    _ <- PS.string ","
    ay <- numberParser
    _ <- PS.string " -> "
    bx <- numberParser
    _ <- PS.string ","
    by <- numberParser
    pure { ax, ay, bx, by }

type Input
  = Array Vent

inputParser :: P.Parser String Input
inputParser = do
  vents <- PC.many1 ventParser
  pure
    $ vents
    # fromFoldable

parse :: _ -> Input
parse = case _ of
  Left _ -> unsafeCrashWith ":("
  Right i -> case P.runParser i inputParser of
    Left err -> unsafeCrashWith $ P.parseErrorMessage err
    Right o -> o

isOrthogonal :: Vent -> Boolean
isOrthogonal { ax, ay, bx, by } =
  let
    dx = ax - bx

    dy = ay - by
  in
    not $ (dx == 0) == (dy == 0)

allPositions :: Vent -> Array (Tuple Int Int)
allPositions { ax, ay, bx, by } = case dx, dy of
  0, n -> A.zip (A.replicate (n + 1) ax) (ay .. by)
  n, 0 -> A.zip (ax .. bx) (A.replicate (n + 1) ay)
  a, b
    | a == b -> A.zip (ax .. bx) (ay .. by)
  _, _ -> unsafeCrashWith ":<"
  where
  dx = abs $ ax - bx

  dy = abs $ ay - by

incOrOne :: Maybe Int -> Maybe Int
incOrOne = case _ of
  Just a -> Just (a + 1)
  Nothing -> Just 1

numAtLeast2Overlaps vents =
  let
    positions = vents <#> allPositions # A.concat

    pointCount = A.foldr (HM.alter incOrOne) HM.empty positions
  in
    HM.filter (lessThanOrEq 2) pointCount # HM.size

solve :: Input -> Effect Unit
solve i =
  let
    ortho = numAtLeast2Overlaps $ filter isOrthogonal i

    all = numAtLeast2Overlaps i
  in
    do
      log "Day 05"
      log $ show ortho
      log $ show all

main :: Effect Unit
main = do
  FS.readTextFile UTF8 "input/d05.txt" (parse >>> solve)

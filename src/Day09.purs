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
import Data.Foldable (minimum, sum, product)
import Debug

type Input = M.Map (Tuple Int Int) Int

parseLine :: P.Parser String (Array (Tuple Int Int))
parseLine = do
  PS.skipSpaces
  digits <- PC.many1Till PT.digit (PS.string "\n")
  pure $ fromFoldable digits
    <#> singleton
    <#> fromString
    <#> fromMaybe 0
    # A.mapWithIndex Tuple

inputParser :: P.Parser String Input
inputParser = do
  lines <- PC.many1 (PC.try parseLine)
  pure $ lines
    # A.fromFoldable
    -- :thinking:
    # (A.mapWithIndex \y as -> as <#> \(Tuple x n) -> Tuple (Tuple x y) n)
    # A.concat
    # M.fromFoldable

parse :: forall t. Either t String -> Input
parse = case _ of
  Left _ -> unsafeCrashWith ":("
  Right i -> case P.runParser i inputParser of
    Left err -> unsafeCrashWith $ P.parseErrorMessage err
    Right o -> o

adjacenct :: Input -> Tuple Int Int -> Array (Tuple Int Int)
adjacenct field (Tuple x y) =
  [ Tuple (x + 1) y
  , Tuple (x - 1) y
  , Tuple x (y + 1)
  , Tuple x (y - 1)
  ]

neighbors :: Input -> Tuple Int Int -> Array Int
neighbors field pos = adj # A.mapMaybe (\a -> M.lookup a field)
  where
  adj = adjacenct field pos

type Seen = S.Set (Tuple Int Int)

fill :: Input -> Tuple Int Int -> Seen -> Seen
fill field pos seen = case M.lookup pos field, S.member pos seen of
  _, true -> seen
  Just 9, _ -> seen
  Nothing, _ -> seen
  _, _ -> adjacenct field pos # A.foldr (fill field) (S.insert pos seen)

startFill :: Input -> Tuple Int Int -> Seen -> Tuple Seen Int
startFill field start seenPre =
  let
    startSize = S.size seenPre
    seen = fill field start seenPre
    endSize = S.size seen
  in
    Tuple seen (endSize - startSize)

solve :: Input -> Effect Unit
solve i =
  let
    isLowest :: Tuple (Tuple Int Int) Int -> Maybe Int
    isLowest (Tuple pos value) = if value < (minimum adj # fromMaybe 0) then Just value else Nothing
      where
      adj = neighbors i pos

    search pos { seen, res } =
      let
        Tuple seenPost size = startFill i pos seen
      in
        { seen: seenPost, res: A.snoc res size }

    riskLevels = i # M.toUnfoldable
      # A.mapMaybe isLowest
      <#> add 1
      # sum

    res = i # M.keys
      # A.fromFoldable
      # A.foldr search { seen: S.empty, res: [] }
      # _.res
      # A.filter (notEq 0)
      # A.sort
      # A.takeEnd 3
      # product
  in
    do
      log "Day 09"
      log $ show riskLevels
      log $ show res

main :: Effect Unit
main = do
  FS.readTextFile UTF8 "input/d09.txt" (parse >>> solve)

module Day11 where

import Prelude
import Data.Array (fromFoldable, (!!), (:))
import Data.Array as A
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Map as M
import Data.Set as S
import Data.String.CodeUnits (singleton)
import Data.Ord (abs)
import Data.Eq (eq)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Async as FS
import Partial.Unsafe (unsafeCrashWith)
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS
import Text.Parsing.Parser.Token as PT
import Data.Foldable (minimum, sum, product, or)
import Data.List.Lazy as L
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
    # (A.mapWithIndex \y as -> as <#> \(Tuple x n) -> Tuple (Tuple x y) n)
    # A.concat
    # M.fromFoldable

parse :: forall t. Either t String -> Input
parse = case _ of
  Left _ -> unsafeCrashWith ":("
  Right i -> case P.runParser i inputParser of
    Left err -> unsafeCrashWith $ P.parseErrorMessage err
    Right o -> o

inc :: Int -> Int
inc = (+) 1

adjacenct :: Tuple Int Int -> Array (Tuple Int Int)
adjacenct (Tuple x y) =
  [ Tuple (x + 1) y
  , Tuple (x - 1) y
  , Tuple x (y + 1)
  , Tuple x (y - 1)
  , Tuple (x - 1) (y - 1)
  , Tuple (x + 1) (y + 1)
  , Tuple (x - 1) (y + 1)
  , Tuple (x + 1) (y - 1)
  ]

type State = { state :: Input, flashed :: S.Set (Tuple Int Int) }

checkFlash :: State -> Tuple Int Int -> State
checkFlash id@{ state, flashed } pos =
  let
    expensive p = postUpdate
      where
      neighbors = adjacenct p
      postFlash = neighbors
        # A.foldl (\s n -> M.update (inc >>> Just) n s) state
      postUpdate = neighbors
        # A.foldl checkFlash { state: postFlash, flashed: S.insert pos flashed }

    hasFlashed = S.member pos flashed
    notShouldFlash = M.lookup pos state <= Just 9
  in
    if or [ hasFlashed, notShouldFlash ] then id
    else expensive pos

step :: State -> State
step { state } =
  let
    reset n = if n > 9 then 0 else n
    stateInc = M.mapMaybe (inc >>> Just) state
    stateFlash = M.keys stateInc
      # A.fromFoldable
      # A.foldl checkFlash { state: stateInc, flashed: S.empty }
    stateZero = M.mapMaybe (reset >>> Just) stateFlash.state
  in
   { state: stateZero, flashed: stateFlash.flashed }

solve :: Input -> Effect Unit
solve i =
  let
    numNodes = M.size i
    allStates = L.iterate step { state: i, flashed: S.empty }
    first = allStates # L.take (1 + 100) # A.fromFoldable
    second = allStates # L.findIndex (_.flashed >>> S.size >>> (eq numNodes)) # fromMaybe 0
  in
    do
      log "Day 11"
      log $ first <#> _.flashed <#> S.size # sum # show
      log $ show second

main :: Effect Unit
main = do
  FS.readTextFile UTF8 "input/d11.txt" (parse >>> solve)

module Day10 where

import Prelude
import Data.Array (fromFoldable, (!!), (:))
import Data.Array as A
import Data.Either (Either(..), fromLeft, fromRight)
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Tuple (Tuple(..))
import Data.Ord (abs)
import Data.Eq (eq)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Async as FS
import Partial.Unsafe (unsafeCrashWith)
import Data.Foldable (minimum, sum, product)
import Data.String.Common as SC
import Data.String.Pattern as SP
import Data.String as S
import Data.String.Utils as SU
import Data.Function.Memoize (memoize)
import Debug

type Input = Array String

parse :: forall t. Either t String -> Input
parse = case _ of
  Left _ -> unsafeCrashWith ":("
  Right i -> SC.split (SP.Pattern "\n") (SU.trimEnd i)

isClose c = 0 /= score c

isOpen c = openToClose c # isJust

openToClose o = case o of
  "(" -> Just ")"
  "[" -> Just "]"
  "{" -> Just "}"
  "<" -> Just ">"
  _ -> Nothing

score c = case c of
  ")" -> 3
  "]" -> 57
  "}" -> 1197
  ">" -> 25137
  _ -> 0

type At = { head :: String, tail :: String }

match o c = (openToClose o) == (Just c)

eat line = { head: S.take 1 line, tail: S.drop 1 line }

parseLine :: String -> Either String String
parseLine "" = Left ""
parseLine line =
  let
    { head: open, tail: afterOpen } = eat line
  in
    if not $ isOpen open then
      Right line
    else do
      afterMiddle <- parseLine afterOpen
      let { head: close, tail: afterClose } = eat afterMiddle
      if not $ isClose close then
        Right line
      else if not $ match open close then
        Left afterMiddle
      else
        parseLine afterClose

rebuildLine :: String -> Either String String
rebuildLine "" = Left ""
rebuildLine line =
  let
    { head: open, tail: afterOpen } = eat line
  in
    if not $ isOpen open then
      Right line
    else do
      afterMiddle <- case rebuildLine afterOpen of
        Left s -> Left $ s <> (openToClose open # fromMaybe "")
        Right s -> Right s
      let { head: close, tail: afterClose } = eat afterMiddle
      if not $ isClose close then
        Right line
      else if close == "" then
        Left (openToClose open # fromMaybe "")
      else
        rebuildLine afterClose

scoreRebuild = A.foldl
  ( \s c -> s * 5.0 + case c of
      ")" -> 1.0
      "]" -> 2.0
      "}" -> 3.0
      ">" -> 4.0
      _ -> 0.0
  )
  0.0

solve :: Input -> Effect Unit
solve i =
  let
    incomplete = i # A.filter (parseLine >>> fromLeft "" >>> eq "")
    rebuiltScores = incomplete <#>
      ( rebuildLine
          >>> fromLeft ""
          >>> SU.toCharArray
          >>> scoreRebuild
      )
    middle = (rebuiltScores # A.sort) !! ((A.length rebuiltScores) / 2)
  in
    do
      log "Day 10"
      log $ show $ i <#> parseLine <#> fromLeft "" <#> S.take 1 <#> score # sum
      log $ show $ middle # fromMaybe 0.0

-- 228508708 - to low

main :: Effect Unit
main = do
  FS.readTextFile UTF8 "input/d10.txt" (parse >>> solve)

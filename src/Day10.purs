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
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Async as FS
import Partial.Unsafe (unsafeCrashWith)
import Data.Foldable (minimum, sum, product)
import Data.String.Common as SC
import Data.String.Pattern as SP
import Data.String as S
import Data.Function.Memoize (memoize)
import Debug

type Input = Array String

parse :: forall t. Either t String -> Input
parse = case _ of
  Left _ -> unsafeCrashWith ":("
  Right i -> SC.split (SP.Pattern "\n") i

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

parseLine :: String -> String
parseLine "" = ""
parseLine line =
  let
    { head: open, tail: afterOpen } = eat line

    inner afterOpen =
      let
        afterMiddle = parseLine afterOpen
        { head: close, tail: afterClose } = eat afterMiddle
      in
        if not $ isClose close then
          afterOpen
        else if not $ match open close then
          afterMiddle
        else
          parseLine afterClose
  in
    if not $ isOpen open then line
    else inner afterOpen

solve :: Input -> Effect Unit
solve i =
  do
    log "Day 10"
    log $ show $ i <#> parseLine <#> S.take 1 <#> spy "error" <#> score # sum

main :: Effect Unit
main = do
  FS.readTextFile UTF8 "input/d10.txt" (parse >>> solve)

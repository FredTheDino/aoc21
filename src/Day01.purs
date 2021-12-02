module Day01 where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Node.FS.Async as FS
import Node.Encoding (Encoding(UTF8))
import Partial.Unsafe (unsafeCrashWith)
import Data.String.Utils (lines)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Data.Array (mapMaybe, zipWith, drop, foldMap, filter, length)
import Data.Tuple (fst, snd)

parse :: _
parse = case _ of
  Left _ -> unsafeCrashWith "Failed to read"
  Right buff -> lines buff # mapMaybe fromString

solve :: Array Int -> Effect Unit
solve i =
  let
    linesZipped = zipWith (\a b -> { a, b }) i (drop 1 i)

    numInc = (filter (\t -> t.a < t.b) linesZipped) # length

    lineWindows = zipWith (\a c -> a.a + a.b + c) linesZipped (drop 2 i)

    incWindows = zipWith (\a b -> a < b) lineWindows (drop 1 lineWindows)

    numIncWin = (filter (\t -> t) incWindows) # length
  in
    do
      log "Day 01"
      log (show numInc)
      log (show numIncWin)

main :: _
main = do
  FS.readTextFile UTF8 "inputd01.txt" (parse >>> solve)

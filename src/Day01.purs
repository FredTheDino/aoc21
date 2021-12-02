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
import Data.Array (mapMaybe, zipWith, drop, filter, length)

parse :: forall t. Either t String -> Array Int
parse = case _ of
  Right buff -> lines buff # mapMaybe fromString
  _ -> unsafeCrashWith "Failed to read"

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

main :: Effect Unit
main = do
  FS.readTextFile UTF8 "input/d01.txt" (parse >>> solve)

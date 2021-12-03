module Day03 where

import Prelude
import Effect.Console (log)
import Data.Either (Either(..))
import Node.FS.Async as FS
import Node.Encoding (Encoding(UTF8))
import Partial.Unsafe (unsafeCrashWith)
import Data.Maybe (Maybe(..))
import Data.String.Utils (lines, words, toCharArray)
import Data.Int (fromString)
import Data.Array (mapMaybe, foldl, drop, (!!), zip, filter, null, length)
import Data.Tuple (fst, snd, Tuple(..))
import Data.Function (identity)
import Debug (spy)

reduce :: (Array Int -> Array Int -> Array Int) -> Array (Array Int) -> Array Int
reduce f xs = case xs !! 0 of
                (Just x)
                -> foldl f x (drop 1 xs)
                Nothing -> unsafeCrashWith ":("

sumComp :: Array Int -> Array Int -> Array Int
sumComp a b = do
            Tuple a b <- zip a b
            pure (a + b)

mostCommon :: Int -> Array (Array Int) -> Array (Array Int)
mostCommon n xs =
  let
      total = length xs
      counts = reduce sumComp xs
      want = case counts !! n of
                 Just count -> count >= (total - count)
                 Nothing -> unsafeCrashWith ":<"
   in
      if total == 1 then
        xs
      else
        filter (\a -> ((a !! n) == Just 1) == want) xs

leastCommon :: Int -> Array (Array Int) -> Array (Array Int)
leastCommon n xs =
  let
      total = length xs
      counts = reduce sumComp xs
      want = case counts !! n of
                 Just count -> not (count >= (total - count))
                 Nothing -> unsafeCrashWith ":<"
   in
      if total == 1 then
        xs
      else
        filter (\a -> ((a !! n) == Just 1) == want) xs


untilZero :: forall a. (Int -> a -> a) -> Int -> (a -> a)
untilZero f 0 = f 0
untilZero f n = untilZero f (n - 1) >>> f n


parseBin :: Array Boolean -> Int
parseBin = foldl (\i s -> 2 * i + if s then 1 else 0) 0

parse :: _
parse =
  let
    parseLine :: String -> Array Int
    parseLine line = do
      c <- toCharArray line
      if c == "1" then pure 1 else pure 0
  in
    case _ of
      Right buff -> lines buff # map parseLine # filter (not null)
      _ -> unsafeCrashWith "Failed to read"

solve :: _
solve i =
  let
    total = spy "length" $ length i
    counts = spy "counts" $ reduce sumComp i

    gamma = counts <#> (\a -> a >= (total - a))
    epsilon = gamma <#> not

    oxygen = case (untilZero mostCommon 11 i) !! 0 of
                 Just x -> x <#> eq 1
                 Nothing -> unsafeCrashWith ":["
    scrubber = case (untilZero leastCommon 11 i) !! 0 of
                 Just x -> x <#> eq 1
                 Nothing -> unsafeCrashWith ":["
  in
    do
      log "Day 03"
      log $ show $ (parseBin gamma) * (parseBin epsilon)
      log $ show $ (parseBin oxygen) * (parseBin scrubber)

main :: _
main = do
  FS.readTextFile UTF8 "input/d03.txt" (parse >>> solve)

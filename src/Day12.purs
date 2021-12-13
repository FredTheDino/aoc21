module Day12 where

import Prelude
import Data.Array (fromFoldable, (!!), (:))
import Data.Array as A
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
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
import Data.String.Pattern as SP
import Data.String.Common as SC
import Data.String.Utils as SU
import Data.String.Regex as R
import Data.String.Regex.Flags as RF
import Data.Foldable (minimum, sum, product, or)
import Data.List.Lazy as L
import Debug

-- type Input = _ -- M.Map String (S.Set String)

assumeTwo [ a, b ] = Tuple a b
assumeTwo q = unsafeCrashWith ":/" { q }

parse :: forall t. Either t String -> _
parse = case _ of
  Left _ -> unsafeCrashWith ":("
  Right i ->
    let
      pairs = i # SU.trimEnd
        # SC.split (SP.Pattern "\n")
        <#> SC.split (SP.Pattern "-")

      maps = pairs
        <#> assumeTwo
        <#> \(Tuple a b) -> M.fromFoldable [ (Tuple a (S.singleton b)), (Tuple b (S.singleton a)) ]

      res = maps # A.foldr (M.unionWith S.union) M.empty
    in
      res

isBig =
  let
    r = case R.regex "[A-Z]" RF.noFlags of
      Left _ -> unsafeCrashWith ":<<<"
      Right x -> x
  in
    R.test r

countAllPaths :: { graph :: M.Map String (S.Set String), visited :: S.Set String } -> String -> Int
countAllPaths { graph, visited } at =
  let
    done = S.member at visited
    search at =
      let
        newVisited =
          if isBig at then visited
          else S.insert at visited
        neighbors = M.lookup at graph # fromMaybe S.empty # A.fromFoldable

      in
       neighbors <#> countAllPaths { graph: graph, visited: newVisited } # sum
  in
    case at, done of
      "end", _ -> 1
      _, true -> 0
      _, _ -> search at

findAllPathsOneDouble
  :: { graph :: M.Map String (S.Set String), visited :: S.Set String, double :: Maybe String }
  -> String
  -> Array (Array String)
findAllPathsOneDouble { graph, visited, double } at =
  let
    done = S.member at visited
    search at =
      let
        newVisited =
          if isBig at then visited
          else S.insert at visited
        neighbors = M.lookup at graph # fromMaybe S.empty # A.fromFoldable
        subPaths =
          if not done then
            neighbors <#> findAllPathsOneDouble { graph: graph, visited: newVisited, double: double } # A.concat
          else []
        subPathsDouble =
          if (double # isNothing) && (at /= "start") then
            neighbors <#> findAllPathsOneDouble { graph: graph, visited: newVisited, double: Just at } # A.concat
          else []
      in
        [ subPaths, subPathsDouble ] # A.concat <#> A.cons at
  in
    case at, done of
      "end", _ -> [ [ at ] ]
      _, _ -> search at

solve :: _ -> Effect Unit
solve i =
  let
    one = countAllPaths { graph: i, visited: S.empty } "start"
    two = (findAllPathsOneDouble { graph: i, visited: S.empty, double: Nothing } "start") <#> A.fold # S.fromFoldable
  in
    do
      log "Day 12"
      log $ show $ one
      log $ show $ two # S.size

main :: Effect Unit
main = do
  FS.readTextFile UTF8 "input/d12.txt" (parse >>> solve)

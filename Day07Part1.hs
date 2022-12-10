{- stack script
 --resolver lts-20.3
 --package mtl,containers
 --ghc-options -Wall
-}
{-# LANGUAGE LambdaCase #-}
module Day07Part1 where

-- Day 7: No Space Left On Device - Part 1

import           Control.Monad.State.Strict
import           Data.Foldable
import           Data.List
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as Seq

type FS = Map (Seq String) Integer
type Pwd = Seq String
type FSMonad = State (Pwd, FS)

goUp :: FSMonad ()
goUp = get >>= \case
    (Seq.Empty    , _ ) -> error "Cannot go up past root"
    (pwd Seq.:|> _, fs) -> put (pwd, fs)

enter :: String -> FSMonad ()
enter dir = state (\(pwd, fs) -> ((), (pwd Seq.|> dir, fs)))

addDir :: String -> FSMonad ()
addDir name =
    state (\(pwd, fs) -> ((), (pwd, Map.insert (pwd Seq.:|> name) 0 fs)))

addFile :: Integer -> FSMonad ()
addFile filesize =
    state (\(pwd, fs) -> ((), (pwd, Map.adjust (+ filesize) pwd fs)))

interpret :: [String] -> FSMonad ()
interpret []            = pure ()
interpret (line : rest) = case words line of
    ["$", "cd", dir] -> if dir == ".."
        then do
            goUp
            interpret rest
        else do
            enter dir
            interpret rest
    ["$"  , "ls"] -> interpret rest
    ["dir", dir ] -> do
        addDir dir
        interpret rest
    [filesizeStr, _name] -> do
        addFile (read filesizeStr)
        interpret rest
    err -> error $ "Could not parse line: " <> unwords err

initState :: (Pwd, FS)
initState = (Seq.empty, Map.fromList [(Seq.singleton "/", 0)])

totalSizes :: FS -> Map [String] Integer
totalSizes dirSizes = go (Map.toAscList $ Map.mapKeys toList dirSizes)
                         Map.empty
  where
    go [] totals = totals
    go ((dir, siz) : rest) totals =
        let subDirsSize =
                sum . map snd $ filter (\(d, _) -> dir `isPrefixOf` d) rest
        in  go rest (Map.insert dir (siz + subDirsSize) totals)


main :: IO ()
main = do
    inputLines <- lines <$> getContents
    let (_, fs)      = execState (interpret inputLines) initState
        totals       = totalSizes fs
        sumOfTargets = sum $ filter (<= 100000) $ Map.elems totals
    print sumOfTargets

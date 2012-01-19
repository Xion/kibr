module Main where

import Preamble

import Control.Exception  (bracket)
import Data.Acid          (openLocalState)
import Data.Acid.Local    (createCheckpointAndClose)
import Data.Kibr.State
import System.Environment (getArgs)
import System.Exit        (exitFailure)
import System.IO          (putStrLn)

import qualified Data.IxSet        as Ix
import qualified Network.Kibr.Http as Http
import qualified Network.Kibr.Irc  as Irc
import qualified Test.Kibr         as Test
import qualified Text.Kibr.Xml     as Xml

main :: IO ()
main = run =<< getArgs

run :: [String] -> IO ()
run ("http":args)   = withState $ Http.run args
run ("irc":args)    = withState $ Irc.run args
run ("import":args) = withState $ Xml.run args
run ("test":args)   = Test.run args
run _               =
  do
    putStrLn "usage: kibr <http|irc|import|test> [args]"
    exitFailure

withState :: (Acid -> IO a) -> IO a
withState = bracket (openLocalState $ State Ix.empty) createCheckpointAndClose

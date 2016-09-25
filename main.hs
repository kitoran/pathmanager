{-# Language OverloadedStrings, NoMonomorphismRestriction #-}
module Main where

import qualified Text.ShellEscape 
import Data.ByteString.UTF8 --(hPutStrLn)
import Data.ByteString ( hPutStrLn )
import Data.Monoid
import Data.Set
--import Data.Text
import System.Console.Argument
import System.Console.Command
import System.Console.Program
import System.IO (withFile, IOMode(AppendMode))
import System.FilePath.Posix

import Debug.Hood.Observe
import Debug.Trace

data PathVar = Set FilePath

escape = (Text.ShellEscape.bytes::Text.ShellEscape.Bash -> ByteString) . Text.ShellEscape.escape


addAction1 :: FilePath -> FilePath -> Action IO
addAction1 target path = io $ withFile (observe "bashrc"  target) AppendMode (\handle ->
    hPutStrLn handle (traceId (toString a) `seq` a)) where a = ("export PATH=$PATH:" <> escape (fromString path))

addAction = withOption (option ['f'] ["file"] file (trace "default" "~/.bashrc") "File to add command to, default is ~/.bashrc") (\target -> withNonOption 
      directory
      (\path -> addAction1 target path))

addCommand = Command "add" 
                     "\"add p\" adds line \"export PATH=$PATH:p\" to ~/.bashrc file (or other specified file)" 
                     addAction
                     True

emptyAction = Command "" "" (io (return ())) True

main = single (Node emptyAction [Node addCommand []])
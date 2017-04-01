{-# Language OverloadedStrings, NoMonomorphismRestriction #-}
module Main where

import qualified Text.ShellEscape 
import Data.ByteString.UTF8 as  BS --(hPutStrLn)
import Data.ByteString as BS ( hPutStrLn{-deprecated but i cant get rid of it-}, readFile, intercalate, writeFile )
import Data.Monoid
--import Data.Text
import System.Console.Argument
import System.Console.Command
import System.Console.Program
import System.Directory
import System.IO (withFile, IOMode(AppendMode))
import System.FilePath.Posix

import Debug.Hood.Observe
import Debug.Trace

data PathVar = Set FilePath

escape = (Text.ShellEscape.bytes::Text.ShellEscape.Bash -> ByteString) . Text.ShellEscape.escape

formLine :: FilePath -> BS.ByteString
formLine path = ("export PATH=$PATH:" <> escape (fromString path) <> "  # line is managed by pathmanager")

addAction1 :: FilePath -> FilePath -> Action IO
addAction1 target path = io $ 
    withFile (observe "bashrc"  target) AppendMode (\handle ->
       hPutStrLn handle (traceId (toString a) `seq` a)) where a = "\n" <> formLine path <> "\n"

addAction homePath = withOption (option ['f'] ["file"] file (trace "default" (homePath++"/.bashrc")) "File to add command to, default is ~/.bashrc") (\target -> withNonOption 
      directory
      (\path -> addAction1 target path))

addCommand homePath  = Command "add"
                               "\"add p\" adds line \"export PATH=$PATH:p\" to ~/.bashrc file (or other specified file)" 
                               (addAction homePath )
                               True

removeAction1 :: FilePath -> FilePath -> Action IO
removeAction1 target path = io $ do
    linesOfFile <- fmap (BS.lines::ByteString -> [ByteString]) $ BS.readFile target 
    BS.writeFile target $ BS.intercalate "\n" ( filter (/= (formLine path)) (linesOfFile::[BS.ByteString])) <> "\n"

removeAction homePath = withOption (option ['f'] ["file"] file (trace "default" (homePath++"/.bashrc")) "File to remove command from, default is ~/.bashrc") (\target -> withNonOption 
      directory
      (\path -> removeAction1 target path))

removeCommand homePath  = Command "remove"
                               "\"remove p\" removes line \"export PATH=$PATH:p\" from ~/.bashrc file (or other specified file)" 
                               (removeAction homePath )
                               True



emptyAction = Command "" "" (io (return ())) True

main = do
    homePath <- getHomeDirectory
    single (Node emptyAction [Node (addCommand homePath) [], Node (removeCommand homePath) []])
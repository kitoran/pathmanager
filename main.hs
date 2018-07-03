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

formLine :: ByteString -> FilePath -> BS.ByteString
formLine v path = ("export "<>v<>"=$"<>v<>":" <> escape (fromString path) <> "  # line is managed by pathmanager")

addAction1 :: FilePath -> String -> FilePath -> Action IO
addAction1 target v path = io $ 
    withFile (observe "bashrc"  target) AppendMode (\handle ->
       hPutStrLn handle (traceId (toString a) `seq` a)) where a = "\n" <> formLine (fromString v) path <> "\n"

addAction homePath = withOption (option ['v'] 
                                        ["variable"] 
                                        string 
                                        "PATH" 
                                        "Variable to add string (path) to, default is PATH") 
                                (\var -> withOption (option ['f']
                                                            ["file"] 
                                                            file 
                                                            (trace "default" 
                                                                   (homePath++"/.bashrc")) 
                                                            "File to add command to, default is ~/.bashrc") 
                                                    (\target -> withNonOption directory
                                                                              (\path -> addAction1 target var path)))

addCommand homePath  = Command "add"
                               "\"add p\" adds line \"export PATH=$PATH:p\" to ~/.bashrc file (or other specified file)" 
                               (addAction homePath )
                               True

removeAction1 :: FilePath -> FilePath -> Action IO
removeAction1 target path = io $ do
    linesOfFile <- fmap (BS.lines::ByteString -> [ByteString]) $ BS.readFile target 
    BS.writeFile target $ BS.intercalate "\n" ( filter (/= (formLine "PATH" path)) (linesOfFile::[BS.ByteString])) <> "\n"

removeAction homePath = withOption (option ['f'] ["file"] file (trace "default" (homePath++"/.bashrc")) "File to remove command from, default is ~/.bashrc") (\target -> withNonOption 
      directory
      (\path -> removeAction1 target path))

removeCommand homePath  = Command "remove"
                               "\"remove p\" removes line \"export PATH=$PATH:p\" from ~/.bashrc file (or other specified file)" 
                               (removeAction homePath )
                               True



main = do
    homePath <- getHomeDirectory
    let emptyAction = withOption (option ['h']
                                 ["help"]
                                 (optional undefined string)
                                 undefined
                                 "shows this usage text, argument is ignored")
                                 (\_ -> io $ showUsage commands)

        commands = (Node (Command "" "" emptyAction True) [Node (addCommand homePath) [], Node (removeCommand homePath) []]) 
    single commands
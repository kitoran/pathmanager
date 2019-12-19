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

formLine :: ByteString -> FilePath -> Bool -> BS.ByteString
formLine v path begin 
  | begin = ("export "<>v<>"="<>escape (fromString path)<>":" <> "$" <> v<> "  # line is managed by pathmanager")
  | not begin = ("export "<>v<>"=$"<>v<>":" <> escape (fromString path) <> "  # line is managed by pathmanager")

addAction1 :: FilePath -> String -> FilePath -> Bool -> Bool -> Action IO
addAction1 target v path begin resolve = io $ do
    resultPath <- if resolve then
                    makeAbsolute path else
                    return path  
    let a = "\n" <> formLine (fromString v) resultPath begin <> "\n"
    withFile (observe "bashrc"  target) AppendMode (\handle ->
       hPutStrLn handle (traceId (toString a) `seq` a)) 

-- addAction :: FilePath -> Action IO
addAction homePath = withOption (option ['b']
                                        ["beginning"]
                                        boolean
                                        False
                                        "Add value to the beginning of a variable, like VAR=value:$VAR")
                                        (\begin -> 
                     withOption (option ['v'] 
                                        ["variable"] 
                                        string 
                                        "PATH" 
                                        "Variable to add string (path) to, default is PATH") 
                                        (\var -> 
                     withOption (option ['f']
                                        ["file"] 
                                        file 
                                        (trace "default" 
                                               (homePath++"/.bashrc")) 
                                        "File to add command to, default is ~/.bashrc") 
                                        (\target -> 
                     withOption (option ['r']
                                        ["resolve relative path"] 
                                        boolean 
                                        False
                                        "resolve relative path, off by default") 
                                        (\resolve -> 
                     withNonOption directory
                                  (\path -> 
                     addAction1 target var path begin resolve )))))

addCommand homePath = Command "add"
                              "\"add p\" adds line \"export PATH=$PATH:p\" to ~/.bashrc file (or other specified file)" 
                              (addAction homePath) 
                              True

removeAction1 :: FilePath -> FilePath -> Action IO
removeAction1 target path = io $ do
    linesOfFile <- fmap (BS.lines::ByteString -> [ByteString]) $ BS.readFile target 
    BS.writeFile target $ BS.intercalate "\n" ( filter (\s -> (s /= formLine "PATH" path True) && (s /= formLine "PATH" path False)) (linesOfFile::[BS.ByteString])) <> "\n"

removeAction homePath = withOption (option ['f'] ["file"] file (trace "default" (homePath++"/.bashrc")) "File to remove command from, default is ~/.bashrc") (\target -> withNonOption 
      directory
      (\path -> removeAction1 target path))

removeCommand homePath  = Command "remove"
                               "\"remove p\" removes line \"export PATH=$PATH:p\" or \"export PATH=p:$PATH\" from ~/.bashrc file (or other specified file)" 
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

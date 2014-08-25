{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}

import           Control.Applicative
import           Control.Monad
import           Data.SafeCopy
import           Data.Serialize.Get (runGet)
import           Data.Serialize.Put (runPut)
import           GHC.Generics
import           System.Environment
import           System.Exit
import           System.IO
import qualified Data.ByteString as BS

type Task = String
data PilotState = PilotState
  { tasks :: [Task]
  } deriving (Eq, Ord, Show, Generic)

deriveSafeCopy 1 'base ''PilotState


main :: IO ()
main = do 
  args <- getArgs
  case args of 
    [] -> putStrLn strUsageError
    "list":_ -> listMode
    "slot":_ -> slotMode
    "view":_ -> viewMode
    _          -> hPutStrLn stderr strUsageError >> exitFailure

strUsageError :: String
strUsageError = "Usage: ..."

listMode :: IO ()
listMode = do 
  str <- ask "Please enter a task:" 
  ps <- loadStateFile 
  writeStateFile $ ps
    { tasks = str : tasks ps 
    } 


slotMode :: IO ()
slotMode = undefined

viewMode :: IO ()
viewMode = do
  ps <- loadStateFile
  print ps 

filepath :: FilePath
filepath = "pilotstate.bin"

writeStateFile :: PilotState -> IO ()
writeStateFile = BS.writeFile filepath . runPut . safePut 

loadStateFile :: IO PilotState
loadStateFile = do 
  content <- runGet safeGet <$> BS.readFile filepath
  case content of 
    Left err -> hPutStrLn stderr ("Error loading " ++ filepath ++ ": " ++ err) >> exitFailure
    Right ps -> return ps

ask :: String -> IO String
ask request = do  
  unless (null request) $ putStrLn request
  str <- getLine
  if null str
    then ask request
    else return str 

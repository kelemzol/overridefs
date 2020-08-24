{-# LANGUAGE DataKinds
           , RecordWildCards
           , ViewPatterns
           #-}

module Main where


import System.Directory.Tree
import System.Directory
import System.Environment

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except

import Options.Declarative

data Layer = Layer FilePath
  deriving (Eq, Read, Show)

data OverrideFS
  = OverrideFS
    { oId :: String
    , destination :: FilePath
    , backup :: FilePath
    , layers :: [Layer]
    }
  deriving (Eq, Read, Show)

data Config
  = Config
    { overrideFSs :: [OverrideFS]
    }
  deriving (Eq, Read, Show)

type CmdMonad a =  ExceptT String IO a

defaultConfig :: Config
defaultConfig = Config []

newCmd :: Arg "ID" String
       -> Arg "PATH" String
       -> Arg "BACKUP-PATH" String
       -> Cmd "Create new Override FS" ()
newCmd (get -> oId') (get -> destination') (get -> backup') = do
    config <- readConfig
    liftIO $ runCmdMonad $ do
      checkNewParams config oId' destination' backup'
      checkNewPaths destination' backup'

runCmdMonad :: CmdMonad () -> IO ()
runCmdMonad m = do
    e <- runExceptT m
    case e of
      Right r -> return ()
      Left err -> putStrLn $ "error: " ++ err

checkNewParams :: Config -> FilePath -> FilePath -> FilePath -> CmdMonad ()
checkNewParams Config{..} oId' destination' backup' = do
    when (elem oId' (map oId overrideFSs)) $ throwE ("OverrideFS id : " ++ oId' ++ " is already exists")
    when (elem destination' (map destination overrideFSs)) $ throwE ("The path : " ++ destination' ++ " as OverrideFS destination is already exists")
    when (elem backup' (map backup overrideFSs)) $ throwE ("The path : " ++ backup' ++ " as OverrideFS backup-path is already exists")

checkNewPaths :: FilePath -> FilePath -> CmdMonad ()
checkNewPaths destination' backup' = do
    existDestination <- liftIO $ doesDirectoryExist destination'
    when (not existDestination) $ throwE ("The path : " ++ destination' ++ " not exists")
    existBackup <- liftIO $ doesDirectoryExist backup'
    when (not existBackup) $ throwE ("The path : " ++ backup' ++ " not exists")

main :: IO ()
main = run_ $
    Group "Test program for library"
    [ subCmd "new" newCmd
    ]


readConfig :: (MonadIO m) => m Config
readConfig = liftIO $ do
    home <- getHomeDirectory
    let confFile = home ++ "\\.overridefs.config"
    existConf <- doesFileExist confFile
    if existConf
      then read <$> readFile confFile
      else return defaultConfig

writeConfig :: Config -> IO ()
writeConfig conf = do
    home <- getHomeDirectory
    let confFile = home ++ "\\.overridefs.config"
    writeFile confFile (show conf)
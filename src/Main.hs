{-# LANGUAGE DataKinds
           , RecordWildCards
           , ViewPatterns
           , TupleSections
           #-}

module Main where


import System.Directory.Tree
import System.Directory
import System.Environment
import System.Exit

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except

import Data.List
import Data.Maybe
import Data.Traversable
import Data.Foldable

import Options.Declarative

-- Representation
-- ----------------------------------------------------------------------------

data Layer = 
  Layer
    { layerFilePath :: FilePath
    }
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

defaultConfig :: Config
defaultConfig = Config []


-- CmdMonad
-- ----------------------------------------------------------------------------

type CmdMonad a =  ExceptT String IO a

runCmdMonad :: (MonadIO m) => CmdMonad () -> m ()
runCmdMonad m = liftIO $ do
    e <- runExceptT m
    case e of
      Right r -> exitSuccess
      Left err -> die $ "error: " ++ err

checkMaybe :: Maybe a -> CmdMonad () -> (a -> CmdMonad ()) -> CmdMonad ()
checkMaybe m n j = maybe n j m

putString :: String -> CmdMonad ()
putString = liftIO . putStrLn

makeAbsolutes :: [FilePath] -> CmdMonad [FilePath]
makeAbsolutes = liftIO . mapM makeAbsolute . map cutEnd
  where
    cutEnd :: FilePath -> FilePath
    cutEnd fp = let l = last fp in if l == '\\' || l == '/' then init fp else fp

getFiles :: FilePath -> CmdMonad [FilePath]
getFiles fp = do
    (a :/ dtree) <- liftIO $ build fp
    let cutPrefix = ("@"++) . (drop (length fp))
    return (map cutPrefix $ toList dtree)

getFiles' :: FilePath -> CmdMonad (FilePath, [FilePath])
getFiles' fp = (fp,) <$> (getFiles fp)

putScreen :: Screen -> CmdMonad ()
putScreen = mapM_ putTop
  where
    putTop Top{..} = putString $ "[" ++ topLayer ++ "] " ++ topTarget

copyFile' :: FilePath -> FilePath -> CmdMonad ()
copyFile' src dst = do
    createDirectoryIfMissing

-- Screen
-- ----------------------------------------------------------------------------

type Screen = [Top]

emptyScreen :: Screen
emptyScreen = []

data Top
  = Top
    { topLayer :: FilePath
    , topTarget :: FilePath
    }
  deriving (Eq, Read, Show)

mergeLayer :: Screen -> FilePath -> [FilePath] -> Screen
mergeLayer sc l [] = sc
mergeLayer sc l (fp:fps) = mergeLayer (merge sc l fp) l fps
  where
    merge [] l fp = [Top l fp]
    merge (t:sc) l fp = if topTarget t == fp then (Top l fp):sc else t:(merge sc l fp)

mergeLayers :: Screen -> [(FilePath, [FilePath])] -> Screen
mergeLayers sc [] = sc
mergeLayers sc ((l,fps):ls) = mergeLayers (mergeLayer sc l fps) ls

getScreen :: OverrideFS -> CmdMonad Screen
getScreen overridefs = do
    originalFiles <- ("",) <$> (getFiles (destination overridefs))
    layers' <- forM (map layerFilePath $ layers $ overridefs) getFiles'
    let layersWithOriginalFiles = originalFiles:layers'
        screen = mergeLayers emptyScreen layersWithOriginalFiles
    return screen


e111 = "l1"
e112 = ["1","2"]
e121 = "l2"
e122 = ["1","3"]

e1 = mergeLayers [] [(e111,e112),(e121,e122)]

-- COMMAND: new
-- ----------------------------------------------------------------------------

newCmd :: Arg "OID" String
       -> Arg "DESTINATION" String
       -> Arg "BACKUP-PATH" String
       -> Cmd "Create new Override FS" ()
newCmd (get -> oId') (get -> destination') (get -> backup') = runCmdMonad $ do
    config <- readConfig
    [absoluteDestination, absoluteBackup] <- makeAbsolutes [destination', backup']
    checkNewParams config oId' absoluteDestination absoluteBackup
    checkNewPaths absoluteDestination absoluteBackup
    let config' = Config { overrideFSs = overrideFSs config ++ [OverrideFS {oId = oId', destination = absoluteDestination, backup = absoluteBackup, layers = []}] }
    writeConfig config'

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

-- COMMAND: add-layer
-- ----------------------------------------------------------------------------

addLayerCmd :: Arg "OID" String
            -> Arg "LAYER-PATH" String
            -> Cmd "Add a new layer to an Override FS" ()
addLayerCmd (get -> oId') (get -> layerPath) = runCmdMonad $ do
    config <- readConfig
    [absoluteLayerPath] <- makeAbsolutes [layerPath]
    checkAddLayerParams config oId' absoluteLayerPath
    checkAddLayerPath absoluteLayerPath
    let config' = Config { overrideFSs = fmap ( \ o@OverrideFS{..} -> if oId == oId' then o{layers = layers ++ [Layer absoluteLayerPath]} else o ) (overrideFSs config) }
    writeConfig config'

checkAddLayerParams :: Config -> FilePath -> FilePath -> CmdMonad ()
checkAddLayerParams Config{..} oId' layerPath = do
    checkMaybe (find ((oId'==) . oId) overrideFSs)
               (throwE ("OverrideFS with id: " ++ oId' ++ " is not exists"))
               $ \ OverrideFS{..} -> do
                   when (elem layerPath (map layerFilePath layers)) $ throwE ("OverrideFS with id: " ++ oId' ++ " is already contain the layer: " ++ layerPath)

checkAddLayerPath :: FilePath -> CmdMonad ()
checkAddLayerPath layerPath = do
    existLayerPath <- liftIO $ doesDirectoryExist layerPath
    when (not existLayerPath) $ throwE ("The path : " ++ layerPath ++ " not exists")

-- COMMAND: show-all
-- ----------------------------------------------------------------------------

showAllCmd :: Cmd "Show all OverrideFS and Layers" ()
showAllCmd = runCmdMonad $ do
    config <- readConfig
    forM_ (overrideFSs config) $ putOverrideFs

putOverrideFs :: OverrideFS -> CmdMonad ()
putOverrideFs OverrideFS{..} = do
    putString $ " - " ++ oId
    putString $ "    destination: " ++ destination
    putString $ "    backup:      " ++ backup
    putString $ "    layers:"
    forM_ layers $ \ Layer{..} -> putString $ "      " ++ layerFilePath

-- COMMAND: show-overridefs
-- ----------------------------------------------------------------------------

showOverrideFsCmd :: Arg "OID" String
                  -> Flag "" '["show-layered-files"] "" "Show layered files about the Override FS" Bool
                  -> Flag "" '["show-all-files"] "" "Show every files about the Override FS" Bool
                  -> Cmd "Show an Override FS" ()
showOverrideFsCmd (get -> oId') (get -> showLayeredFiles) (get -> showAllFiles) = runCmdMonad $ do
    config <- readConfig
    checkShowOverrideFsParam config oId'
    let overridefs = fromJust $ find ((oId'==) . oId) (overrideFSs config)
    putOverrideFs overridefs
    when (showLayeredFiles && (not showAllFiles)) $ do
        putString "Layered files:"
        layers' <- forM (map layerFilePath $ layers $ overridefs) getFiles'
        let screen = mergeLayers emptyScreen layers'
        putScreen screen
    when showAllFiles $ do
        putString "All files:"
        originalFiles <- ("",) <$> (getFiles (destination overridefs))
        layers' <- forM (map layerFilePath $ layers $ overridefs) getFiles'
        let layersWithOriginalFiles = originalFiles:layers'
            screen = mergeLayers emptyScreen layersWithOriginalFiles
        putScreen screen

checkShowOverrideFsParam :: Config -> FilePath -> CmdMonad ()
checkShowOverrideFsParam Config{..} oId' = do
    when (not $ elem oId' (map oId overrideFSs)) $ throwE ("OverrideFS id : " ++ oId' ++ " is not exists")

-- COMMAND: remove-overridefs
-- ----------------------------------------------------------------------------

removeOverrideFsCmd :: Arg "OID" String
                    -> Flag "" '["inconsistent"] "" "Remove without restore the original state" Bool
                    -> Cmd "Remove an Override FS" ()
removeOverrideFsCmd (get -> oId') (get -> inconsistent) = runCmdMonad $ do
    config <- readConfig
    checkRemoveOverrideFsParam config oId'
    when (not inconsistent) $ do
        -- TODO
        return ()
    let config' = Config { overrideFSs = filter (not . (oId'==) . oId) (overrideFSs config) }
    putString $ show config'
    writeConfig config'

checkRemoveOverrideFsParam :: Config -> FilePath -> CmdMonad ()
checkRemoveOverrideFsParam Config{..} oId' = do
    when (not $ elem oId' (map oId overrideFSs)) $ throwE ("OverrideFS id : " ++ oId' ++ " is not exists")

-- COMMAND: override
-- ----------------------------------------------------------------------------

overrideCmd :: Arg "OID" String
            -> Cmd "Override an Override FS with Layers" ()
overrideCmd (get -> oId') = do
    checkOverrideParam config oId'
    let overridefs = fromJust $ find ((oId'==) . oId) (overrideFSs config)
    screen <- getScreen overridefs
    forM_ screen $ \ Top{..} -> do
        putString $ "item: " ++ topTarget
        putString $ "  " ++ if topLayer == [] then "original" else "from " ++ topLayer
        when (topLayer /= "") $ do
            let backupFilePath = (backup overridefs) ++ (tail topTarget)
           checkBackup <- checkFile backupFilePath
           when (not checkBackup) $ do
               putString $ "  no backup"
               



checkFile :: FilePath -> CmdMonad Bool
checkFile = liftIO . doesFileExist
    

checkOverrideParam :: Config -> FilePath -> CmdMonad ()
checkOverrideParam Config{..} oId' = do
    when (not $ elem oId' (map oId overrideFSs)) $ throwE ("OverrideFS id : " ++ oId' ++ " is not exists")


-- MAIN
-- ----------------------------------------------------------------------------

main :: IO ()
main = run_ $
    Group "Test program for library"
    [ subCmd "new" newCmd
    , subCmd "add-layer" addLayerCmd
    , subCmd "show-all" showAllCmd
    , subCmd "show-overridefs" showOverrideFsCmd
    , subCmd "remove-overridefs" removeOverrideFsCmd
    ]


readConfig :: (MonadIO m) => m Config
readConfig = liftIO $ do
    home <- getHomeDirectory
    let confFile = home ++ "\\.overridefs.config"
    existConf <- doesFileExist confFile
    if existConf
      then read <$> readFile confFile
      else return defaultConfig

writeConfig :: (MonadIO m) => Config -> m ()
writeConfig conf = liftIO $ do
    home <- getHomeDirectory
    let confFile = home ++ "\\.overridefs.config"
    writeFile confFile (show conf)
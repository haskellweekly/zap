module HW.Main where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Version as Version
import qualified HW.Server.Application as Application
import qualified HW.Server.Settings as Settings
import qualified HW.Type.Config as Config
import qualified HW.Type.Flag as Flag
import qualified HW.Vendor.Exception as Exception
import qualified HW.Vendor.Warp as Warp
import qualified Paths_hw as Package
import qualified System.Console.GetOpt as Console
import qualified System.Environment as Environment
import qualified System.Exit as Exit

main :: IO ()
main = do
    config <- getConfig
    let settings = Settings.fromConfig config
    Warp.runSettings settings Application.application

getConfig :: IO Config.Config
getConfig = do
    name <- Environment.getProgName
    arguments <- Environment.getArgs
    result <- getConfigWith name arguments
    case result of
        Left message -> do
            putStrLn message
            Exit.exitSuccess
        Right config -> pure config

getConfigWith
    :: Exception.MonadThrow m
    => String
    -> [String]
    -> m (Either String Config.Config)
getConfigWith name arguments = do
    flags <- Flag.fromArguments arguments
    config <- Config.fromFlags flags
    pure $ if Config.help config
        then Left . List.dropWhileEnd Char.isSpace $ Console.usageInfo
            name
            Flag.options
        else if Config.version config
            then Left $ Version.showVersion Package.version
            else Right config

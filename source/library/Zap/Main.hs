module Zap.Main where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Version as Version
import qualified System.Console.GetOpt as Console
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified Zap.Package as Package
import qualified Zap.Server.Application as Application
import qualified Zap.Server.Settings as Settings
import qualified Zap.Type.Config as Config
import qualified Zap.Type.Context as Context
import qualified Zap.Type.Flag as Flag
import qualified Zap.Vendor.Exception as Exception
import qualified Zap.Vendor.Warp as Warp

main :: IO ()
main = do
    config <- getConfig
    context <- Context.fromConfig config
    Warp.runSettings
        (Settings.fromConfig config)
        (Application.fromContext context)

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

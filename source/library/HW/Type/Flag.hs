module HW.Type.Flag where

import qualified Control.Monad.Catch as Exception
import qualified Data.Char as Char
import qualified Data.List as List
import qualified HW.Exception.InvalidOption as InvalidOption
import qualified HW.Exception.UnexpectedArgument as UnexpectedArgument
import qualified HW.Exception.UnknownOption as UnknownOption
import qualified System.Console.GetOpt as Console

data Flag
    = Help
    | Port String
    | Version
    deriving (Eq, Show)

fromArguments :: Exception.MonadThrow m => [String] -> m [Flag]
fromArguments arguments = do
    let
        (flags, args, opts, errs) =
            Console.getOpt' Console.Permute options arguments
    mapM_ (Exception.throwM . UnexpectedArgument.UnexpectedArgument) args
    mapM_ (Exception.throwM . UnknownOption.UnknownOption) opts
    mapM_
        (Exception.throwM
        . InvalidOption.InvalidOption
        . List.dropWhileEnd Char.isSpace
        )
        errs
    pure flags

options :: [Console.OptDescr Flag]
options =
    [ Console.Option
        ['h', '?']
        ["help"]
        (Console.NoArg Help)
        "shows this help message"
    , Console.Option
        []
        ["port"]
        (Console.ReqArg Port "PORT")
        "sets the port number"
    , Console.Option
        ['v']
        ["version"]
        (Console.NoArg Version)
        "shows the version number"
    ]

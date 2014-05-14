#!/usr/bin/env runhaskell

{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Monad (when)
import System.Directory (doesFileExist, removeFile)
import System.FilePath ((</>))

import Distribution.PackageDescription (PackageDescription)
import Distribution.Simple (UserHooks(..),
                            defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Program (gccProgram, lookupProgram, runProgram)
import Distribution.Simple.Setup (BuildFlags, CleanFlags)
import Distribution.Simple.Utils (die, rawSystemStdout)
import Distribution.System (OS(..), buildOS)
import Distribution.Verbosity (silent)

main :: IO ()
main = defaultMainWithHooks
       simpleUserHooks { buildHook = mkDerivedGmpConstants,
                         cleanHook = rmDerivedGmpConstants
                       }
  where
    mkDerivedGmpConstants :: PackageDescription
                          -> LocalBuildInfo
                          -> UserHooks
                          -> BuildFlags
                          -> IO ()
    mkDerivedGmpConstants pkg_descr lbi userHooks flags =
        case lookupProgram gccProgram (withPrograms lbi) of
            Just gcc ->
                let path = "src" </> exeName in do
                    runProgram silent gcc
                        ["bin" </> "mkDerivedGmpConstants.c", "-o", path]
                    output <- rawSystemStdout silent path []
                    writeFile ("cbits" </> "GmpDerivedConstants.h") output
                    removeFile path
                    buildHook simpleUserHooks pkg_descr lbi userHooks flags
            Nothing -> die "Failed to find GCC!"
      where
        exeName :: FilePath
        exeName = case buildOS of
            Windows -> "mkDerivedGmpConstants.exe"
            _       -> "mkDerivedGmpConstants"

    rmDerivedGmpConstants :: PackageDescription
                          -> ()
                          -> UserHooks
                          -> CleanFlags
                          -> IO ()
    rmDerivedGmpConstants pkg_descr () userHooks flags =
        let path = "cbits" </> "GmpDerivedConstants.h" in
        doesFileExist path >>= \res -> do
            when res $ removeFile path
            cleanHook simpleUserHooks pkg_descr () userHooks flags

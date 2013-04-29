#!/usr/bin/env runhaskell
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wall -Werror #-}

import System.FilePath ((</>))

import Distribution.PackageDescription (PackageDescription)
import Distribution.Simple (UserHooks(..),
                            defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Program (gccProgram, lookupProgram, runProgram)
import Distribution.Simple.Setup (BuildFlags)
import Distribution.Simple.Utils (die, withTempFile, rawSystemStdout,
                                  writeFileAtomic)
import Distribution.Verbosity (silent)

main :: IO ()
main = defaultMainWithHooks
       simpleUserHooks { buildHook = mkDerivedGmpConstants }
  where
    mkDerivedGmpConstants :: PackageDescription
                          -> LocalBuildInfo
                          -> UserHooks
                          -> BuildFlags
                          -> IO ()
    mkDerivedGmpConstants pkg_descr lbi userHooks flags =
        case lookupProgram gccProgram (withPrograms lbi) of
            Just gcc -> do
                withTempFile "src" "mkDerivedGmpConstants" $ \path _h -> do
                    runProgram silent gcc
                        [ "bin/mkDerivedGmpConstants.c", "-o", path]
                    output <- rawSystemStdout silent path []
                    writeFileAtomic ("cbits" </> "GmpDerivedConstants.h") output
                buildHook simpleUserHooks pkg_descr lbi userHooks flags
            Nothing -> die "Failed to find GCC!"

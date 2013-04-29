#!/usr/bin/env runhaskell

import Distribution.PackageDescription (HookedBuildInfo)
import Distribution.Simple (UserHooks(..), Args,
                            defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.Setup (BuildFlags(..))

main :: IO ()
main = defaultMainWithHooks
       simpleUserHooks { preBuild = mkGmpDerivedConstants }
  where
    mkGmpDerivedConstants :: Args -> BuildFlags -> IO HookedBuildInfo
    mkGmpDerivedConstants _args (BuildFlags {}) = undefined

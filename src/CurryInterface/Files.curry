------------------------------------------------------------------------------
--- This library defines operations to read Curry interfaces.
---
--- @version September 2024
------------------------------------------------------------------------------

module CurryInterface.Files where

import System.CurryPath     ( getLoadPathForModule, inCurrySubdir
                            , lookupModuleSourceInLoadPath, stripCurrySuffix )
import System.Directory     ( doesFileExist )
import System.FilePath      ( takeFileName, (</>), (<.>) )
import System.FrontendExec

import CurryInterface.Parser
import CurryInterface.Types

--- I/O action which parses a Curry interface and returns it as a data structure.
--- The argument is the module name (without suffix ".curry" or ".lcurry").
readCurryInterface :: String -> IO Interface
readCurryInterface modname =
  readCurryInterfaceWithParseOptions modname (setQuiet True defaultParams)

--- I/O action which reads a Curry interface from a file (with extension
--- `.icurry`) with respect to some parser options.
readCurryInterfaceWithParseOptions :: String -> FrontendParams -> IO Interface
readCurryInterfaceWithParseOptions modname options = do
  mbsrc <- lookupModuleSourceInLoadPath modname
  case mbsrc of
    Nothing -> error $ "Module '" ++ modname ++ "' not found in load path!"
    Just (dir,_) -> do
      -- read FlatCurry to generate interface file
      callFrontendWithParams FCY options modname
      readCurryInterfaceFile (curryInterfaceFileName (dir </> modname))

--- Transforms a name of a Curry program (with or without suffix ".curry"
--- or ".lcurry") into the name of the file containing the Curry interface.
curryInterfaceFileName :: String -> String
curryInterfaceFileName prog = inCurrySubdir (stripCurrySuffix prog) <.> "icurry"

--- I/O action which reads a Curry interface from a file in `.icurry` format.
readCurryInterfaceFile :: String -> IO Interface
readCurryInterfaceFile filename = do
  exacy <- doesFileExist filename
  if exacy
   then readExistingICURRY filename
   else error $ "EXISTENCE ERROR: Curry interface file '" ++ filename ++
                "' does not exist"
 where
   readExistingICURRY fname = do
     icurrystring <- readFile fname
     -- for testing:
     --putStrLn icurrystring
     return $ parseCurryInterface icurrystring

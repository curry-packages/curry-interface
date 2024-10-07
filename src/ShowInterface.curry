------------------------------------------------------------------------------
--- This module contains the implementation of a tool to show the interface
--- of a Curry program in human-readable form.
---
--- For this purpose, it reads the `.icurry` interface file of a module and
--- pretty-prints it with various options.
---
--- @author Michael Hanus
--- @version September 2024
------------------------------------------------------------------------------

module ShowInterface
 where

import Control.Monad         ( unless, when )
import Data.Maybe
import System.Environment    ( getArgs )
import System.Console.GetOpt

import System.CurryPath      ( runModuleAction, stripCurrySuffix )
import System.Process        ( exitWith )
import Text.Pretty

import CurryInterface.Files
import CurryInterface.Pretty
import CurryInterface.Types

------------------------------------------------------------------------------
banner :: String
banner = unlines [bannerLine, bannerText, bannerLine]
 where
  bannerText = "Curry Interface Printing Tool (Version of 28/09/24)"
  bannerLine = take (length bannerText) (repeat '=')

main :: IO ()
main = do
  args <- getArgs
  let dfltopts = defaultOptions
                   { optWithImports = False, optQualify = False
                   , optWithArity = False, optWithHiding = False
                   , optWithInstance = False, optWithKinds = False }
  (opts,progs) <- processOptions dfltopts args
  mapM_ (runModuleAction (showInterface opts)) progs

-- Reads an `.icurry` interface for a module and pretty print it.
showInterface :: Options -> String -> IO ()
showInterface opts mname = do
  ic <- readCurryInterface mname
  --print ic -- only for debugging
  let Interface _ _ idecls = ic
      linsts  = filter isLocalInstance idecls
      modopts = opts { optModule = mname, optInstances = linsts }
  putStrLn $ line ++ "\nInterface of module '" ++ mname ++ "':\n" ++ line
  putStrLn $ pPrint $ ppInterface modopts ic
 where
  line = take 70 (repeat '-')

  isLocalInstance id = case id of
    IInstanceDecl _ _ _ _ Nothing -> True
    _                             -> False

------------------------------------------------------------------------------
--- Process the actual command line argument and return the options
--- and the name of the main program.
processOptions :: Options -> [String] -> IO (Options,[String])
processOptions initopts argv = do
  let (funopts, args, opterrors) = getOpt Permute options argv
      opts = foldl (flip id) initopts funopts
  unless (null opterrors)
         (putStr (unlines opterrors) >> printUsage >> exitWith 1)
  when (optHelp opts) (printUsage >> exitWith 0)
  return (opts, map stripCurrySuffix args)
 where
  printUsage = putStrLn (banner ++ "\n" ++ usageText)

-- Help text
usageText :: String
usageText =
  usageInfo ("Usage: curry-showinterface [options] <module names>\n") options

-- Definition of actual command line options.
options :: [OptDescr (Options -> Options)]
options =
  [ Option "h?" ["help"]
           (NoArg (\opts -> opts { optHelp = True }))
           "print help and exit"
  , Option "a" ["all"]
           (NoArg (\opts -> opts { optWithImports = True, optWithString = False
                                 , optWithHiding = True, optWithInstance = True
                                 , optWithKinds = True
                                 , optQualify = True, optWithArity = True }))
           "show all details of the interface"
  , Option "i" ["instances"]
           (NoArg (\opts -> opts { optWithInstance = True }))
           "show detailed class instances"
  , Option "q" ["qualify"]
           (NoArg (\opts -> opts { optQualify = True }))
           "show module qualifications"
  ]

------------------------------------------------------------------------------

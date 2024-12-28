module Test where

import CurryInterface.Files  ( curryInterfaceFileName, readCurryInterface )
import CurryInterface.Pretty ( ppInterface, defaultOptions )
import ShowInterface         ( showInterface )

import Text.Pretty         ( pPrint )
import System.FrontendExec ( callFrontendWithParams, FrontendTarget(..)
                           , defaultParams, setQuiet )
import Test.Prop           ( returns, PropIO )

check :: String -> PropIO
check = flip returns True . runTest 

testModule :: PropIO
testModule = check "Module"

testHidden :: PropIO
testHidden = check "Module"

-- Returns the pretty-printed interface of a module as a string
getInterfaceStr :: String -> IO String
getInterfaceStr mname = do
  ic <- readCurryInterface mname
  return $ pPrint $ ppInterface defaultOptions ic

-- Compares the pretty-printed interface of a module with the
-- content of the corresponding `.icurry` file. 
--
-- Returns `True` if they match, `False` otherwise. Matching 
-- means that the pretty-printed interface is equal to the
-- content of the `.icurry` file, ignoring all formatting.
runTest :: String -> IO Bool
runTest cm = do
  callFrontendWithParams ACY (setQuiet True defaultParams) cm
  
  let ifile = curryInterfaceFileName cm
  cif  <- reformat <$> readFile ifile
  cipp <- reformat <$> getInterfaceStr cm

  return $ cif == cipp
 where  
  reformat = unwords . words

main :: IO ()
main = do
  x <- runTest "Module"
  print x
module Lambdabot.Util.Powershell (
  makePSCommand,
  execPSCommand
) where

import Control.Exception.Base
import Data.String.Utils
import System.Exit (ExitCode(..))
import System.IO
import System.Process

newtype PSCommand = PSCommand String

makePSCommand :: String -> PSCommand
makePSCommand = PSCommand . replace "'" "''"

execPSCommand :: PSCommand -> IO (ExitCode, String)
execPSCommand (PSCommand cmd) = do
    bracket
      (createProcess (proc "powershell.exe" args){ std_out = CreatePipe })
      (\(_, Just outh, _, _) -> hClose outh)
      (\(_, Just outh, _, pid) -> do
        output <- hGetContents outh
        exitCode <- waitForProcess pid
        return (exitCode, output))
  where
    args = [ "-ExecutionPolicy", "Unrestricted", "-Command", "'" ++ cmd ++ "'" ]

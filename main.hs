import Control.Exception (handle, IOException)

import qualified System.Environment as Env
import qualified System.IO as IO

import Coder

main = Env.getArgs >>= decideRunner
    where decideRunner [] = runRepl
          decideRunner (path:_) = runScriptByPath path

runScriptByPath :: String -> IO ()
runScriptByPath = handle ioPrinter . openAndRun
    where openAndRun path = IO.withFile path IO.ReadMode runScript
          ioPrinter :: IOException -> IO ()
          ioPrinter = print

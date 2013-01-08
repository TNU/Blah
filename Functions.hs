module Functions (
    sysFuncs
) where

import Control.Monad (liftM)

import qualified Data.Map as Map

import Failure
import Converters (toStr)
import State (Value(..), Runtime)
import qualified State as State

sysFuncs :: Map.Map String Value
sysFuncs = Map.fromList . map makeFuncEntry $ [
       ("isEOF",     isEOF),
       -- ("read",      Vsf read "isEOF"),
       ("readLine",  readLine),
       ("write",     write),
       ("writeLine", writeLine)
    ]
    where makeFuncEntry (name, func) = (name, Vsf func name)

isEOF :: [Value] -> Runtime Value
isEOF [] = Vb `liftM` State.isEOF
isEOF _  = argFail "isEOF()"

readLine :: [Value] -> Runtime Value
readLine [] = State.isEOF >>= readOne
    where readOne True  = return Vnothing
          readOne False = Vs `liftM` State.readLine
readLine _  = argFail "readLine()"

write :: [Value] -> Runtime Value
write values = writeValues values >> return Vnothing
    where writeValues [] = return ()
          writeValues [value] = writeSingle value
          writeValues (value:rest) = writeValues rest >> writeOne value
          writeSingle value = toStr value >>= State.usingIO . putStr
          writeOne value = State.usingIO (putStr " ") >> writeSingle value

writeLine :: [Value] -> Runtime Value
writeLine values = write values >> State.writeLine "" >> return Vnothing

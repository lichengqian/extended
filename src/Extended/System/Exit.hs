{-# LANGUAGE ForeignFunctionInterface #-}
module Extended.System.Exit (
    module System.Exit
  , exitProgram
    ) where

import           System.Exit

foreign import ccall "exit" exit :: IO ()

exitProgram :: IO ()
exitProgram = exit

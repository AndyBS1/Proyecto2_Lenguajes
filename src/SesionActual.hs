module SesionActual (currentUser) where

import Data.IORef (IORef, newIORef)
import System.IO.Unsafe (unsafePerformIO)

-- Usuario actual
currentUser :: IORef (Maybe String)
currentUser = unsafePerformIO (newIORef Nothing)
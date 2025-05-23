{-# LANGUAGE DeriveGeneric #-}

module Main(main, currentUser) where

import GHC.Generics (Generic)
import System.IO (hSetEcho, hGetEcho, stdin, stdout, hFlush, getChar)
import Data.Char (isSpace)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.List (find)
import Crypto.Hash (hash, SHA256(..), Digest)
import Data.ByteString.Base16 (encode)
import System.Process (callCommand)
import System.Directory (doesFileExist)
import System.Exit (exitSuccess)
import Data.Aeson (FromJSON, ToJSON, decodeFileStrict, encodeFile)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)
import SesionActual (currentUser)
import GestionDeContrasenas

-- Estructura de usuario
data UserInfo = UserInfo
  { username :: String
  , pinHash :: String
  } deriving (Show, Generic)

instance FromJSON UserInfo
instance ToJSON UserInfo

-- Archivo donde se guarda la información de usuarios
userFile :: FilePath
userFile = "users.json"

-- Hashear el PIN
hashPIN :: String -> String
hashPIN pin =
  let digest = hash (BC.pack pin) :: Digest SHA256
      str = show digest
      bytes = BC.pack str
  in BC.unpack $ encode bytes

-- Leer input oculto con asteriscos
getHiddenInput :: IO String
getHiddenInput = do
  hSetEcho stdin False
  input <- loop ""
  hSetEcho stdin True
  putStrLn ""
  return input
  where
    loop acc = do
      c <- getChar
      case c of
        '\n' -> return acc
        '\DEL' -> if null acc
                    then loop acc
                    else do putStr "\b \b"; loop (init acc)
        _ -> do
          putChar '*'
          loop (acc ++ [c])

-- Cargar usuarios desde JSON
loadUsers :: IO [UserInfo]
loadUsers = do
  exists <- doesFileExist userFile
  if exists
    then do
      mUsers <- decodeFileStrict userFile
      return (maybe [] id mUsers)
    else return []

-- Guardar usuarios en JSON
saveUsers :: [UserInfo] -> IO ()
saveUsers = encodeFile userFile

-- Registro de usuario con validación
registerUser :: IO ()
registerUser = do
  users <- loadUsers
  putStrLn "Por favor, introduce un nombre de usuario:"
  username' <- getLine
  if any (\u -> username u == username') users
    then putStrLn "Ese nombre de usuario ya está registrado."
    else do
      putStrLn "Define un PIN:"
      pin1 <- getHiddenInput
      putStrLn "Confirma tu PIN:"
      pin2 <- getHiddenInput
      if pin1 == pin2
        then do
          let newUser = UserInfo username' (hashPIN pin1)
          saveUsers (users ++ [newUser])
          putStrLn "Usuario registrado exitosamente."
        else
          putStrLn "Los PINs no coinciden."

-- Validar ingreso de usuario
requestPIN :: IO Bool
requestPIN = do
  users <- loadUsers
  putStrLn "Nombre de usuario:"
  enteredUsername <- getLine
  putStrLn "PIN:"
  enteredPIN <- getHiddenInput
  case find (\u -> username u == enteredUsername) users of
    Just user ->
      if hashPIN enteredPIN == pinHash user
        then do
          writeIORef currentUser (Just enteredUsername)
          return True
        else putStrLn "PIN incorrecto." >> return False
    Nothing -> putStrLn "Usuario no encontrado." >> return False

-- Menú principal
showMenu :: IO ()
showMenu = do
  putStrLn "\n=== MENÚ PRINCIPAL ==="
  putStrLn "1. Iniciar sesión"
  putStrLn "2. Registrar nuevo usuario"
  putStrLn "3. Salir"
  putStr "Seleccione una opción: "
  hFlush stdout

-- Menú selección
handleMenuSelection :: IO ()
handleMenuSelection = do
  option <- getLine
  case option of
    "1" -> do
      accessGranted <- requestPIN
      if accessGranted
        then do
          putStrLn "\nInicio de sesión exitoso!"
          maybeUsername <- readIORef currentUser
          case maybeUsername of
            Just username -> putStrLn $ "Bienvenido, " ++ username
            Nothing -> putStrLn "Error, no se pudo cargar el usuario."
          iniciarGestion
        else putStrLn "Acceso denegado."
      showMenu
      handleMenuSelection
    "2" -> do
      registerUser
      showMenu
      handleMenuSelection
    "3" -> do
      putStrLn "Saliendo del sistema..."
      exitSuccess
    _ -> do
      putStrLn "Opción no válida. Intente nuevamente."
      showMenu
      handleMenuSelection

-- Función principal
main :: IO ()
main = do
  showMenu
  handleMenuSelection
module Main(main, currentUser) where

import System.IO (hSetEcho, hGetEcho, stdin, stdout, hFlush)
import Data.Char (isSpace)
import qualified Data.ByteString.Char8 as BC
import Crypto.Hash (hash, SHA256(..), Digest)
import Data.ByteString.Base16 (encode)
import System.Process (callCommand)
import System.Directory (doesFileExist, removeFile)
import Control.Monad (when)
import Data.List (lines, break)
import System.Exit (exitSuccess)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)
import SesionActual (currentUser)
import GestionDeContrasenas

-- Menu principal
showMenu :: IO ()
showMenu = do
  putStrLn "\n=== MENU PRINCIPAL ==="
  putStrLn "1. Iniciar sesion"
  putStrLn "2. Registrar nuevo usuario"
  putStrLn "3. Salir"
  putStr "Seleccione una opción: "
  hFlush stdout

-- Ocultar PINs Preguntar si asi o con asteriscos ----------------------------
withEcho :: Bool -> IO String -> IO String
withEcho echo action = do
  old <- hGetEcho stdin
  hSetEcho stdin echo
  x <- action
  hSetEcho stdin old
  return x

-- Usuario
data UserInfo = UserInfo { username :: String, pinHash :: String }

-- Archivo CAMBIAR Y VER CUAL VAMOS A USAR ---------------
userFile :: FilePath
userFile = "users.txt"

-- Hashear el PIN TEMPORALLLLLLLLLLLLLLLLLLLLLLLLLL -----------------
hashPIN :: String -> String
hashPIN pin = 
  let digest = hash (BC.pack pin) :: Digest SHA256
      str = show digest
      bytes = BC.pack str
  in BC.unpack $ encode bytes

-- Registrar un usuario
registerUser :: IO ()
registerUser = do
  putStrLn "Bienvenido al Administrador de Contrasenas."
  putStrLn "Por favor, introduce un nombre de usuario:"
  username' <- getLine
  putStrLn "Por favor, define un PIN para proteger tu acceso:"
  pin1 <- withEcho False getLine
  putStrLn "\nConfirma tu PIN:"
  pin2 <- withEcho False getLine
  putStrLn "" 
  
  if pin1 == pin2
    then do
      let pinHashed = hashPIN pin1
      appendFile userFile (username' ++ ":" ++ pinHashed ++ "\n")
      putStrLn $ "Usuario '" ++ username' ++ "' registrado correctamente."
    else do
      putStrLn "Los PINs no coinciden."


splitOn :: Char -> String -> [String]
splitOn delimiter text = case break (== delimiter) text of
  (prefix, "")    -> [prefix]
  (prefix, suffix) -> prefix : splitOn delimiter (drop 1 suffix)

-- Buscar un usuario por nombre
findUser :: String -> [[String]] -> Maybe (String, String)
findUser usernameToFind users =
  foldr (\(u:ph:_) acc -> if u == usernameToFind then Just (u, ph) else acc) Nothing users

-- Verificar el PIN
requestPIN :: IO Bool
requestPIN = do
  putStrLn "Por favor, ingresa tu nombre de usuario:"
  enteredUsername <- getLine
  putStrLn "Por favor, ingresa tu PIN para acceder:"
  enteredPIN <- withEcho False getLine
  putStrLn ""

  fileExists <- doesFileExist userFile
  if fileExists
    then do
      contents <- readFile userFile
      let users = map (splitOn ':') (lines contents)
      case findUser enteredUsername users of
        Just (_, storedPINHash) ->
          if hashPIN enteredPIN == trim storedPINHash
            then do
              writeIORef currentUser (Just enteredUsername)  
              return True
            else return False
        Nothing -> do
          putStrLn "Nombre de usuario no encontrado."
          return False
    else do
      putStrLn "No se encontro informacion de usuario, primero debes registrarte."
      return False

-- Trim
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse

-- FCopiar texto al portapapeles 
copyToClipboard :: String -> IO ()
copyToClipboard text = callCommand $ "echo " ++ text ++ " | clip"

-- Menu
handleMenuSelection :: IO ()
handleMenuSelection = do
  option <- getLine
  case option of
    "1" -> do
      accessGranted <- requestPIN
      if accessGranted
        then do
          putStrLn "\nInicio de sesion exitoso!"
          maybeUsername <- readIORef currentUser
          case maybeUsername of
            Just username -> putStrLn $ "Bienvenido, " ++ username
            Nothing -> putStrLn "Error, no se pudo cargar el usuario."
          putStrLn "Pendiente............."
          showMenu
          handleMenuSelection
        else do
          putStrLn "\nError en las credenciales"
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
      putStrLn "Opcion no valida, por favor intente nuevamente."
      showMenu
      handleMenuSelection

-- Funcion principal 
main :: IO ()
main = do
  fileExists <- doesFileExist userFile
  if not fileExists
    then do
      putStrLn "Primera vez que ingresas a nuestro sistema, Bienvenido."
      registerUser
      showMenu
      handleMenuSelection
    else do
      showMenu
      handleMenuSelection
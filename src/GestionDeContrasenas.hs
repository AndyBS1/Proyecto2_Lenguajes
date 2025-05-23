{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module GestionDeContrasenas where

import System.IO (hSetBuffering, hSetEcho, hGetEcho, stdin, stdout, BufferMode(NoBuffering))
import GHC.Generics (Generic)
import Data.Aeson
import Data.IORef (readIORef)
import SesionActual (currentUser)
import qualified Data.ByteString.Lazy as B
import Control.Monad (when)
import System.Directory (doesFileExist)
import Cifrado (cifrar, descifrar)
import System.Process (callCommand)


data Credencial = Credencial {
    titulo :: String,
    usuario :: String,
    password :: String
} deriving (Show, Generic, Eq)

instance ToJSON Credencial
instance FromJSON Credencial

data Cuenta = Cuenta
  { cuenta :: String,
    credenciales :: [Credencial]  
  } deriving (Show, Generic, Eq)

instance ToJSON Cuenta
instance FromJSON Cuenta

passwordFile :: FilePath
passwordFile = "cuentas.json"

withEcho_GC :: Bool -> IO String -> IO String
withEcho_GC echo action = do
  old <- hGetEcho stdin
  hSetEcho stdin echo
  x <- action
  hSetEcho stdin old
  return x

agregarCredencialACuenta :: String -> Credencial -> [Cuenta] -> [Cuenta]
agregarCredencialACuenta nombreUsuario nuevaCredencial =
  map actualizarCuenta
  where
    actualizarCuenta cuenta@(Cuenta nombre creds)
      | nombre == nombreUsuario = Cuenta nombre (nuevaCredencial : creds)
      | otherwise = cuenta

consultarTodasCredenciales :: IO ()
consultarTodasCredenciales = do
    maybeUsuario <- readIORef currentUser
    case maybeUsuario of
      Nothing -> putStrLn "ERROR. No se ha encontrado una sesión activa."
      Just usuarioActual -> do
        existe <- doesFileExist passwordFile
        if not existe 
          then putStrLn "No hay cuentas registradas aún."
          else do
            contenido <- B.readFile passwordFile
            case decode contenido :: Maybe [Cuenta] of
              Nothing -> putStrLn "Error al leer las cuentas."
              Just cuentas -> do
                let cuentaUsuario = filter (\(Cuenta nombre _) -> nombre == usuarioActual) cuentas
                case cuentaUsuario of
                  [] -> putStrLn "No se encontró la cuenta para el usuario actual."
                  (Cuenta _ credenciales : _) -> do
                    if null credenciales
                      then putStrLn "No hay credenciales guardadas para esta cuenta."
                      else do
                        putStrLn $ "\nCredenciales de: " ++ usuarioActual
                        putStrLn $ replicate 50 '-'
                        putStrLn $ ajustarTexto "Título" 15 ++ " | " ++ ajustarTexto "Usuario" 15 ++ " | " ++ ajustarTexto "Contraseña" 15
                        putStrLn $ replicate 50 '-'
                        mapM_ imprimirCredenciales credenciales

-- Imprimir en formato tabla
imprimirCredenciales :: Credencial -> IO ()
imprimirCredenciales (Credencial titulo usuario _) = do
    let tituloTabla = ajustarTexto titulo 15
    let usuarioTabla = ajustarTexto (ocultarUsuario usuario) 15
    let passwordTabla = ajustarTexto (simularAsteriscos 8) 15
    putStrLn $ tituloTabla ++ " | " ++ usuarioTabla ++ " | " ++ passwordTabla

ocultarUsuario :: String -> String
ocultarUsuario usuario
    | length usuario <= 4 = replicate (length usuario) '*'
    | otherwise = take 4 usuario ++ replicate (length usuario - 4) '*'

simularAsteriscos :: Int -> String
simularAsteriscos n = replicate n '*'

ajustarTexto :: String -> Int -> String
ajustarTexto str ancho
  | length str >= ancho = take ancho str
  | otherwise = str ++ replicate (ancho - length str) ' '

consultarPorServicio :: IO ()
consultarPorServicio = do
    maybeUsuario <- readIORef currentUser
    case maybeUsuario of
        Nothing -> putStrLn "ERROR. No se ha encontrado una sesión activa."
        Just usuarioActual -> do
            putStrLn "Ingrese un servicio: "
            servicioBuscado <- getLine

            existe <- doesFileExist passwordFile
            if not existe 
              then putStrLn "No hay cuentas registradas aún."
              else do
                contenido <- B.readFile passwordFile
                case decode contenido :: Maybe [Cuenta] of
                    Nothing -> putStrLn "Error al leer las cuentas."
                    Just cuentas -> do
                        let cuentaUsuario = filter (\(Cuenta nombre _) -> nombre == usuarioActual) cuentas
                        case cuentaUsuario of
                            [] -> putStrLn "No se encontró la cuenta del usuario actual."
                            (Cuenta _ credenciales : _) -> do
                                let credencialesFiltradas = filter (\credencial -> titulo credencial == servicioBuscado) credenciales
                                if null credencialesFiltradas
                                  then putStrLn "No se encontraron credenciales para ese servicio."
                                  else do
                                    putStrLn $ replicate 50 '-'
                                    mapM_ imprimirCredencialEspecifica credencialesFiltradas
                                    putStrLn $ replicate 50 '-'
                                    putStrLn "=== Menú de opciones para copiar al portapapeles ==="
                                    putStrLn "1. Usuario"
                                    putStrLn "2. Contraseña"
                                    putStrLn "3. Salir"
                                    opcion <- getLine
                                    let credencial = head credencialesFiltradas
                                    case opcion of
                                        "1" -> do
                                            copyToClipboard (usuario credencial)
                                            putStrLn "Usuario copiado al portapapeles."
                                        "2" -> do
                                            copyToClipboard (descifrar (password credencial))
                                            putStrLn "Contraseña copiada al portapapeles."
                                        "3" -> putStrLn "No se ha copiado ningún dato al portapapeles."
                                        _   -> putStrLn "Opción no válida."

-- Copiar al portapapeles
copyToClipboard :: String -> IO ()
copyToClipboard text = callCommand $ "echo " ++ text ++ " | clip"

consultarPorUsuario :: IO ()
consultarPorUsuario = do
    maybeUsuario <- readIORef currentUser
    case maybeUsuario of
        Nothing -> putStrLn "ERROR. No se ha encontrado una sesión activa."
        Just usuarioActual -> do
            putStrLn "Ingrese el nombre de la cuenta: "
            usuarioBuscado <- getLine

            existe <- doesFileExist passwordFile
            if not existe 
              then putStrLn "No hay cuentas registradas aún."
              else do
                contenido <- B.readFile passwordFile
                case decode contenido :: Maybe [Cuenta] of
                    Nothing -> putStrLn "Error al leer las cuentas."
                    Just cuentas -> do
                        let cuentaUsuario = filter (\(Cuenta nombre _) -> nombre == usuarioActual) cuentas
                        case cuentaUsuario of
                            [] -> putStrLn "No se encontró la cuenta del usuario actual."
                            (Cuenta _ credenciales : _) -> do
                                let credencialesFiltradas = filter (\credencial -> usuario credencial == usuarioBuscado) credenciales
                                if null credencialesFiltradas
                                  then putStrLn "No se encontraron credenciales para ese nombre de usuario."
                                  else do
                                    putStrLn $ replicate 50 '-'
                                    mapM_ imprimirCredencialEspecifica credencialesFiltradas
                                    putStrLn $ replicate 50 '-'

-- Imprimir en otro formato (No el de la tabla)
imprimirCredencialEspecifica :: Credencial -> IO ()
imprimirCredencialEspecifica (Credencial titulo usuario password) = do
    putStrLn $ "Servicio: " ++ titulo
    putStrLn $ "Usuario: " ++ usuario
    putStrLn $ "Contraseña: " ++ descifrar password
  
eliminarCredencial :: String -> String -> [Cuenta] -> ([Cuenta], Bool)
eliminarCredencial usuarioActual servicioAEliminar cuentas =
  let (nuevasCuentas, huboCambio) = unzip $ map actualizarCredenciales cuentas
  in (nuevasCuentas, or huboCambio)
  where
    actualizarCredenciales (Cuenta nombre credenciales)
      | nombre == usuarioActual =
          let nuevasCredenciales = filter (\credencial -> titulo credencial /= servicioAEliminar) credenciales
          in (Cuenta nombre nuevasCredenciales, length credenciales /= length nuevasCredenciales)
      | otherwise = (Cuenta nombre credenciales, False)

modificarCredencial :: String -> String -> String -> String -> [Cuenta] -> ([Cuenta], Bool)
modificarCredencial usuarioActual servicio campoAModificar nuevoValor cuentas =
  let (cuentasActualizadas, modificaciones) = unzip $ map actualizarCuenta cuentas
  in (cuentasActualizadas, or modificaciones)
  where
    actualizarCuenta (Cuenta nombre credenciales)
      | nombre == usuarioActual =
          let (nuevasCredenciales, cambios) = unzip $ map credencialEspecifica credenciales
          in (Cuenta nombre nuevasCredenciales, or cambios)
      | otherwise = (Cuenta nombre credenciales, False)

    credencialEspecifica credencial
      | titulo credencial == servicio =
          case campoAModificar of
            "usuario"    -> (credencial { usuario = nuevoValor }, True)
            "contrasena" -> (credencial { password = nuevoValor }, True)
            _            -> (credencial, False)
      | otherwise = (credencial, False)

iniciarGestion  :: IO ()
iniciarGestion  = do
    hSetBuffering stdout NoBuffering

    maybeUsuario <- readIORef currentUser
    case maybeUsuario of
      Nothing -> putStrLn "ERROR. No se ha encontrado una sesión activa"
      Just enteredUsername -> do
        let rutaArchivo = "cuentas.json"
        existe <- doesFileExist rutaArchivo
        cuentas <- if existe
                    then do
                      contenido <- B.readFile rutaArchivo
                      case decode contenido of
                        Just cs -> return cs
                        Nothing -> return []
                    else return []

        let yaExisteCuenta = any (\(Cuenta nombre _) -> nombre == enteredUsername) cuentas

        when (not yaExisteCuenta) $ do
          let nuevaCuenta = Cuenta enteredUsername []
              cuentasActualizadas = nuevaCuenta : cuentas
          B.writeFile rutaArchivo (encode cuentasActualizadas)
          putStrLn $ "Cuenta creada para el usuario: " ++ enteredUsername

        -- Menu
        putStrLn "\n--------------------------------"
        putStrLn "=== Gestion de Contraseñas ==="
        putStrLn "1. Consultar todas"
        putStrLn "2. Consultar por servicio"
        putStrLn "3. Consultar por cuenta"
        putStrLn "4. Agregar"
        putStrLn "5. Modificar"
        putStrLn "6. Eliminar"
        putStrLn "7. Salir"
        putStr "Seleccione una opción (1-7): "
        opcion <- getLine

        case opcion of
            "1" -> do
                putStrLn "\n--------------------------------"
                consultarTodasCredenciales
                iniciarGestion

            "2" -> do
                putStrLn "\n--------------------------------"
                consultarPorServicio
                iniciarGestion

            "3" -> do
                putStrLn "\n--------------------------------"
                consultarPorUsuario  
                iniciarGestion
                
            "4" -> do
                putStrLn "\n--------------------------------"
                putStrLn "Ingrese un servicio: "
                servicio <- getLine

                putStrLn "Ingrese el usuario: "
                usuario <- getLine

                putStrLn "Ingrese su contraseña: "
                password <- withEcho_GC False getLine
                putStrLn ""
                let passwordCifrado = cifrar password
                let nuevaCredencial = Credencial servicio usuario passwordCifrado

                let yaExisteServicio = any (\(Cuenta nombre creds) ->
                                      nombre == enteredUsername &&
                                      any (\c -> titulo c == servicio) creds
                                  ) cuentas

                if yaExisteServicio
                  then putStrLn "Ya existe una credencial para ese servicio."
                  else do
                    let cuentasActualizadas =
                          map (\cuenta@(Cuenta nombre creds) ->
                                if nombre == enteredUsername
                                  then Cuenta nombre (nuevaCredencial : creds)
                                  else cuenta
                              ) cuentas

                    B.writeFile rutaArchivo (encode cuentasActualizadas)
                    putStrLn "Credencial agregada exitosamente."

                iniciarGestion
                
            "5" -> do
                putStrLn "\n--------------------------------"
                putStrLn "Ingrese un servicio: "
                servicio <- getLine

                putStrLn "Ingrese el dato a modificar (usuario/contrasena): "
                campoAModificar <- getLine

                let mensaje = case campoAModificar of
                      "usuario"    -> "Ingrese el nuevo nombre de usuario: "
                      "contrasena" -> "Ingrese la nueva contraseña: "
                      _            -> "Ingrese el nuevo valor: "
                putStrLn mensaje

                nuevoValor_input <- if campoAModificar == "contrasena"
                                      then do
                                        valor <- withEcho_GC False getLine
                                        putStrLn ""
                                        return valor
                                        else getLine

                let nuevoValor = if campoAModificar == "contrasena"
                                  then cifrar nuevoValor_input
                                  else nuevoValor_input

                maybeUsuario <- readIORef currentUser
                case maybeUsuario of
                  Nothing -> putStrLn "ERROR. No hay sesión activa."
                  Just usuarioActual -> do
                    existe <- doesFileExist passwordFile
                    if not existe
                      then putStrLn "No hay cuentas registradas aún."
                      else do
                        contenido <- B.readFile passwordFile
                        case decode contenido :: Maybe [Cuenta] of
                          Nothing -> putStrLn "Error al leer las cuentas."
                          Just cuentas -> do
                            let (cuentasActualizadas, fueModificada) = modificarCredencial usuarioActual servicio campoAModificar nuevoValor cuentas
                            if not fueModificada
                              then putStrLn "No se encontró la credencial o el campo no es válido."
                              else do
                                B.writeFile passwordFile (encode cuentasActualizadas)
                                putStrLn "Credencial modificada exitosamente."

                iniciarGestion

            "6" -> do
                maybeUsuario <- readIORef currentUser
                case maybeUsuario of
                  Nothing -> putStrLn "ERROR. No se ha encontrado una sesión activa."
                  Just usuarioActual -> do
                    putStrLn "\n--------------------------------"
                    putStrLn "Ingrese un servicio: "
                    servicio <- getLine

                    existe <- doesFileExist passwordFile
                    if not existe 
                      then putStrLn "No hay cuentas registradas aún."
                      else do
                        contenido <- B.readFile passwordFile
                        case decode contenido :: Maybe [Cuenta] of
                          Nothing -> putStrLn "Error al leer las cuentas."
                          Just cuentas -> do
                            let (cuentasActualizadas, seElimino) = eliminarCredencial usuarioActual servicio cuentas
                            if not seElimino
                              then putStrLn "No se encontró una credencial asociada al servicio."
                              else do
                                B.writeFile passwordFile (encode cuentasActualizadas)
                                putStrLn "Credencial eliminada exitosamente."

                iniciarGestion

            "7" -> putStrLn "Saliendo del programa..."
            
            _ -> do
                putStrLn "Opción no válida. Intente nuevamente."
                iniciarGestion
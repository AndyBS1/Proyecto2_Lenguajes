{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module GestionDeContrasenas where

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import GHC.Generics (Generic)
import Data.Aeson
import Data.IORef (readIORef)
import SesionActual (currentUser)
import qualified Data.ByteString.Lazy as B
import Control.Monad (when)
import System.Directory (doesFileExist)

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

agregarCredencialACuenta :: String -> Credencial -> [Cuenta] -> [Cuenta]
agregarCredencialACuenta nombreUsuario nuevaCredencial =
  map actualizarCuenta
  where
    actualizarCuenta cuenta@(Cuenta nombre creds)
      | nombre == nombreUsuario = Cuenta nombre (nuevaCredencial : creds)
      | otherwise = cuenta

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
        putStrLn "Gestion de Contrasenas"
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
                -- Mostrar todas las cuentas
                iniciarGestion

            "2" -> do
                putStrLn "\n--------------------------------"
                putStrLn "Ingrese un servicio: "
                texto <- getLine

                iniciarGestion

            "3" -> do
                putStrLn "\n--------------------------------"
                putStrLn "Ingrese el nombre de la cuenta: "
                texto <- getLine
                
                iniciarGestion
                
            "4" -> do
                putStrLn "\n--------------------------------"
                putStrLn "Ingrese un servicio: "
                servicio <- getLine

                putStrLn "Ingrese el usuario: "
                usuario <- getLine

                putStrLn "Ingrese su contrasena: "
                password <- getLine
                
                let nuevaCredencial = Credencial servicio usuario password

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

                putStrLn "Ingrese el dato a modificar (usuario/contraseña): "
                dato <- getLine
                
                iniciarGestion

            "6" -> do
                putStrLn "\n--------------------------------"
                putStrLn "Ingrese un servicio: "
                servicio <- getLine
                
                iniciarGestion

            "7" -> putStrLn "Saliendo del programa..."
            
            _ -> do
                putStrLn "Opción no válida. Intente nuevamente."
                iniciarGestion
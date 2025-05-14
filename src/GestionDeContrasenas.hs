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

data Credencial = Credencial {
    titulo :: String,
    usuario :: String,
    password :: String
} deriving (Show, Generic)

-- Instancia de Aeson para Credencial
instance ToJSON Credencial where
  toJSON (Credencial titulo usuario password) =
    object ["titulo" .= titulo, "usuario" .= usuario, "password" .= password]

instance FromJSON Credencial where
  parseJSON = withObject "Credencial" $ \v ->
    Credencial <$> v .: "titulo"
               <*> v .: "usuario"
               <*> v .: "password"

data Cuenta = Cuenta
  { cuenta :: String,
    credenciales :: [Credencial]  
  } deriving (Show, Generic)

-- Instancia de Aeson para Cuenta
instance ToJSON Cuenta where
  toJSON (Cuenta cuenta credenciales) =
    object ["cuenta" .= cuenta, "credenciales" .= credenciales]

instance FromJSON Cuenta where
  parseJSON = withObject "Cuenta" $ \v ->
    Cuenta <$> v .: "cuenta"
           <*> v .: "credenciales"

passwordFile :: FilePath
passwordFile = "passwords.json"

iniciarGestion  :: IO ()
iniciarGestion  = do
    hSetBuffering stdout NoBuffering
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

            putStrLn "Ingrese su contraseña: "
            password <- getLine
            
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
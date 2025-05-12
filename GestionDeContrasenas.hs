import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
--import Data.Aeson

data Credencial = Credencial {
    titulo :: String,
    usuario :: String,
    password :: String
} deriving (Eq, Show) --Falta agregar Aeson

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "\n--------------------------------"
    putStrLn "Gestión de Contraseñas"
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
            main

        "2" -> do
            putStrLn "\n--------------------------------"
            putStrLn "Ingrese un servicio: "
            texto <- getLine

            main

        "3" -> do
            putStrLn "\n--------------------------------"
            putStrLn "Ingrese el nombre de la cuenta: "
            texto <- getLine
            
            main
            
        "4" -> do
            putStrLn "\n--------------------------------"
            putStrLn "Ingrese un servicio: "
            servicio <- getLine

            putStrLn "Ingrese el usuario: "
            usuario <- getLine

            putStrLn "Ingrese su contraseña: "
            password <- getLine
            
            main
            
        "5" -> do
            putStrLn "\n--------------------------------"
            putStrLn "Ingrese un servicio: "
            servicio <- getLine

            putStrLn "Ingrese el dato a modificar (usuario/contraseña): "
            dato <- getLine
            
            main

        "6" -> do
            putStrLn "\n--------------------------------"
            putStrLn "Ingrese un servicio: "
            servicio <- getLine
            
            main

        "7" -> putStrLn "Saliendo del programa..."
        
        _ -> do
            putStrLn "Opción no válida. Intente nuevamente."
            main
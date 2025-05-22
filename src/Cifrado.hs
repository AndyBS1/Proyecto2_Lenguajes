{-# LANGUAGE OverloadedStrings #-}

module Cifrado (cifrar, descifrar) where

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..), makeIV, IV, cbcEncrypt, cbcDecrypt)
import Crypto.Error (CryptoFailable(..))
import Data.ByteArray (convert)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64 as B64

-- Clave de 32 bytes 
clave :: BS.ByteString
clave = "xT7$1wqzD8!kV0cPb@F9lMjx#3uRz1Wa"

-- Vector de inicialización
ivBytes :: BS.ByteString
ivBytes = "Z1u2b3Nc4Vr5Ty6H"

cifrar :: String -> String
cifrar texto =
    let textoBS = aplicarPadding $ BS.pack texto
        CryptoPassed cipher = cipherInit clave :: CryptoFailable AES256
        Just iv = makeIV ivBytes
        cifrado = cbcEncrypt cipher iv textoBS -- cbcEncrypt cifra los datos
    in BS.unpack $ B64.encode cifrado --B64 para poder almacenarlo como texto

descifrar :: String -> String
descifrar textoBase64 =
    let textoCifrado = B64.decodeLenient $ BS.pack textoBase64
        CryptoPassed cipher = cipherInit clave :: CryptoFailable AES256
        Just iv = makeIV ivBytes
        textoPlano = cbcDecrypt cipher iv textoCifrado -- cbcDecrypt descifra los datos
    in BS.unpack $ eliminarPadding textoPlano

-- Función para aplicar relleno al texto para que su longitud sea múltiplo de 16 bytes. (AES)
aplicarPadding :: BS.ByteString -> BS.ByteString
aplicarPadding texto =
    let tamañoBloque = 16
        largoTexto = BS.length texto
        cantidadRelleno = tamañoBloque - (largoTexto `mod` tamañoBloque)
        byteRelleno = toEnum cantidadRelleno  -- byte de relleno para indicar cuantos se agregaron
        relleno = BS.replicate cantidadRelleno byteRelleno
    in texto <> relleno  -- junta el texto con el relleno

-- Función para eliminar el relleno aplicado previamente
eliminarPadding :: BS.ByteString -> BS.ByteString
eliminarPadding textoConRelleno =
    let byteFinal = BS.last textoConRelleno -- byte que indica cuántos bytes de relleno hay (último)
        cantidadRelleno = fromEnum byteFinal
        largoTexto = BS.length textoConRelleno
    in BS.take (largoTexto - cantidadRelleno) textoConRelleno
{- git-annex crypto
 -
 - Currently using gpg; could later be modified to support different
 - crypto backends if neccessary.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Crypto (
	genCipher,
	updateCipher,
	storeCipher,
	extractCipher,
	decryptCipher,		
	encryptKey,
	encryptContent,
	decryptContent
) where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M
import qualified Codec.Binary.Base64 as B64
import System.Cmd.Utils
import Data.String.Utils
import Data.List
import Data.Bits.Utils

import Types
import RemoteClass
import Utility

data Cipher = Cipher String -- XXX ideally, this would be a locked memory region

data EncryptedCipher = EncryptedCipher String KeyIds

data KeyIds = KeyIds [String]

instance Show KeyIds where
	show (KeyIds ks) = join "," ks

instance Read KeyIds where
	readsPrec _ s = [(KeyIds (split "," s), "")]

{- Creates a new Cipher, encrypted as specified in the remote's configuration -}
genCipher :: RemoteConfig -> IO EncryptedCipher
genCipher c = do
	ks <- configKeyIds c
	random <- genrandom
	encryptCipher (Cipher random) ks
	where
		genrandom = gpgPipeRead
			[ Params "--gen-random"
			, Param $ show randomquality
			, Param $ show ciphersize
			]
		randomquality = 1 :: Int -- 1 is /dev/urandom; 2 is /dev/random
		ciphersize = 1024 :: Int

{- Updates an existing Cipher, re-encrypting it to add KeyIds specified in
 - the remote's configuration. -}
updateCipher :: RemoteConfig -> EncryptedCipher -> IO EncryptedCipher
updateCipher c encipher@(EncryptedCipher _ ks) = do
	ks' <- configKeyIds c
	cipher <- decryptCipher c encipher
	encryptCipher cipher (combine ks ks')
	where
		combine (KeyIds a) (KeyIds b) = KeyIds $ a ++ b

{- Stores an EncryptedCipher in a remote's configuration. -}
storeCipher :: RemoteConfig -> EncryptedCipher -> RemoteConfig
storeCipher c (EncryptedCipher t ks) = 
	M.insert "cipher" (toB64 t) $ M.insert "cipherkeys" (show ks) c
	where
		toB64 = B64.encode . s2w8

{- Extracts an EncryptedCipher from a remote's configuration. -}
extractCipher :: RemoteConfig -> Maybe EncryptedCipher
extractCipher c = 
	case (M.lookup "cipher" c, M.lookup "cipherkeys" c) of
		(Just t, Just ks) -> Just $ EncryptedCipher (fromB64 t) (read ks)
		_ -> Nothing
	where
		fromB64 s = case B64.decode s of
			Nothing -> error "bad base64 encoded cipher in remote config"
			Just ws -> w82s ws

{- Encrypts a Cipher to the specified KeyIds. -}
encryptCipher :: Cipher -> KeyIds -> IO EncryptedCipher
encryptCipher (Cipher c) (KeyIds ks) = do
	let ks' = nub $ sort ks -- gpg complains about duplicate recipient keyids
	encipher <- gpgPipeBoth (encrypt++recipients ks') c
	return $ EncryptedCipher encipher (KeyIds ks')
	where
		encrypt = [ Params "--encrypt" ]
		recipients l = 
			-- Force gpg to only encrypt to the specified
			-- recipients, not configured defaults.
			[ Params "--no-encrypt-to --no-default-recipient"] ++
			(concat $ map (\k -> [Param "--recipient", Param k]) l)

{- Decrypting an EncryptedCipher is expensive; the Cipher should be cached. -}
decryptCipher :: RemoteConfig -> EncryptedCipher -> IO Cipher
decryptCipher _ (EncryptedCipher encipher _) = 
	return . Cipher =<< gpgPipeBoth decrypt encipher
	where
		decrypt = [ Params "--decrypt" ]

{- Genetates an encrypted form of a Key. The enctyption does not need to be
 - reversable, nor does it need to be the same type of encryption used
 - on content. -}
encryptKey :: Cipher -> Key -> IO Key
encryptKey = error "TODO"

{- Streams content, encrypting. -}
encryptContent :: Cipher -> L.ByteString -> IO L.ByteString
encryptContent = error "TODO"

{- Streams encrypted content, decrypting. -}
decryptContent :: Cipher -> L.ByteString -> IO L.ByteString
decryptContent = error "TODO"


gpgParams :: [CommandParam] -> [String]
gpgParams params =
	-- avoid console IO, and be quiet, even about checking the trustdb
	["--batch", "--quiet", "--trust-model", "always"] ++
	toCommand params

gpgPipeRead :: [CommandParam] -> IO String
gpgPipeRead params = pOpen ReadFromPipe "gpg" (gpgParams params) hGetContentsStrict

gpgPipeBoth :: [CommandParam] -> String -> IO String
gpgPipeBoth params input = do
	(_, s) <- pipeBoth "gpg" (gpgParams params) input
	return s

configKeyIds :: RemoteConfig -> IO KeyIds
configKeyIds c = do
	let k = configGet c "encryption"
	s <- gpgPipeRead [Params "--with-colons --list-public-keys", Param k]
	return $ KeyIds $ parseWithColons s
	where
		parseWithColons s = map keyIdField $ filter pubKey $ lines s
		pubKey = isPrefixOf "pub:"
		keyIdField s = (split ":" s) !! 4

configGet :: RemoteConfig -> String -> String
configGet c key =
	case M.lookup key c of
		Just v -> v
		Nothing -> error $ "missing " ++ key ++ " in remote config"

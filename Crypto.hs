{- git-annex crypto
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
import System.IO
import System.Cmd.Utils

import Types
import RemoteClass
import Utility

data Cipher = Cipher String -- XXX ideally, this would be a locked memory region
data EncryptedCipher = EncryptedCipher String
	deriving Show

{- Creates a new Cipher, encrypted as specified in the remote's configuration -}
genCipher :: RemoteConfig -> IO EncryptedCipher
genCipher config = do
	random <- genrandom
	encryptCipher config $ Cipher random
	where
		genrandom = gpgPipeRead
			[ Params "--armor --gen-random"
			, Param $ show randomquality
			, Param $ show ciphersize
			]
		randomquality = 1 -- 1 is /dev/urandom; 2 is /dev/random
		ciphersize = 1024

{- Updates an existing Cipher, re-encrypting it as specified in the
 - remote's configuration  -}
updateCipher :: RemoteConfig -> EncryptedCipher -> IO EncryptedCipher
updateCipher config encipher = do
	cipher <- decryptCipher config encipher
	encryptCipher config cipher

{- Stores an EncryptedCipher in a remote's configuration. -}
storeCipher :: RemoteConfig -> EncryptedCipher -> RemoteConfig
storeCipher config (EncryptedCipher c) = M.insert "cipher" c config

{- Extracts an EncryptedCipher from a remote's configuration. -}
extractCipher :: RemoteConfig -> EncryptedCipher
extractCipher config = case M.lookup "cipher" config of
	Just c -> EncryptedCipher c
	Nothing -> error "missing cipher in remote config"

{- Encryptes a Cipher as specified by a remote's configuration. -}
encryptCipher :: RemoteConfig -> Cipher -> IO EncryptedCipher
encryptCipher config (Cipher c) = do
	encipher <- gpgPipeBoth (encrypt++recipient) c
	return $ EncryptedCipher encipher
	where
		encrypt =
			[ Params "--encrypt --armor"
			, Params "--trust-model always"
			]
		recipient = case M.lookup "encryption" config of
			Nothing -> [ Param "--default-recipient-self" ]
			Just r ->
				-- Force gpg to only encrypt to the specified
				-- recipients, not configured defaults.
				[ Params "--no-encrypt-to --no-default-recipient"
				, Param "--recipient"
				, Param r
				]

{- Decrypting an EncryptedCipher is expensive; the Cipher should be cached. -}
decryptCipher :: RemoteConfig -> EncryptedCipher -> IO Cipher
decryptCipher = error "TODO"

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
gpgParams params = ["--batch", "--quiet"] ++ toCommand params

gpgPipeRead :: [CommandParam] -> IO String
gpgPipeRead params = pOpen ReadFromPipe "gpg" (gpgParams params) hGetContentsStrict

gpgPipeBoth :: [CommandParam] -> String -> IO String
gpgPipeBoth params input = do
	(_, s) <- pipeBoth "gpg" (gpgParams params) input
	return s

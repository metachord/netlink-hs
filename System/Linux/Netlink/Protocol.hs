module System.Linux.Netlink.Protocol where

import Prelude hiding (length)

import Control.Applicative ((<$>))
import Data.ByteString (ByteString, length)
import Data.Serialize (Serialize(..))
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Word (Word32)
import Foreign.C.Error (Errno(..), errnoToIOError)
import Foreign.C.Types (CInt)

import System.Linux.Netlink.C

data Message = GetInterface ArpHardware Word32 Word32
             | NewInterface ArpHardware Word32 Word32
             | Success
             | Error CInt String
             | Other MessageType
             deriving (Eq, Show)

instance Serialize Message where
    put (GetInterface deviceType interfaceIndex linkFlags) =
        putPacket MSG_GetLink [MSG_F_Request] $ runPut $ do
            putWord8 (_fromEnum Unspec)
            putWord8 0
            putWord16host (_fromEnum deviceType)
            putWord32host interfaceIndex
            putWord32host linkFlags
            putWord32host 0xFFFFFFFF
    
    put _ = fail "Unsupported userspace->kernel message type"

    get = do
        skip 4
        msgTy <- _toEnum <$> getWord16host
        skip 10
        case msgTy of
            MSG_Error   -> getError 
            MSG_GetLink -> getInterface
            MSG_NewLink -> newInterface
            _           -> return $ Other msgTy
      where
        getError = do
            code <- negate . fromIntegral <$> getWord32host
            let err = errnoToIOError "recvmsg" (Errno code) Nothing Nothing
                errStr = show err
            if code == 0
                then return Success
                else return $ Error code errStr

        getInterface = do
            skip 2
            deviceType <- _toEnum <$> getWord16host
            interfaceIndex <- getWord32host
            flags <- getWord32host
            return $ GetInterface deviceType interfaceIndex flags
            
        newInterface = do
            skip 2
            deviceType <- _toEnum <$> getWord16host
            interfaceIndex <- getWord32host
            flags <- getWord32host
            return $ NewInterface deviceType interfaceIndex flags

putPacket :: MessageType -> [MessageFlags] -> ByteString -> Put
putPacket msgTy flags payload = do
    putWord32host . fromIntegral $ (length payload) + 16
    putWord16host (_fromEnum msgTy)
    putWord16host (setFlags flags)
    putWord32host 1
    putWord32host 0
    putByteString payload

_fromEnum :: (Enum a, Num b) => a -> b
_fromEnum = fromIntegral . fromEnum

_toEnum :: (Integral a, Enum b) => a -> b
_toEnum = toEnum . fromIntegral

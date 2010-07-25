module System.Linux.Netlink.Protocol where

import Prelude hiding (length)

import Control.Applicative ((<$>))
import Data.ByteString (ByteString, length)
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Word (Word8, Word16, Word32)
import Foreign.C.Error (Errno(..))

import System.Linux.Netlink.C

data Header = Header
    {
      messageType   :: MessageType
    , messageFlags  :: MessageFlags
    , messageSeqNum :: Word32
    , messagePID    :: Word32
    } deriving (Eq, Show)

data Message = ErrorMsg
    {
      errorCode   :: Errno
    , errorHeader :: Header
    }
             | LinkMsg
    {
      interfaceType  :: LinkType
    , interfaceIndex :: Word32
    , interfaceFlags :: Word32
    }
             | AddrMsg
    {
      addrFamily         :: AddressFamily
    , addrMaskLength     :: Word8
    , addrFlags          :: Word8
    , addrScope          :: Word8
    , addrInterfaceIndex :: Word32
    }
             deriving (Eq)

data Attributes = Attributes deriving (Eq, Show)

type Packet = (Header, Message, Attributes)

--
-- Packet decoding
--
getPacket :: Get Packet
getPacket = do
    header <- getHeader
    msg    <- getMessage (messageType header)
    attrs  <- getAttributes
    return (header, msg, attrs)

getHeader :: Get Header
getHeader = do
    skip 4
    ty     <- cToEnum <$> g16
    flags  <- cToEnum <$> g16
    seqnum <- g32
    pid    <- g32
    return $ Header ty flags seqnum pid

getMessage :: MessageType -> Get Message
getMessage NlmsgError = getMessageError
getMessage RtmNewlink = getMessageLink
getMessage RtmDellink = getMessageLink
getMessage RtmGetlink = getMessageLink
getMessage RtmNewaddr = getMessageAddr
getMessage RtmDeladdr = getMessageAddr
getMessage RtmGetaddr = getMessageAddr
getMessage a          = error $ "Can't decode message " ++ show a

getMessageError :: Get Message
getMessageError = do
    code <- Errno . abs . fromIntegral <$> g32
    hdr  <- getHeader
    return $ ErrorMsg code hdr

getMessageLink :: Get Message
getMessageLink = do
    skip 2
    ty    <- cToEnum <$> getWord16be
    idx   <- g32
    flags <- g32
    skip 4
    return $ LinkMsg ty idx flags

getMessageAddr :: Get Message
getMessageAddr = do
    fam <- cToEnum <$> g8
    maskLen <- g8
    flags <- g8
    scope <- cToEnum <$> g8
    idx <- g32
    return $ AddrMsg fam maskLen flags scope idx

getAttributes :: Get Attributes
getAttributes = return Attributes

--
-- Packet serialization
--
putPacket :: Packet -> [ByteString]
putPacket (header, message, attributes) =
    let attrs  = runPut $ putAttributes attributes
        msg    = runPut $ putMessage message
        hdr = runPut $ putHeader (length msg + length attrs + 16) header
    in [hdr, msg, attrs]

putHeader :: Int -> Header -> Put
putHeader len (Header ty flags seqnum pid) = do
    p32 (fromIntegral len)
    p16 (cFromEnum ty)
    p16 (cFromEnum flags)
    p32 seqnum
    p32 pid

putMessage :: Message -> Put
putMessage (LinkMsg ty idx flags) = do
    p8 (cFromEnum AfUnspec) >> p8 0
    p16 (cFromEnum ty)
    p32 idx
    p32 flags
    p32 0xFFFFFFFF
putMessage (AddrMsg fam maskLen flags scope idx) = do
    p8 (cFromEnum fam)
    p8 maskLen
    p8 flags
    p8 (cFromEnum scope)
    p32 idx
putMessage _ = error "Can't transmit this message to the kernel."

putAttributes :: Attributes -> Put
putAttributes _ = p8 0

--
-- Helpers
--
p8 :: Word8 -> Put
p8  = putWord8
p16 :: Word16 -> Put
p16 = putWord16host
p32 :: Word32 -> Put
p32 = putWord32host

g8 :: Get Word8
g8  = getWord8
g16:: Get Word16
g16 = getWord16host
g32 :: Get Word32
g32 = getWord32host

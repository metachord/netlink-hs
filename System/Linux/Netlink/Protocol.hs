module System.Linux.Netlink.Protocol 
    (
      Header(..)
    , Message(..)
    , Attributes
    , Packet(..)
    
    , getPacket
    , putPacket
    ) where

import Prelude hiding (length)

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.Loops (whileM)
import Data.ByteString (ByteString, length)
import Data.Map (Map, fromList, toList)
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Word (Word8, Word16, Word32)
import Foreign.C.Error (Errno(..))
import Foreign.C.Types (CInt)
import System.IO.Unsafe (unsafePerformIO)

import System.Linux.Netlink.C

data Header = Header
    {
      messageType   :: MessageType
    , messageFlags  :: Word16
    , messageSeqNum :: Word32
    , messagePID    :: Word32
    } deriving (Eq, Show)

data Message = DoneMsg
             | ErrorMsg
    {
      errorCode    :: CInt
    , errorPacket  :: Packet
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
    } deriving (Eq, Show)

type Attributes = Map Int ByteString

data Packet = Packet
    {
      packetHeader     :: Header 
    , packetMessage    :: Message
    , packetAttributes :: Attributes
    } deriving (Eq, Show)

--
-- Packet decoding
--
getPacket :: ByteString -> Either String [Packet]
getPacket bytes = flip runGet bytes $ do
    pkts <- whileM (not <$> isEmpty) getPacketInternal
    isEmpty >>= \e -> when (not e) $ fail "Incomplete message parse"
    return pkts

getPacketInternal :: Get Packet
getPacketInternal = do
    len <- fromIntegral <$> g32
    isolate (len - 4) $ do
        header <- getHeader
        msg    <- getMessage (messageType header)
        attrs <- whileM (not <$> isEmpty) getSingleAttribute
        return $ Packet header msg (fromList attrs)

getHeader :: Get Header
getHeader = do
    ty     <- cToEnum <$> g16
    flags  <- cToEnum <$> g16
    seqnum <- g32
    pid    <- g32
    return $ Header ty flags seqnum pid

getMessage :: MessageType -> Get Message
getMessage NlmsgDone  = skip 4 >> return DoneMsg
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
    code <- abs . fromIntegral <$> g32
    pkt  <- getPacketInternal
    return $ ErrorMsg code pkt

getMessageLink :: Get Message
getMessageLink = do
    skip 2
    ty    <- cToEnum <$> g16
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
getAttributes = fromList <$> run 0
  where
    run preSkip = isEmpty >>= \e -> if e then return [] else do
        when (preSkip > 0) $ skip preSkip
        -- RFC violation: RFC says type, then length, with length of
        -- value only. Reality says length of everything including
        -- length bytes, then type, then value.
        len <- fromIntegral <$> g16
        ty <- fromIntegral <$> g16
        val <- getByteString (len - 4)
        -- Another spec violation: attributes are 32-bit aligned. The
        -- spec says nothing about that.
        ((ty, val) :) <$> run (4 - (len `mod` 4))

getSingleAttribute :: Get (Int, ByteString)
getSingleAttribute = do
    len <- fromIntegral <$> g16
    ty <- fromIntegral <$> g16
    val <- getByteString (len - 4)
    isEmpty >>= \e -> when (not e && len `mod` 4 /= 0) $ skip (4 - (len `mod` 4))
    return (ty, val)

--
-- Packet serialization
--
putPacket :: Packet -> [ByteString]
putPacket (Packet header message attributes) =
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
putMessage DoneMsg = p32 0
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
putAttributes = mapM_ putAttr . toList
  where
    putAttr (ty, value) = do
        p16 (fromIntegral ty)
        p16 (fromIntegral . length $ value)
        putByteString value

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

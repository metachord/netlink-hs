{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module System.Linux.Netlink.C
    (
      NetlinkSocket
    , makeSocket
    , sendmsg
    , recvmsg

    , Flags(..)
      
    , MessageType(..)
    , MessageFlags(..)
    , LinkType(..)
    , LinkFlags(..)
    , AddressFamily(..)

    , cFromEnum
    , cToEnum
    ) where

import Control.Applicative ((<$>), (<*))
import Control.Monad (when)
import Data.Bits (Bits, (.&.), (.|.), complement, shiftL)
import Data.ByteString (ByteString)
import Data.ByteString.Internal (createAndTrim, toForeignPtr)
import Data.Unique (hashUnique, newUnique)
import Data.Word (Word32)
import Foreign.C.Error (throwErrnoIf, throwErrnoIfMinus1, throwErrnoIfMinus1_)
import Foreign.C.Types (CInt, CUInt, CUShort)
import Foreign.ForeignPtr (touchForeignPtr, unsafeForeignPtrToPtr)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable(..))
import System.Posix.Process (getProcessID)

#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>

#include "enums.h"

#c
typedef struct msghdr msdhdr;
typedef struct iovec iovec;
typedef struct sockaddr_nl sockaddr_nl;
#endc

newtype NetlinkSocket = NS CInt

makeSocket :: IO NetlinkSocket
makeSocket = do
    fd <- throwErrnoIfMinus1 "makeSocket.socket" $
          ({#call socket #}
           (cFromEnum AfNetlink)
           (cFromEnum Raw)
           (cFromEnum Route))
    unique <- fromIntegral . hashUnique <$> newUnique
    pid <- fromIntegral <$> getProcessID
    let sockId = (unique `shiftL` 16) .|. pid
    with (SockAddrNetlink sockId) $ \addr ->
        throwErrnoIfMinus1_ "makeSocket.bind" $ do
            {#call bind #} fd (castPtr addr) {#sizeof sockaddr_nl #}
    return $ NS fd

sendmsg :: NetlinkSocket -> [ByteString] -> IO ()
sendmsg (NS fd) bs =
    useManyAsPtrLen bs $ \ptrs ->
    withArrayLen (map IoVec ptrs) $ \iovlen iov ->
    with (MsgHdr (castPtr iov, iovlen)) $ \msg ->
    throwErrnoIfMinus1_ "sendmsg" $ do
        {#call sendmsg as _sendmsg #} fd (castPtr msg) (0 :: CInt)

recvmsg :: NetlinkSocket -> Int -> IO ByteString
recvmsg (NS fd) len = 
    createAndTrim len $ \ptr ->
    with (IoVec (castPtr ptr, len)) $ \vec ->
    with (MsgHdr (castPtr vec, 1)) $ \msg ->
    fmap fromIntegral . throwErrnoIf (<= 0) "recvmsg" $ do
        {#call recvmsg as _recvmsg #} fd (castPtr msg) (0 :: CInt)

{#enum define PF { NETLINK_ROUTE as Route } #}
{#enum define ST { SOCK_RAW as Raw } #}

{#enum AddressFamily {} deriving (Eq, Show) #}
{#enum MessageType {} deriving (Eq, Show) #}
{#enum MessageFlags {} deriving (Eq, Show) #}
{#enum LinkType {} deriving (Eq, Show) #}
{#enum LinkFlags {} deriving (Eq, Show) #}
{#enum LinkAttrType {} deriving (Eq, Show) #}
{#enum AddrFlags {} deriving (Eq, Show) #}
{#enum rt_scope_t as Scope { underscoreToCase
                           , upcaseFirstLetter} deriving (Eq, Show) #}
{#enum define AddrAttrType { IFA_UNSPEC as IfaUnspec
                           , IFA_ADDRESS as IfaAddress
                           , IFA_LOCAL as IfaLocal
                           , IFA_LABEL as IfaLabel
                           , IFA_BROADCAST as IfaBroadcast
                           , IFA_ANYCAST as IfaAnycast
                           , IFA_CACHEINFO as IfaCacheinfo
                           , IFA_MULTICAST as IfaMulticast
                           } deriving (Eq, Show) #}
{#enum rt_class_t as RouteTableId { underscoreToCase
                                  , upcaseFirstLetter } deriving (Eq, Show) #}
{#enum RouteProto {} deriving (Eq, Show) #}
{#enum define RouteType { RTN_UNSPEC as RtnUnspec
                        , RTN_UNICAST as RtnUnicast
                        , RTN_LOCAL as RtnLocal
                        , RTN_BROADCAST as RtnBroadcast
                        , RTN_ANYCAST as RtnAnycast
                        , RTN_MULTICAST as RtnMulticast
                        , RTN_BLACKHOLE as RtnBlackhole
                        , RTN_UNREACHABLE as RtnUnreachable
                        , RTN_PROHIBIT as RtnProhibit
                        , RTN_THROW as RtnThrow
                        , RTN_NAT as RtnNat
                        , RTN_XRESOLVE as RtnXresolve } deriving (Eq, Show) #}
{#enum RouteFlags {} deriving (Eq, Show) #}
{#enum rtattr_type_t as RouteAttrType { underscoreToCase
                                      , upcaseFirstLetter } deriving (Eq, Show) #}

-- Enumerations of flags used in netlink requests/responses.
class Enum a => Flags a where
    setFlags :: Num b => [a] -> b
    setFlags = sum . map (fromIntegral . fromEnum)

    isFlagSet :: Bits b => a -> b -> Bool
    isFlagSet flag n = n .&. fromIntegral (fromEnum flag) /= 0

    setFlag :: (Integral b, Bits b) => a -> b -> b
    setFlag f v = v .|. (cFromEnum f)

    clearFlag :: (Integral b, Bits b) => a -> b -> b
    clearFlag f v = v .&. complement (cFromEnum f)

instance Flags MessageFlags
instance Flags LinkFlags
instance Flags AddrFlags
instance Flags RouteFlags

data IoVec = IoVec (Ptr (), Int)

instance Storable IoVec where
    sizeOf    _ = {#sizeof iovec #}
    alignment _ = 4
    peek p = do
        addr <- {#get iovec.iov_base #} p
        len  <- {#get iovec.iov_len #}  p
        return $ IoVec (addr, (fromIntegral len))
    poke p (IoVec (addr, len)) = do
        zero p
        {#set iovec.iov_base #} p addr
        {#set iovec.iov_len  #} p (fromIntegral len)

data MsgHdr = MsgHdr (Ptr (), Int)

instance Storable MsgHdr where
    sizeOf    _ = {#sizeof iovec #}
    alignment _ = 4
    peek p = do
        iov     <- {#get msghdr.msg_iov     #} p
        iovlen  <- {#get msghdr.msg_iovlen  #} p
        return $ MsgHdr (iov, fromIntegral iovlen)
    poke p (MsgHdr (iov, iovlen)) = do
        zero p
        {#set msghdr.msg_iov     #} p iov
        {#set msghdr.msg_iovlen  #} p (fromIntegral iovlen)

data SockAddrNetlink = SockAddrNetlink Word32

instance Storable SockAddrNetlink where
    sizeOf    _ = {#sizeof sockaddr_nl #}
    alignment _ = 4
    peek p = do
        family <- cToEnum <$> {#get sockaddr_nl.nl_family #} p
        when (family /= AfNetlink) $ fail "Bad address family"
        SockAddrNetlink . fromIntegral <$> {#get sockaddr_nl.nl_pid #} p
    poke p (SockAddrNetlink pid) = do
        zero p
        {#set sockaddr_nl.nl_family #} p (cFromEnum AfNetlink)
        {#set sockaddr_nl.nl_pid    #} p (fromIntegral pid)

useManyAsPtrLen :: [ByteString] -> ([(Ptr (), Int)] -> IO a) -> IO a
useManyAsPtrLen bs act =
    let makePtrLen (fptr, off, len) =
            let ptr = plusPtr (unsafeForeignPtrToPtr fptr) off
            in (ptr, len)
        touchByteStringPtr (fptr, _, _) = touchForeignPtr fptr
        foreigns = map toForeignPtr bs
    in act (map makePtrLen foreigns) <* mapM_ touchByteStringPtr foreigns

sizeOfPtr :: (Storable a, Integral b) => Ptr a -> b
sizeOfPtr = fromIntegral . sizeOf . (undefined :: Ptr a -> a)

zero :: Storable a => Ptr a -> IO ()
zero p = void $ {#call memset #} (castPtr p) 0 (sizeOfPtr p)

void :: Monad m => m a -> m ()
void act = act >> return ()

cFromEnum :: (Enum e, Integral i) => e -> i
cFromEnum = fromIntegral . fromEnum

cToEnum :: (Integral i, Enum e) => i -> e
cToEnum = toEnum . fromIntegral

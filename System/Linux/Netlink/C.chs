{-# LANGUAGE ForeignFunctionInterface #-}

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
    ) where

import C2HS
import Control.Applicative ((<$>), (<*))
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString.Internal (createAndTrim, toForeignPtr)
import Data.Unique (hashUnique, newUnique)
import System.Posix.Process (getProcessID)

#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>

#c
typedef int64_t aligned_u64;
#endc

#include "../../../../../cbits/enums.h"

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
    fmap cIntConv . throwErrnoIf (<= 0) "recvmsg" $ do
        {#call recvmsg as _recvmsg #} fd (castPtr msg) (0 :: CInt)

{#enum define PF { NETLINK_ROUTE as Route } #}
{#enum define ST { SOCK_RAW as Raw } #}

{#enum MessageType {} deriving (Eq, Show) #}
{#enum MessageFlags {} deriving (Eq, Show) #}
{#enum LinkType {} deriving (Eq, Show) #}
{#enum LinkFlags {} deriving (Eq, Show) #}
{#enum AddressFamily {} deriving (Eq, Show) #}

-- Enumerations of flags used in netlink requests/responses.
class Enum a => Flags a where
    setFlags :: Num b => [a] -> b
    setFlags = sum . map (fromIntegral . fromEnum)

    flagSet :: Bits b => a -> b -> Bool
    flagSet flag n = n .&. fromIntegral (fromEnum flag) /= 0

instance Flags MessageFlags
instance Flags LinkFlags

data IoVec = IoVec (Ptr (), Int)

instance Storable IoVec where
    sizeOf    _ = {#sizeof iovec #}
    alignment _ = 4
    peek p = do
        addr <- {#get iovec.iov_base #} p
        len  <- {#get iovec.iov_len #}  p
        return $ IoVec (addr, (cIntConv len))
    poke p (IoVec (addr, len)) = do
        zero p
        {#set iovec.iov_base #} p addr
        {#set iovec.iov_len  #} p (cIntConv len)

data MsgHdr = MsgHdr (Ptr (), Int)

instance Storable MsgHdr where
    sizeOf    _ = {#sizeof iovec #}
    alignment _ = 4
    peek p = do
        iov     <- {#get msghdr.msg_iov     #} p
        iovlen  <- {#get msghdr.msg_iovlen  #} p
        return $ MsgHdr (iov, cIntConv iovlen)
    poke p (MsgHdr (iov, iovlen)) = do
        zero p
        {#set msghdr.msg_iov     #} p iov
        {#set msghdr.msg_iovlen  #} p (cIntConv iovlen)

data SockAddrNetlink = SockAddrNetlink Word32

instance Storable SockAddrNetlink where
    sizeOf    _ = {#sizeof sockaddr_nl #}
    alignment _ = 4
    peek p = do
        family <- cToEnum <$> {#get sockaddr_nl.nl_family #} p
        when (family /= AfNetlink) $ fail "Bad address family"
        SockAddrNetlink . cIntConv <$> {#get sockaddr_nl.nl_pid #} p
    poke p (SockAddrNetlink pid) = do
        zero p
        {#set sockaddr_nl.nl_family #} p (cFromEnum AfNetlink)
        {#set sockaddr_nl.nl_pid    #} p (cIntConv pid)

useManyAsPtrLen :: [ByteString] -> ([(Ptr (), Int)] -> IO a) -> IO a
useManyAsPtrLen bs act =
    let makePtrLen (fptr, off, len) =
            let ptr = plusPtr (unsafeForeignPtrToPtr fptr) off
            in (ptr, len)
        touchByteStringPtr (fptr, _, _) = touchForeignPtr fptr
        foreigns = map toForeignPtr bs
    in act (map makePtrLen foreigns) <* mapM_ touchByteStringPtr foreigns

sizeOfPtr :: (Storable a, Integral b) => Ptr a -> b
sizeOfPtr = cIntConv . sizeOf . (undefined :: Ptr a -> a)

zero :: Storable a => Ptr a -> IO ()
zero p = void $ {#call memset #} (castPtr p) 0 (sizeOfPtr p)

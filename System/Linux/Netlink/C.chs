{-# LANGUAGE ForeignFunctionInterface #-}

module System.Linux.Netlink.C
    (
      NetlinkSocket
    , makeSocket
    , sendmsg
    , recvmsg

    , AddressFamily(..)
    , MessageType(..)
    , ArpHardware(..)

    , Flags(..)
    , MessageFlags(..)
    , LinkFlags(..)
    ) where

import C2HS
import Control.Applicative ((<$>), (<*))
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString.Internal (createAndTrim, toForeignPtr)
import Data.Unique (hashUnique, newUnique)
import System.Posix.Process (getProcessID)

#include <sys/types.h>
#include <sys/socket.h>
#include <linux/if.h>
#include <linux/if_arp.h>
#include <linux/netlink.h>
#include <linux/rtnetlink.h>
#include <string.h>

#c
typedef struct msghdr msdhdr;
typedef struct iovec iovec;
typedef struct sockaddr_nl sockaddr_nl;
#endc

newtype NetlinkSocket = NS CInt

makeSocket :: IO NetlinkSocket
makeSocket = do
    fd <- throwErrnoIfMinus1 "makeSocket.socket" $
          {#call socket #} (cFromEnum Netlink) (cFromEnum Raw) (cFromEnum Route)
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
{#enum define AddressFamily { AF_NETLINK as Netlink
                            , AF_INET    as Inet
                            , AF_INET6   as Inet6
                            , AF_UNSPEC  as Unspec } deriving (Eq, Show) #}
{#enum define ST { SOCK_RAW   as Raw } #}

{#enum define MessageType { NLMSG_ERROR  as MSG_Error
                          , RTM_NEWLINK  as MSG_NewLink
                          , RTM_DELLINK  as MSG_DelLink
                          , RTM_GETLINK  as MSG_GetLink
                          , RTM_NEWADDR  as MSG_NewAddr
                          , RTM_DELADDR  as MSG_DelAddr
                          , RTM_GETADDR  as MSG_GetAddr
                          , RTM_NEWROUTE as MSG_NewRoute
                          , RTM_DELROUTE as MSG_DelRoute
                          , RTM_GETROUTE as MSG_GetRoute
                          } deriving (Eq, Show) #}

{#enum define ArpHardware { ARPHRD_NETROM as ARP_NETROM
                          , ARPHRD_ETHER as ARP_ETHER
                          , ARPHRD_EETHER as ARP_EETHER
                          , ARPHRD_AX25 as ARP_AX25
                          , ARPHRD_PRONET as ARP_PRONET
                          , ARPHRD_CHAOS as ARP_CHAOS
                          , ARPHRD_IEEE802 as ARP_IEEE802
                          , ARPHRD_ARCNET as ARP_ARCNET
                          , ARPHRD_APPLETLK as ARP_APPLETLK
                          , ARPHRD_DLCI as ARP_DLCI
                          , ARPHRD_ATM as ARP_ATM
                          , ARPHRD_METRICOM as ARP_METRICOM
                          , ARPHRD_IEEE1394 as ARP_IEEE1394
                          , ARPHRD_EUI64 as ARP_EUI64
                          , ARPHRD_INFINIBAND as ARP_INFINIBAND
                          , ARPHRD_SLIP as ARP_SLIP
                          , ARPHRD_CSLIP as ARP_CSLIP
                          , ARPHRD_SLIP6 as ARP_SLIP6
                          , ARPHRD_CSLIP6 as ARP_CSLIP6
                          , ARPHRD_RSRVD as ARP_RSRVD
                          , ARPHRD_ADAPT as ARP_ADAPT
                          , ARPHRD_ROSE as ARP_ROSE
                          , ARPHRD_X25 as ARP_X25
                          , ARPHRD_HWX25 as ARP_HWX25
                          , ARPHRD_CAN as ARP_CAN
                          , ARPHRD_PPP as ARP_PPP
                          , ARPHRD_CISCO as ARP_CISCO
                          , ARPHRD_HDLC as ARP_HDLC
                          , ARPHRD_LAPB as ARP_LAPB
                          , ARPHRD_DDCMP as ARP_DDCMP
                          , ARPHRD_RAWHDLC as ARP_RAWHDLC
                          , ARPHRD_TUNNEL as ARP_TUNNEL
                          , ARPHRD_TUNNEL6 as ARP_TUNNEL6
                          , ARPHRD_FRAD as ARP_FRAD
                          , ARPHRD_SKIP as ARP_SKIP
                          , ARPHRD_LOOPBACK as ARP_LOOPBACK
                          , ARPHRD_LOCALTLK as ARP_LOCALTLK
                          , ARPHRD_FDDI as ARP_FDDI
                          , ARPHRD_BIF as ARP_BIF
                          , ARPHRD_SIT as ARP_SIT
                          , ARPHRD_IPDDP as ARP_IPDDP
                          , ARPHRD_IPGRE as ARP_IPGRE
                          , ARPHRD_PIMREG as ARP_PIMREG
                          , ARPHRD_HIPPI as ARP_HIPPI
                          , ARPHRD_ASH as ARP_ASH
                          , ARPHRD_ECONET as ARP_ECONET
                          , ARPHRD_IRDA as ARP_IRDA
                          , ARPHRD_FCPP as ARP_FCPP
                          , ARPHRD_FCAL as ARP_FCAL
                          , ARPHRD_FCPL as ARP_FCPL
                          , ARPHRD_FCFABRIC as ARP_FCFABRIC
                          , ARPHRD_IEEE802_TR as ARP_IEEE802_TR
                          , ARPHRD_IEEE80211 as ARP_IEEE80211
                          , ARPHRD_IEEE80211_PRISM as ARP_IEEE80211_PRISM
                          , ARPHRD_IEEE80211_RADIOTAP as ARP_IEEE80211_RADIOTAP
                          , ARPHRD_IEEE802154 as ARP_IEEE802154
                          , ARPHRD_PHONET as ARP_PHONET
                          , ARPHRD_PHONET_PIPE as ARP_PHONET_PIPE
                          , ARPHRD_VOID as ARP_VOID
                          , ARPHRD_NONE as ARP_NONE
                          } deriving (Eq, Show) #}

-- Enumerations of flags used in netlink requests/responses.
class Enum a => Flags a where
    setFlags :: Num b => [a] -> b
    setFlags = sum . map (fromIntegral . fromEnum)

    flagSet :: Bits b => a -> b -> Bool
    flagSet flag n = n .&. fromIntegral (fromEnum flag) /= 0

{#enum define MessageFlags { NLM_F_REQUEST as MSG_F_Request
                           , NLM_F_ACK     as MSG_F_AckRequest
                           , NLM_F_DUMP    as MSG_F_Dump
                           , NLM_F_REPLACE as MSG_F_Replace
                           , NLM_F_CREATE  as MSG_F_Create
                           } deriving (Eq, Show) #}
instance Flags MessageFlags

{#enum define LinkFlags { IFF_UP          as LINK_F_LinkUp
                        , IFF_BROADCAST   as LINK_F_LinkHasBroadcastAddress
                        , IFF_LOOPBACK    as LINK_F_LinkIsLoopback
                        , IFF_POINTOPOINT as LINK_F_LinkIsPointToPoint
                        , IFF_RUNNING     as LINK_F_LinkRunning
                        , IFF_NOARP       as LINK_F_LinkNoARP
                        , IFF_PROMISC     as LINK_F_LinkIsPromiscuous
                        , IFF_ALLMULTI    as LINK_F_LinkReceivesAllMulticast
                        , IFF_MULTICAST   as LINK_F_LinkSupportsMulticast
                        } deriving (Eq, Show) #}
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
        when (family /= Netlink) $ fail "Bad address family"
        SockAddrNetlink . cIntConv <$> {#get sockaddr_nl.nl_pid #} p
    poke p (SockAddrNetlink pid) = do
        zero p
        {#set sockaddr_nl.nl_family #} p (cFromEnum Netlink)
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

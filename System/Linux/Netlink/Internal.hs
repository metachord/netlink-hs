module System.Linux.Netlink.Internal
    (
      query
    , queryOne
      
    , module System.Linux.Netlink.C
    , module System.Linux.Netlink.Constants
    , module System.Linux.Netlink.Protocol
    ) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.Bits (Bits, (.&.))

import System.Linux.Netlink.C
import System.Linux.Netlink.Constants
import System.Linux.Netlink.Protocol

bufferSize = 8192

recvOne :: NetlinkSocket -> IO [Packet]
recvOne sock = recvmsg sock bufferSize >>= \b -> case (getPacket b) of
    Left err   -> fail err
    Right pkts -> return pkts

recvMulti :: NetlinkSocket -> IO [Packet]
recvMulti sock = do
    pkts <- recvOne sock
    if isMulti (head pkts)
        then if isDone (last pkts)
             then return $ init pkts
             else (pkts ++) <$> recvMulti sock
        else return pkts
  where
    isMulti = isFlagSet fNLM_F_MULTI . messageFlags . packetHeader
    isDone  = (== eNLMSG_DONE) . messageType . packetHeader

query :: NetlinkSocket -> Packet -> IO [Packet]
query sock req = do
    sendmsg sock (putPacket req)
    recvMulti sock

queryOne :: NetlinkSocket -> Packet -> IO Packet
queryOne sock req = do
    sendmsg sock (putPacket req)
    pkts <- recvMulti sock
    let len = length pkts
    when (len /= 1) $ fail ("Expected one packet, received " ++ show len)
    return $ head pkts

isFlagSet :: Bits a => a -> a -> Bool
isFlagSet f v = (f .&. v) == f

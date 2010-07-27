module Main where

import Prelude hiding (length)

import Control.Applicative ((<$>))
import Control.Exception (throwIO)
import Data.ByteString.Char8 (ByteString, length, unpack, pack)
import Data.Map (fromList, empty)
import System.Linux.Netlink.C
import System.Linux.Netlink.Protocol
import Data.Char (ord)

main = do
    sock <- makeSocket

    let flags = setFlags [NlmFRequest, NlmFRoot, NlmFMatch]

    sendmsg sock $ putPacket $ (Header RtmGetlink flags 42 0,
                                LinkMsg ArphrdEther 0 0,
                                empty)
    res <- recvmsg sock 8192
    case getPacket res of
        Left err -> print err
        Right pkts -> mapM_ print pkts

    res <- recvmsg sock 8192
    case getPacket res of
        Left err -> print err
        Right pkts -> mapM_ print pkts

dumpNumeric :: ByteString -> IO ()
dumpNumeric b = print $ unpack b

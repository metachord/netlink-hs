module Main where

import Prelude hiding (length)

import Control.Applicative ((<$>))
import Control.Exception (throwIO)
import Data.ByteString (ByteString, length, unpack)
import Data.Map (empty)
import System.Linux.Netlink.C
import System.Linux.Netlink.Protocol
import Data.Char (ord)

main = do
    sock <- makeSocket

    let flags = setFlags [NlmFRequest]

    sendmsg sock $ putPacket $ (Header RtmGetlink flags 42 0,
                                LinkMsg ArphrdNone 1 0,
                                empty)
    res <- recvmsg sock 8192
    dumpNumeric res
    print (getPacket res)

    sendmsg sock $ putPacket $ (Header RtmGetlink flags 43 0,
                                LinkMsg ArphrdNone 2 0,
                                empty)
    res <- recvmsg sock 8192
    print res
    print (getPacket res)

    sendmsg sock $ putPacket $ (Header RtmGetlink flags 44 0,
                                LinkMsg ArphrdNone 3 0,
                                empty)
    res <- recvmsg sock 8192
    print res
    print (getPacket res)

dumpNumeric :: ByteString -> IO ()
dumpNumeric b = print $ unpack b

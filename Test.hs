module Main where

import Prelude hiding (length)

import Control.Exception (throwIO)
import Data.ByteString (length)
import System.Linux.Netlink.C
import System.Linux.Netlink.Protocol

import Data.Serialize (encode, decode)

main = do
    sock <- makeSocket
    
    sendmsg sock [encode $ GetInterface ARP_NONE 1 0]
    res <- recvmsg sock 8192
    case decode res of
        Left err -> print err
        Right msg -> print (msg :: Message)

    sendmsg sock [encode $ GetInterface ARP_NONE 2 0]
    res <- recvmsg sock 8192
    case decode res of
        Left err -> print err
        Right msg -> print (msg :: Message)

    sendmsg sock [encode $ GetInterface ARP_NONE 3 0]
    res <- recvmsg sock 8192
    case decode res of
        Left err -> print err
        Right msg -> print (msg :: Message)

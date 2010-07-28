module Main where

import Prelude hiding (length)

import Control.Applicative ((<$>))
import Control.Exception (throwIO)
import Data.ByteString.Char8 (ByteString, length, unpack, pack)
import Data.Map (fromList, empty)
import Data.Char (ord)

import System.Linux.Netlink.Internal

main = do
    sock <- makeSocket

    let flags   = setFlags [NlmFRequest, NlmFRoot, NlmFMatch]
        header  = Header RtmGetlink flags 42 0
        message = LinkMsg ArphrdNone 0 0
        attrs   = empty
    
    mapM_ print $ query (Packet header message attrs)

dumpNumeric :: ByteString -> IO ()
dumpNumeric b = print $ unpack b

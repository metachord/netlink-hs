module Main where

import Prelude hiding (length, concat)

import Control.Applicative ((<$>))
import Control.Exception (throwIO)
import Data.ByteString (ByteString, length, unpack, pack, concat)
import Data.Bits ((.|.))
import Data.Map (fromList, empty)
import Data.Char (ord)

import System.Linux.Netlink.Internal

main = do
    sock <- makeSocket

    let flags   = foldr (.|.) 0 [fNLM_F_REQUEST, fNLM_F_ROOT, fNLM_F_MATCH]
        header  = Header eRTM_GETLINK flags 42 0
        message = LinkMsg 0 0 0
        attrs   = empty
    mapM_ print =<< query sock (Packet header message attrs)

dumpNumeric :: ByteString -> IO ()
dumpNumeric b = print $ unpack b

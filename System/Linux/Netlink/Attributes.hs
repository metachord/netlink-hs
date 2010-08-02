{-# LANGUAGE OverloadedStrings #-}

module System.Linux.Netlink.Attributes 
    ( getLinkAddress
    , getLinkBroadcast
    , getLinkName
    , getLinkMTU
    , getLinkQDisc
    , getLinkTXQLen

    , putLinkAddress
    , putLinkBroadcast
    , putLinkName
    , putLinkMTU
    , putLinkQDisc
    , putLinkTXQLen
    ) where

import Prelude hiding (init, lookup)

import Control.Applicative ((<$>))
import Data.ByteString.Char8 (ByteString, append, init, pack, unpack)
import Data.Char (chr, ord)
import Data.Map (insert, lookup)
import Data.Serialize.Get (runGet, getWord32host)
import Data.Serialize.Put (runPut, putWord32host)
import Data.Word (Word8, Word32)

import System.Linux.Netlink.Constants
import System.Linux.Netlink.Protocol

type AttributeReader a = Attributes -> Maybe a

type AttributeWriter a = a -> Attributes -> Attributes

--
-- Link message attributes
--
type LinkAddress = (Word8, Word8, Word8, Word8, Word8, Word8)

getLinkAddress :: AttributeReader LinkAddress
getLinkAddress attrs = decodeMAC <$> lookup eIFLA_ADDRESS attrs

putLinkAddress :: AttributeWriter LinkAddress
putLinkAddress addr = insert eIFLA_ADDRESS (encodeMAC addr)

getLinkBroadcast :: AttributeReader LinkAddress
getLinkBroadcast attrs = decodeMAC <$> lookup eIFLA_BROADCAST attrs

putLinkBroadcast :: AttributeWriter LinkAddress
putLinkBroadcast addr = insert eIFLA_BROADCAST (encodeMAC addr)

getLinkName :: AttributeReader String
getLinkName attrs = getString <$> lookup eIFLA_IFNAME attrs

putLinkName :: AttributeWriter String
putLinkName ifname = insert eIFLA_IFNAME (putString ifname)

getLinkMTU :: AttributeReader Word32
getLinkMTU attrs = get32 =<< lookup eIFLA_MTU attrs

putLinkMTU :: AttributeWriter Word32
putLinkMTU mtu = insert eIFLA_MTU (put32 mtu)

-- TODO: IFLA_LINK - need to understand what it does

getLinkQDisc :: AttributeReader String
getLinkQDisc attrs = getString <$> lookup eIFLA_QDISC attrs

putLinkQDisc :: AttributeWriter String
putLinkQDisc disc = insert eIFLA_QDISC (putString disc)

-- TODO: IFLA_STATS - bloody huge message, will deal with it later.

-- TODO: IFLA_{COST,PRIORITY,MASTER,WIRELESS,PROTINFO} - need to
-- understand what they do.

getLinkTXQLen :: AttributeReader Word32
getLinkTXQLen attrs = get32 =<< lookup eIFLA_TXQLEN attrs

putLinkTXQLen :: AttributeWriter Word32
putLinkTXQLen len = insert eIFLA_TXQLEN (put32 len)

-- TODO: IFLA_{MAP,WEIGHT} - need to figure out

-- TODO: IFLA_{LINKMODE,LINKINFO} - see Documentation/networking/operstates.txt

-- TODO: IFLA_{NET_NS_PID,IFALIAS} - need to figure out

--
-- Helpers
--

decodeMAC :: ByteString -> LinkAddress
decodeMAC = tuplify . map (fromIntegral . ord) . unpack
  where tuplify [a,b,c,d,e,f] = (a,b,c,d,e,f)
        tuplify _ = error "Bad encoded MAC"

encodeMAC :: LinkAddress -> ByteString
encodeMAC = pack . map (chr . fromIntegral) . listify
  where listify (a,b,c,d,e,f) = [a,b,c,d,e,f]

getString :: ByteString -> String
getString b = unpack (init b)

putString :: String -> ByteString
putString s = append (pack s) "\0"

get32 :: ByteString -> Maybe Word32
get32 bs = case runGet getWord32host bs of
    Left  _ -> Nothing
    Right w -> Just w

put32 :: Word32 -> ByteString
put32 w = runPut (putWord32host w)

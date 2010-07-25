module Main where

import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Data.Char (isNumber, toLower, toUpper)
import Data.List (intersperse, isPrefixOf, isInfixOf, notElem, elem)
import Data.Set (fromList, toList)
import System.Environment (getArgs)
import System.Process (readProcess)

main = do
    [out] <- getArgs
    inc <- includes
    defs <- generate
    writeFile out (inc ++ defs)

includes :: IO String
includes = includeBlock [ "linux/if.h"
                        , "linux/if_tun.h"
                        , "linux/if_arp.h"
                        , "linux/if_link.h"
                        , "linux/netlink.h"
                        , "linux/rtnetlink.h"
                        , "sys/socket.h"
                        ]

generate :: IO String
generate = concat <$> sequence
       [ defsToEnum "LinkFlags" ["linux/if.h",
                                 "linux/if_tun.h"] ("IFF_" `isPrefixOf`)
       , defsToEnum "LinkType" ["linux/if_arp.h"] ("ARPHRD_" `isPrefixOf`)
       , defsToEnum "MessageFlags" ["linux/netlink.h"] ("NLM_F_" `isPrefixOf`)
       , defsToEnum "MessageType" ["linux/netlink.h",
                                   "linux/rtnetlink.h"]
         (\d -> d `notElem` ["NLMSG_ALIGNTO", "NLMSG_MIN_TYPE",
                             "RTM_BASE", "RTM_MAX"] &&
                not ("RTM_F_" `isPrefixOf` d) &&
                ("NLMSG_" `isPrefixOf` d || "RTM_" `isPrefixOf` d))
       , defsToEnum "AddressFamily" ["sys/socket.h"] ("AF_" `isPrefixOf`)
       , defsToEnum "LinkAttrType" ["linux/if_link.h"] ("IFLA_" `isPrefixOf`)
       ]

defsToEnum :: String -> [String] -> (String -> Bool) -> IO String
defsToEnum name patterns cond = do
    ('\n' :) . makeEnum name <$> filteredDefs patterns cond

makeEnum :: String -> [String] -> String
makeEnum name defs = unlines $ header : body ++ ["};"]
  where
    header = "enum " ++ name ++ " {"
    body = map fmt $ map (toCamelCase &&& id) defs
    fmt (a,b) = "  " ++ a ++ " = " ++ b ++ ","

filteredDefs :: [String] -> (String -> Bool) -> IO [String]
filteredDefs patterns filt = filter filt <$> get
  where get = getDefinitions =<< includeBlock patterns

getDefinitions :: String -> IO [String]
getDefinitions headers = uniq . clean <$> preprocess
  where preprocess = readProcess "gcc" ["-E", "-dM", "-"] headers
        uniq = toList . fromList
        clean = map (!! 1) . filter interesting . map words . lines
        interesting def = (isDefine def && hasValue def &&
                           hasNumericValue def && not (isMacro def) &&
                           not (isInternal def))
        isDefine = (== "#define") . head
        hasValue = (>= 3) . length
        hasNumericValue = (/= '(') . head . (!! 2)
        isMacro def = '(' `elem` (def !! 1)
        isInternal def = head (def !! 1) == '_'

includeBlock :: [String] -> IO String
includeBlock patterns = unlines . mkInclude . clean . lines <$> get
  where
    get = concat <$> sequence (map getOne patterns)
    getOne p = readProcess "find" [root, "-wholename", root ++ p] ""
    clean = map $ drop (length root)
    mkInclude = map (\x -> "#include <" ++ x ++ ">")
    root = "/usr/include/"

toCamelCase :: String -> String
toCamelCase s = run s True
  where run []       _     = []
        run ('_':xs) _     = run xs True
        run (x:xs)   True  = (toUpper x) : run xs False
        run (x:xs)   False = (toLower x) : run xs False

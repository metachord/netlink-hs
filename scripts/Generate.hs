module Main where

import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Control.Monad (join)
import Data.Char (isNumber, toLower, toUpper)
import Data.Function (on)
import Data.List (intersperse, isPrefixOf, isInfixOf, notElem, sortBy)
import Data.Map (Map, elems, filterWithKey, fromList,
                 keys, mapKeys, toList, union)
import Data.Maybe (mapMaybe)
import Language.C.Analysis (runTrav_)
import Language.C.Analysis.AstAnalysis (analyseAST)
import Language.C.Analysis.SemRep (GlobalDecls(..), TagDef(EnumDef),
                                   EnumType(..), Enumerator(..))
import Language.C.Data.Ident (Ident(..))
import Language.C.Data.InputStream (inputStreamFromString)
import Language.C.Data.Position (Position(..))
import Language.C.Parser (parseC)
import Language.C.Pretty (pretty)
import Language.C.Syntax.Constants (getCInteger)
import Language.C.Syntax.AST (CExpr(..), CConst(CIntConst), CBinaryOp(..))
import System.Environment (getArgs)
import System.Process (readProcess)
import Text.Regex.PCRE ((=~))

main = do
    [out] <- getArgs
    let inc = mkIncludeBlock includeFiles
    defines <- getDefinitions inc
    enums <- getEnums inc
    let (exports, definitions) = outputs defines enums
        prelude = [
            "{-# LANGUAGE GeneralizedNewtypeDeriving #-}",
            "module System.Linux.Netlink.Constants (" ++
            join (intersperse ", " $ join exports) ++
            ") where",
            "",
            "import Data.Bits",
            ""]
    writeFile out $ unlines (prelude ++ join definitions)

outputs :: Map String Integer -> [Map String Integer] -> ([[String]], [[String]])
outputs d e = let define r = selectDefines r d
                  enum r = selectEnum r e
              in map fst &&& map snd $
    [mkEnum "AddressFamily" $ define "^AF_",
     mkEnum "MessageType" $
       union (define "^NLMSG_(?!ALIGNTO)") (enum "^RTM_"),
     mkFlag "MessageFlags"  $ define "^NLM_F_",
     mkEnum "LinkType"      $ define "^ARPHRD_",
     mkFlag "LinkFlags"     $ define "^IFF_",
     mkEnum "LinkAttrType"  $ enum   "^IFLA_",
     mkFlag "AddrFlags"     $ define "^IFA_F_",
     mkEnum "Scope"         $ enum   "^RT_SCOPE_",
     mkEnum "AddrAttrType"  $ enum   "^IFA_",
     mkEnum "RouteTableId"  $ enum   "^RT_TABLE_",
     mkEnum "RouteProto"    $ define "^RTPROT_",
     mkEnum "RouteType"     $ enum   "^RTN_",
     mkFlag "RouteFlags"    $ define "^RTM_F_",
     mkEnum "RouteAttrType" $ enum   "^RTA_"]

includeFiles :: [String]
includeFiles = [ "sys/types.h"
               , "sys/socket.h"
               , "linux/if.h"
               , "linux/if_tun.h"
               , "linux/if_arp.h"
               , "linux/if_link.h"
               , "linux/netlink.h"
               , "linux/rtnetlink.h"
               ]

mkIncludeBlock :: [String] -> String
mkIncludeBlock = unlines . map (\e -> "#include <" ++ e ++ ">")

mkFlag :: String -> Map String Integer -> ([String], [String])
mkFlag name vals = (name : map fst values,
                    ty : "" : join (map makeConst values))
  where
    ty = ("newtype " ++ name ++ " = " ++
          name ++
          " Int deriving (Bits, Eq, Enum, Integral, Num, Ord, Real, Show)")
    makeConst (n, v) = [n ++ " :: (Num a, Bits a) => a",
                        n ++ " = " ++ show v]
    values = sortBy (compare `on` snd) . toList . mapKeys ("f" ++) $ vals

mkEnum :: String -> Map String Integer -> ([String], [String])
mkEnum name vals = (name : map fst values,
                    ty : "" : join (map makeConst values))
  where
    ty = ("newtype " ++ name ++ " = " ++
          name ++
          " Int deriving (Eq, Enum, Integral, Num, Ord, Real, Show)")
    makeConst (n, v) = [n ++ " :: (Num a) => a",
                        n ++ " = " ++ show v]
    values = sortBy (compare `on` snd) . toList . mapKeys ('e' :) $ vals

selectDefines :: String -> Map String Integer -> Map String Integer
selectDefines regex defines = filterWithKey (\k v -> k =~ regex) defines

selectEnum :: String -> [Map String Integer] -> Map String Integer
selectEnum regex enums = head $ filter (all (=~ regex) . keys) enums

full :: String -> String
full regex = "^" ++ regex ++ "$"

getEnums :: String -> IO [Map String Integer]
getEnums source = do
    parsed <- flip parseC initPos . inputStreamFromString <$> preprocessed
    let unit = gTags . fst . check $ runTrav_ (analyseAST $ check parsed)
        enums = mapMaybe getEnum (elems unit)
    return $ map cleanEnums enums
  where
    check (Left err) = error $ show err
    check (Right a)   = a
    preprocessed = readProcess "gcc" ["-E", "-"] source
    initPos = Position "" 0 0
    getEnum (EnumDef (EnumType _ es _ _)) = Just $ map getEnumValue es
    getEnum _                             = Nothing
    getEnumValue (Enumerator (Ident s _ _) v _ _) = (s, evalCExpr v)
    cleanEnums = filterWithKey (\k v -> not ("_" `isPrefixOf` k)) . fromList

evalCExpr :: CExpr -> Integer
evalCExpr (CConst (CIntConst v _)) = getCInteger v
evalCExpr (CBinary CAddOp a b _)   = (evalCExpr a) + (evalCExpr b)
evalCExpr other                    = error $ show (pretty other)

getDefinitions :: String -> IO (Map String Integer)
getDefinitions headers = do
    defines <- map words . lines <$> readDefines headers
    let isDefine (c:n:_) = c == "#define" && '(' `notElem` n && head n /= '_' 
        hasValue = (>= 3) . length
        names = map (!! 1) $ filter (\d -> isDefine d && hasValue d) defines
        kludge = map (\n -> "@define \"" ++ n ++ "\" " ++ n) names
    defines2 <- map words . lines <$> preprocess (headers ++ unlines kludge)
    let isInteresting d = (hasValue d &&
                           head d == "@define" &&
                           (all isNumber (d !! 2) ||
                            "0x" `isPrefixOf` (d !! 2) &&
                            all isNumber (drop 2 (d !! 2))))
        realDefines = map (take 2 . drop 1) $ filter isInteresting defines2
        clean [k,v] = (init (tail k), read v)
    return $ fromList (map clean realDefines)
  where readDefines = readProcess "gcc" ["-E", "-dM", "-"]
        preprocess  = readProcess "gcc" ["-E", "-"]

module IPv6Addr where

import Data.Char (intToDigit,isDigit,isHexDigit,toLower)
import Data.List (group,groupBy,intercalate,intersperse,elemIndex,isSuffixOf)
import Data.Function (on)
import Data.Maybe
import Numeric

type IPv6Addr = String

data IPv4AddrToken = Dot | EightBits String deriving (Eq)

data IPv6AddrToken = SixteenBits String | AllZeros | Colon | DoubleColon
                     | IPv4Addr String deriving (Eq)

tokenizedBy :: String -> Char -> [String]
tokenizedBy s c = groupBy ((==) `on` ( == c)) s

-- Parsing embedded IPv4 address

dot :: String -> Maybe IPv4AddrToken
dot s = if s == "." then Just Dot else Nothing

eightBitsToken :: String -> Maybe IPv4AddrToken
eightBitsToken s =
    let t = read s::Int
    in
        if not(null s) && all isDigit s && t >= 0 && t <= 255
            then Just (EightBits s) else Nothing

ipv4Token :: String -> Maybe IPv4AddrToken
ipv4Token s
    | isJust(dot s) = Just Dot
    | isJust(eightBitsToken s) = Just (EightBits s)
    | otherwise  = Nothing

ipv4Addr :: String -> Maybe IPv6AddrToken
ipv4Addr s =
    do
        let r = map ipv4Token (s `tokenizedBy` '.')
        if (Nothing `notElem` r) && (length r == 7)
            then Just (IPv4Addr s) else Nothing

-- Parsing IPv6 Address

colon :: String -> Maybe IPv6AddrToken
colon s = if s == ":" then Just Colon  else Nothing

doubleColon :: String -> Maybe IPv6AddrToken
doubleColon s = if s == "::" then Just DoubleColon else Nothing

sixteenBits:: String -> Maybe IPv6AddrToken
sixteenBits s =
    if length s < 5 then
         do
              -- "Leading zeros MUST be suppressed" (RFC 5952, 4.1)
              let s'= dropWhile (=='0') s
              if length s' < 5 && all isHexDigit s'
                 then case s' of
                     "" -> Just AllZeros
                     -- Hexadecimal digits MUST be in lowercase (RFC 5952 4.3)
                     otherwise -> Just (SixteenBits (map toLower s'))
                 else Nothing
    else Nothing

ipv6AddrToken :: String -> Maybe IPv6AddrToken
ipv6AddrToken s
           | isJust s' = s'
           | isJust(colon s) = Just Colon
           | isJust(doubleColon s) = Just DoubleColon
           | isJust(ipv4Addr s) = Just (IPv4Addr s)
           | otherwise = Nothing
           where s' = sixteenBits s

ipv6AddrTokenToString :: IPv6AddrToken -> String
ipv6AddrTokenToString (SixteenBits t) = t
ipv6AddrTokenToString Colon = ":"
ipv6AddrTokenToString DoubleColon = "::"
-- "A single 16-bit 0000 field MUST be represented as 0" (RFC 5952, 4.1)
ipv6AddrTokenToString AllZeros = "0"
ipv6AddrTokenToString (IPv4Addr i) = i

ipv6AddrToString :: [Maybe IPv6AddrToken] -> IPv6Addr
ipv6AddrToString t = concatMap ipv6AddrTokenToString (catMaybes t)

countIPv4Addr :: [Maybe IPv6AddrToken] -> Int
countIPv4Addr ts =
    foldr oneMoreIPv4Token 0 ts
        where
            oneMoreIPv4Token t c =
                case t of
                    Just (IPv4Addr _) -> c + 1
                    otherwise -> c

countDoubleColon :: [Maybe IPv6AddrToken] -> Int
countDoubleColon ts =
    foldr oneMoreDoubleColon 0 ts
        where
            oneMoreDoubleColon t c =
                case t of
                    Just DoubleColon -> c + 1
                    otherwise -> c

firstValidToken :: [Maybe IPv6AddrToken] -> Bool
firstValidToken ts =
    case head ts of
        Just (SixteenBits _) -> True
        Just DoubleColon -> True
        Just AllZeros -> True
        otherwise -> False

ipv6AddrConstraints :: [Maybe IPv6AddrToken] -> Bool
ipv6AddrConstraints [] = False
ipv6AddrConstraints [Just DoubleColon] = True
ipv6AddrConstraints [Just DoubleColon,Just (SixteenBits "1")] = True
ipv6AddrConstraints ts  =
    do
        let cdcts = countDoubleColon ts
        let lents = length ts
        let lastt = last ts
        let lenconst = ( (lents == 15 && cdcts <= 0) || (lents < 15 && cdcts == 1) )
        ((firstValidToken ts && all isJust ts) &&
            (case countIPv4Addr ts of
                0 -> case lastt of
                          Just (SixteenBits _) -> lenconst
                          Just DoubleColon -> lenconst
                          Just AllZeros -> lenconst
                          otherwise -> False
                1 -> case lastt of
                          Just (IPv4Addr _) -> (lents == 13 && cdcts <= 0) || (lents < 13 && cdcts == 1)
                          otherwise -> False
                otherwise -> False))
 
maybeIPv6Addr :: String -> Maybe IPv6Addr
maybeIPv6Addr "::" = Just "::"
maybeIPv6Addr "::1" = Just "::1"
maybeIPv6Addr s =
    do
        let ts = map ipv6AddrToken (s `tokenizedBy` ':')
        if ipv6AddrConstraints ts
            then
                (if Just DoubleColon `notElem` ts
                    then Just $ ipv6AddrToString (ipv4AddrToHex $ replaceTheLongestZerosRun ts)
                    else Just $ ipv6AddrToString (ipv4AddrToHex $ replaceTheLongestZerosRun (expandDoubleColon ts)))
            else Nothing

-- The embedded IPv4 address have to be rewritten to output a pure IPv6 Address
-- text representation in hexadecimal digits. But some well-known prefixed IPv6
-- addresses have to keep visible in their text representation the fact that they
-- deals with IPv4 to IPv6 transition process (RFC 5952 Section 5) :
--
-- 	IPv4-compatible IPv6 address like "::1.2.3.4"
-- 	IPv4-mapped IPv6 address like "::ffff:1.2.3.4"
-- 	IPv4-translated address like "::ffff:0:1.2.3.4"
-- 	IPv4-translatable address like "64:ff9b::1.2.3.4"
-- 	ISATAP address like "fe80::5efe:1.2.3.4"
ipv4AddrToHex :: [Maybe IPv6AddrToken] -> [Maybe IPv6AddrToken]
ipv4AddrToHex [Just DoubleColon,Just (IPv4Addr a)] =
    [Just DoubleColon,Just (IPv4Addr a)]
ipv4AddrToHex [Just DoubleColon,Just (SixteenBits "ffff"),Just Colon,Just (IPv4Addr a)] =
    [Just DoubleColon,Just (SixteenBits "ffff"),Just Colon,Just (IPv4Addr a)]
ipv4AddrToHex [Just DoubleColon,Just (SixteenBits "ffff"),Just Colon,Just AllZeros,Just Colon,Just (IPv4Addr a)] =
    [Just DoubleColon,Just (SixteenBits "ffff"),Just Colon,Just AllZeros,Just Colon,Just (IPv4Addr a)]
ipv4AddrToHex [Just (SixteenBits "64"),Just Colon,Just (SixteenBits "ff9b"),Just DoubleColon,Just (IPv4Addr a)] =
    [Just (SixteenBits "64"),Just Colon,Just (SixteenBits "ff9b"),Just DoubleColon,Just (IPv4Addr a)]
ipv4AddrToHex ts =
        case last ts of
            Just (IPv4Addr a) ->
                do
                    let its = init ts
                    if [Just (SixteenBits "200"),Just Colon,Just (SixteenBits "5efe"),Just Colon] `isSuffixOf` its
                       || [Just AllZeros,Just Colon,Just (SixteenBits "5efe"),Just Colon] `isSuffixOf` its
                       || [Just DoubleColon,Just (SixteenBits "5efe"),Just Colon] `isSuffixOf` its
                        then ts
                        else do
                            let m = map (\x -> showIntAtBase 16 intToDigit (read x::Int) "")
                                    $ filter (/= ".") (a `tokenizedBy` '.')
                            its ++ [Just (SixteenBits ((!!) m 0 ++ addZero((!!) m 1))),Just Colon,Just (SixteenBits ((!!) m 2 ++ addZero((!!) m 3)))]
                                where
                                    addZero s = if length s < 2 then '0':s else s
            otherwise -> ts

expandDoubleColon :: [Maybe IPv6AddrToken] -> [Maybe IPv6AddrToken]
expandDoubleColon mit = do
    let s = splitAt (fromJust $ elemIndex (Just DoubleColon) mit) mit
    let fsts = fst s
    let snds = if length(snd s) >= 1 then tail(snd s) else []
    let fste = if null fsts then [] else fsts ++ [Just Colon]
    let snde = if null snds then [] else Just Colon : snds
    fste ++ allZerosTokensReplacement(quantityOfAllZerosTokenToReplace mit) ++ snde
    where
        quantityOfAllZerosTokenToReplace x =
            8 - foldl (\c x -> if (x /= Just DoubleColon) && (x /= Just Colon)
                               then c+1 else c) 0 x
        allZerosTokensReplacement x =
            intersperse (Just Colon) (replicate x (Just AllZeros))

-- "the longest run of consecutive 16-bit 0 fields MUST be shortened" (RFC 5952, 4.2.3)
-- All the stuff to deal with replacement of the longest zeros run by a double colon

longestLengthZerosRun :: (Num a, Ord a) => [(Bool, a)] -> a
longestLengthZerosRun x =
    maximum $ map longest x
        where
            longest t =
                case t of
                    (True,i) -> i
                    otherwise -> 0

replaceTheLongestZerosRun :: [Maybe IPv6AddrToken] -> [Maybe IPv6AddrToken]
replaceTheLongestZerosRun s' =
    zerosToDoubleColon s' (zerosRunToReplace $ zerosRunsList s')
        where
            zerosRunToReplace t =
                let l = longestLengthZerosRun t
                in (firstLongestZerosRunIndex t l,l)

firstLongestZerosRunIndex :: (Num b) => [(Bool, b)] -> b -> b
firstLongestZerosRunIndex x y =
    sum . snd . unzip $ takeWhile (/=(True,y)) x

zerosRunsList :: [Maybe IPv6AddrToken] -> [(Bool, Int)]
zerosRunsList x =
    map helper $ groupZerosRuns x
        where
            helper h =
                if head h == Just AllZeros
                then (True,lh) else (False,lh)
                    where lh = length h
            groupZerosRuns =
                group . filter (/= Just Colon)

zerosToDoubleColon :: [Maybe IPv6AddrToken] -> (Int,Int) -> [Maybe IPv6AddrToken]
-- No all zeros token, so no double colon replacement...
zerosToDoubleColon ls (_,0) = ls
-- "The symbol '::' MUST NOT be used to shorten just one 16-bit 0 field" (RFC 5952 4.2.2)
zerosToDoubleColon ls (_,1) = ls
zerosToDoubleColon ls (i,l) =
    let ls' = filter (/= Just Colon) ls
    in intersperse (Just Colon) (take i ls')
    ++ [Just DoubleColon]
    ++ intersperse (Just Colon) (drop (i+l) ls')

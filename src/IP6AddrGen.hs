import System.IO
import System.Exit
import Control.Monad (replicateM,replicateM_)
import System.Environment
import System.Console.GetOpt
import Data.List (groupBy,intercalate)
import System.Random (randomRIO)
import Data.Char (isDigit,intToDigit,toLower,isHexDigit)
import Data.Function (on)

-- IPv6 address prefix validation and random rewrite of trailing zero(s)

tokenizeIPv6Input :: String -> [String]
tokenizeIPv6Input = groupBy ((==) `on` (==':'))

is16bitsToken :: String -> Bool
is16bitsToken s = l < 5 && l > 0 && all isHexDigit s where l = length s

ipv6PrefixToken :: String -> Char
ipv6PrefixToken s
                 | s == ":" = 'c'
                 | is16bitsToken s = '6'
                 | otherwise = '?'
		   
inputIPv6PrefixToInternalRep :: [String] -> String
inputIPv6PrefixToInternalRep = map ipv6PrefixToken

readLast16bitsTokenOfPrefix :: (Monad m) => String -> m (String,Int)
readLast16bitsTokenOfPrefix s =
    let p = span (== '0') $ reverse s in return (reverse(snd p),length (fst p))

prefixRewrite :: String -> IO ()							
prefixRewrite s =
    do
        t <- readLast16bitsTokenOfPrefix s
        putStr $ map toLower $fst t
        g <- genNhex $ snd t
        putStr g
				
-- IPv6 Address random generation

genRandomHex :: IO Char
genRandomHex =
    do
        r <- randomRIO(0,15)::IO Int
        return (toLower $ intToDigit r)			
					
genNhex :: Int -> IO String
genNhex n = replicateM n genRandomHex

n16bitsToken :: Int -> IO ()
n16bitsToken n = replicateM n (genNhex 4) >>= putStr . intercalate ":"

count16bitsToken :: String -> Int
count16bitsToken = foldl (\x y -> if y == '6' then x+1 else x) 0

mkRandomIPv6Addr :: IO ()
mkRandomIPv6Addr = n16bitsToken 8 >> putStr "\n"

mkRandomIPv6AddrWithPrefix :: String -> String -> IO ()
mkRandomIPv6AddrWithPrefix s p =
    do
        let c = count16bitsToken p
        if c < 9 then
            do
                prefixRewrite s
                if c == 8 then putStr "" else putStr ":"
                n16bitsToken (8 - c)
                putStr "\n"
                else putStr "Prefix too long\n"  >> exit

ipv6AddrGen :: Int -> String -> IO ()
ipv6AddrGen n []	=	replicateM_ n mkRandomIPv6Addr 
ipv6AddrGen n s 	=
    let p = inputIPv6PrefixToInternalRep $ tokenizeIPv6Input s in
    if ('?' `notElem` p) && (head p /= 'c') && (last p /= 'c')
    then replicateM_ n (mkRandomIPv6AddrWithPrefix s p)
    else putStr "Bad input for -p option\n"
			
-- GetOpt stuff

data Flag = Help | Version | Count String | Prefix String deriving (Show)

options :: [OptDescr Flag]
options =
          [Option "n" [] (ReqArg Count "count")  "Number of IPv6 adresses to output"
          ,Option "p" [] (ReqArg Prefix "prefix") "An uncompressed IPv6 prefix"
          ,Option "v" [] (NoArg Version) "Show version number"
          ,Option "h" [] (NoArg Help)    "Show usage"]

safeReadN :: String -> String -> IO ()
safeReadN n p =
    if all isDigit n
    then ipv6AddrGen (read n :: Int) p >> exit
    else putStrLn "Bad input for -n option" >> exit

parseArgs :: [String] -> IO ()				
parseArgs args =
    case getOpt RequireOrder options args of
        ([Count n],_,_) -> safeReadN n ""
        ([Prefix p],_,_) -> ipv6AddrGen 1 p >> exit
        ([Count n,Prefix p],_,_) -> safeReadN n p
        ([Prefix p,Count n],_,_) -> safeReadN n p
        ([Version],_,_) -> putStrLn version >> exit
        ([Help],_,_) -> putStrLn (usageInfo help options) >> exit
        (_,_,_) -> putStrLn (usageInfo help options) >> exit

version = "ip6addrgen version 0.3.1"

help = "Usage: ip6addrgen [-v|-h] | [-n] [-p]\n"

exit = exitWith ExitSuccess

main :: IO ()
main = getArgs >>= parseArgs

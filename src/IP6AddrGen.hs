--   ip6addrgen generates random IPv6 Addresses
--   Copyright (c) 2010, Michel Boucey
--   All rights reserved.

--   Redistribution and use in source and binary forms,
--   with or without modification, are permitted provided that
--   the following conditions are met:

--   Redistributions of source code must retain the above copyright notice,
--   this list of conditions and the following disclaimer. Redistributions in
--   binary form must reproduce the above copyright notice, this list of
--   conditions and the following disclaimer in the documentation and/or other
--   materials provided with the distribution. The name of the author may not be
--   used to endorse or promote products derived from this software without
--   specific prior written permission.

--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--   AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
--   THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
--   PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
--   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
--   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
--   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
--   OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
--   WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
--   OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
--   ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

import System.IO
import System.Exit
import Control.Monad (replicateM,replicateM_)
import System.Environment
import System.Console.GetOpt
import Data.List (intercalate)
import System.Random (randomRIO)
import Data.Char (isDigit,intToDigit,toUpper)

-- local import
import IsIPv6Addr

-- IPv6 address prefix validation and random rewrite of trailing zero(s)
	
ipv6PrefixToken :: String -> Char
ipv6PrefixToken s
                 | s ==":" = 'c'
                 | is16bitsToken s == True = '6'
                 | otherwise = '?'
		   
inputIPv6PrefixToInternalRep :: [String] -> String
inputIPv6PrefixToInternalRep = map ipv6PrefixToken

readLast16bitsTokenOfPrefix :: (Monad m) => String -> m (String,Int)
readLast16bitsTokenOfPrefix s = do let p = span (== '0') $ reverse s in return ((reverse $ snd p),(length $ fst p))

prefixRewrite :: String -> IO ()							
prefixRewrite s =  do
			t <- readLast16bitsTokenOfPrefix s
			putStr $ map toUpper $fst t
			g <- genNhex $ snd t
			putStr g
				
-- IPv6 Address random generation

genRandomHex :: IO Char
genRandomHex = do
			r <- randomRIO(0,15)::IO Int
			return (toUpper $ intToDigit r)			
					
genNhex :: Int -> IO String
genNhex n = replicateM n genRandomHex

n16bitsToken :: Int -> IO ()
n16bitsToken n = replicateM n (genNhex 4) >>= putStr . intercalate ":"

count16bitsToken :: String -> Int
count16bitsToken = foldl (\x y -> if y == '6' then x+1 else x) 0

mkRandomIPv6Addr :: IO ()
mkRandomIPv6Addr = n16bitsToken 8 >> putStr "\n"

mkRandomIPv6AddrWithPrefix :: String -> String -> IO ()
mkRandomIPv6AddrWithPrefix s p =	do
					let c = count16bitsToken p
					if c < 9
						then
							do
								prefixRewrite s
								if c == 8 then putStr "" else putStr ":"
								n16bitsToken (8 - c)
								putStr "\n"
						else putStr "Prefix too long\n"  >> exit

ipv6AddrGen :: Int -> String -> IO ()
ipv6AddrGen n []	=	replicateM_ n mkRandomIPv6Addr 
ipv6AddrGen n s 	= 	let p = inputIPv6PrefixToInternalRep $ tokenizeIPv6Input s in
						if ('?' `notElem` p) && (head p /= 'c') && (last p /= 'c')
						then do replicateM_ n (mkRandomIPv6AddrWithPrefix s p)
						else putStr "Bad input for -p option\n"
			
-- GetOpt stuff

data Flag = Help | Version | Count String | Prefix String deriving (Show)

options :: [OptDescr Flag]
options =
          [Option ['n'] []  (ReqArg Count "count")  "Number of IPv6 adresses to output"
		  ,Option ['p'] [] 	(ReqArg Prefix "prefix") "An uncompressed IPv6 prefix"
		  ,Option ['v'] [] 	(NoArg Version) "Show version number"
          ,Option ['h'] []	(NoArg Help)    "Show usage"]

safeReadN :: String -> String -> IO ()
safeReadN n p = if all (\x -> isDigit x) n
				then do ipv6AddrGen (read n :: Int) p >> exit
				else putStrLn "Bad input for -n option" >> exit

parseArgs :: [String] -> IO ()				
parseArgs args = case getOpt RequireOrder options args of
					([],[],[])					-> mkRandomIPv6Addr >> exit
					([Count n],_,_)				-> safeReadN n ""
					([Prefix p],_,_)			-> ipv6AddrGen 1 p >> exit
					([Count n,Prefix p],_,_)	-> safeReadN n p
					([Prefix p,Count n],_,_)	-> safeReadN n p
					([Prefix p,Count n,_],_,_)	-> putStrLn version >> exit
					([Version],_,_)    			-> putStrLn version >> exit
					([Help],_,_)      			-> putStrLn (usageInfo help options) >> exit
					(_,_,_)           			-> hPutStr stderr (usageInfo help options)

version = "0.1"

help = "Usage: ip6addrgen [-v|-h] | [-n] [-p]\n"

exit = exitWith ExitSuccess

main :: IO ()
main = getArgs >>= parseArgs

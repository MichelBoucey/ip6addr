--   ip6addr filters parsed IPv6 Addresses against RFC 4291
--   Copyright (c) 2009-2010, Michel Boucey
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

module IsIPv6Addr where

import Data.Char (isDigit,isHexDigit)
import Data.List (groupBy)
import Data.Function (on)

-- Parsing embedded IPv4 address

is8bitsToken :: String -> Bool
is8bitsToken s = if (not $ null s) && all isDigit s
                 then t >= 0 && t <= 255 else False
                 where t = read s::Int

isIPv4Token :: String -> Char
isIPv4Token s
              | s == "." = 'd'
              | is8bitsToken s == True = '8'
              | otherwise = '?'

internalIPv4Rep :: [String] -> String
internalIPv4Rep = map isIPv4Token

tokenizeIPv4AddrInput :: String -> [String]
tokenizeIPv4AddrInput = groupBy ((==) `on` (=='.'))

parseIPv4Tokens :: String -> Bool
parseIPv4Tokens s = s == "8d8d8d8"

embeddedIPv4Addr :: String -> Int
embeddedIPv4Addr s
                   | f == 0 = 0
                   | f == 1 = 1
                   | otherwise = -1
                   where f = length $ filter (=='4') s

isIPv4Addr :: String -> Bool
isIPv4Addr = parseIPv4Tokens . internalIPv4Rep . tokenizeIPv4AddrInput

-- Parsing IPv6 Address

is16bitsToken :: String -> Bool
is16bitsToken s = l < 5 && l > 0 && all isHexDigit s where l = length s

tokenizeIPv6Input :: String -> [String]
tokenizeIPv6Input = groupBy ((==) `on` (==':'))

ipv6AddrToken :: String -> Char
ipv6AddrToken s
               | s ==":" = 'c'
               | s == "::" = 'd'
               | is16bitsToken s == True = '6'
               | isIPv4Addr s == True = '4'
               | otherwise = '?'

ipv6AddrInternalRep :: [String] -> String
ipv6AddrInternalRep = map ipv6AddrToken

isCompressed :: String -> Int
isCompressed s
              | c == 0 = 0
              | c == 1 = 1
              | otherwise = -1
              where c = length $ filter (=='d') s

ipv6AddrConstraints :: String -> Bool
ipv6AddrConstraints s = if '?' `notElem` s then
							let h = head s in
								if (h == '6' || h == 'd') then
									case embeddedIPv4Addr s of
										-1 -> False
										0 -> (e == '6' || e == 'd') && ((l == 15 && c <= 0) || (l < 15 && c == 1))
										1 -> (e == '4' && l == 13 && c <= 0) || (e == '4' && l < 13 && c == 1)
								else False
						else False
								where
									l = length s
									c = isCompressed s
									e = last s

isIPv6Addr :: String -> Bool
isIPv6Addr s
			| length s < 2 = False
			| otherwise = ipv6AddrConstraints $ ipv6AddrInternalRep $ tokenizeIPv6Input s
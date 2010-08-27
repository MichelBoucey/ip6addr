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

module CidrSuffix where

import Data.Char (isDigit)
import Data.List (groupBy)
import Data.Function (on)

isCidrSuffix :: String -> Bool
isCidrSuffix s = if all isDigit s && ((length s > 1 && head s /= '0')||(length s == 1))
				 then t >= 0 && t <= 128 else False
				 where t = read s::Int

removeCidrSuffix :: String -> String
removeCidrSuffix [] = []
removeCidrSuffix s
		  | l == 1 = s
		  | l == 3 = if g !! 1 == "/" && (isCidrSuffix $ last g) then head g else []
		  | otherwise = []
		  where
			g = groupBy ((==) `on` (=='/')) s
			l = length g

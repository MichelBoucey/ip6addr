--   ip6addr filters parsed IPv6 Addresses against RFC 4291
--   Copyright (c) 2009, Michel Boucey
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
import System.Environment
import System.Console.GetOpt

-- local import
import IsIPv6Addr

data Flag = Help | Version | Stderr
            deriving (Show,Eq)

options :: [OptDescr Flag]
options =
          [Option ['v'] ["version"] (NoArg Version) "show version number"
          ,Option ['e'] ["errors"]  (NoArg Stderr)  "throw out discarded inputs to stderr"
          ,Option ['h'] ["help"]    (NoArg Help)    "show usage"]

parseArgs args = case getOpt RequireOrder options args of
                 ([],[],[])           -> evalInputs . lines =<< getContents
                 ([Version],[],[])    -> putStrLn version >> exit
                 ([Help],[],[])       -> putStrLn (usageInfo help options) >> exit
                 ([Stderr],[],[])     -> evalInputsErr . lines =<< getContents
                 ([Stderr],inputs,[]) -> evalInputsErr inputs
                 ([],inputs,[])       -> evalInputs inputs
                 (_,_,errs)           -> hPutStr stderr (concat errs ++ usageInfo help options)
                                         >> exitWith (ExitFailure 1)

evalInputs :: [String] -> IO ()
evalInputs [] = exit
evalInputs (a:as) = if isIPv6Addr a
                    then hPutStrLn stdout a >> evalInputs as
                    else hPutStr stderr ""  >> evalInputs as

evalInputsErr :: [String] -> IO ()
evalInputsErr [] = exit
evalInputsErr (a:as) = if isIPv6Addr a
                       then hPutStrLn stdout a >> evalInputsErr as
                       else hPutStrLn stderr a >> evalInputsErr as

version = "0.1.1"
help   = "Usage: ip6addr [-v|-h] | [-e] [address ...]\n"
exit    = exitWith ExitSuccess

main = getArgs >>= parseArgs

import System.IO
import System.Exit
import System.Environment
import System.Console.GetOpt

-- local import
import IPv6Addr
import CidrSuffix

data Flag = Help | Version | Stderr deriving (Show,Eq)

options :: [OptDescr Flag]
options =
          [Option "v" [] (NoArg Version) "Show version number"
          ,Option "e" [] (NoArg Stderr)  "Throw out discarded inputs to stderr"
          ,Option "h" [] (NoArg Help)    "Show usage"]

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
evalInputs (i:is) =
    case maybeIPv6Addr a of
        Just b -> putStrLn b >> evalInputs is
        Nothing -> evalInputs is
    where a = removeCidrSuffix i

evalInputsErr :: [String] -> IO ()
evalInputsErr [] = exit
evalInputsErr (i:is) =
    case maybeIPv6Addr a of
        Just b -> putStrLn b >> evalInputsErr is
        Nothing -> hPutStrLn stderr i >> evalInputsErr is
    where a = removeCidrSuffix i
			   
version = "ip6addrcan version 0.3.2"

help   = "Usage: ip6addrcan [-v|-h] | [-e] [address ...]\n"

exit   = exitWith ExitSuccess

main :: IO ()
main = getArgs >>= parseArgs

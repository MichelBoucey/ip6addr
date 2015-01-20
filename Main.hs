-- -----------------------------------------------------------------------------
--
-- Copyright  : (c) Michel Boucey 2015
-- License    : BSD-Style
-- Maintainer : michel.boucey@gmail.com
--
-- commandline tool to generate IPv6 address text representations
--
-- -----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable,OverloadedStrings #-}

import Control.Applicative ((<$>))
import Control.Monad (replicateM_)
import Data.Text as T (append,pack)
import Data.Text.IO as TIO (putStrLn,hPutStrLn)
import System.Console.CmdArgs
import System.Exit
import System.IO (stderr)
import Text.IPv6Addr

data Input =Input
    { output   :: String
    , address  :: String
    , quantity :: Int
    } deriving (Show,Data,Typeable)

ip6addrInput :: Input
ip6addrInput = Input
    { address = "" &= typ " <IPv6 address>"
    , output = "canonical" &= typ " [pure|canonical|full|arpa|random]" &= help "(default=canonical)"
    , quantity = 1 &= help "Amount of random addresses to generate" &= typ " <Integer>"
    } &= summary "ip6addr version 0.4 (C) Michel Boucey 2015"
      &= program "ip6addr"
      &= helpArg [name "h"]
      &= details ["Examples:","  ip6addr -a 0:0::FFFF:192.0.2.128","  ip6addr -o full -a ::1","  ip6addr -o random -q 10",""]

main :: IO ()
main = do
    a <- cmdArgs ip6addrInput
    if output a == "random"
        then replicateM_ (quantity a) putRandAddr >> exitSuccess
        else do let m = address a
                case output a of
                    "canonical" -> out maybeIPv6Addr m fromIPv6Addr
                    "full"      -> out maybeFullIPv6Addr m fromIPv6Addr
                    "arpa"      -> out maybePureIPv6Addr m toIP6ARPA
                    "pure"      -> out maybePureIPv6Addr m fromIPv6Addr
                    _           -> hPutStrLn stderr "See help"
                                >> exitFailure
  where
    putRandAddr = fromIPv6Addr <$> randIPv6Addr >>= TIO.putStrLn
    out t i o =
        if i /= ""
            then do let p = T.pack i
                    case t p of
                        Nothing -> hPutStrLn stderr ("'" `T.append` p `T.append` "' is not an IPv6 address") >> exitFailure
                        Just a  -> TIO.putStrLn (o a) >> exitSuccess
            else Prelude.putStrLn "See help" >> exitFailure

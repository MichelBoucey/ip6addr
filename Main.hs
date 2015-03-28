-- -----------------------------------------------------------------------------
--
-- Copyright  : (c) Michel Boucey 2015
-- License    : BSD-Style
-- Maintainer : michel.boucey@gmail.com
--
-- Commandline tool to generate IPv6 address text representations
--
-- -----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable,OverloadedStrings #-}

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
    , prefix   :: String
    } deriving (Show,Data,Typeable)

ip6addrInput :: Input
ip6addrInput = Input
    { address = ""
      &= typ " <IPv6 address>"
    , output = "canonical"
      &= typ " [pure|canonical|full|arpa|random]"
      &= help "(default=canonical)"
    , quantity = 1
      &= help "Amount of random addresses to generate"
      &= typ " <Integer>"
    , prefix = ""
      &= typ " <Prefix>"
      &= help "Set a prefix for random addresses generation"
    } &= summary "ip6addr version 0.5.0.1 (C) Michel Boucey 2015"
      &= program "ip6addr"
      &= helpArg [name "h"]
      &= details [ "Examples:"
                 , ""
                 , "  ip6addr -a 0:0::FFFF:192.0.2.128"
                 , "  ip6addr -o full -a ::1"
                 , "  ip6addr -o random -q 10 -p 1234:abcd::"
                 , ""
                 ]

main :: IO ()
main = do
    a <- cmdArgs ip6addrInput
    if output a == "random"
        then replicateM_ (quantity a) (putRandAddr (prefix a)) >> exitSuccess
        else do
            let m = address a
            case output a of
                "canonical" -> out maybeIPv6Addr m fromIPv6Addr
                "full"      -> out maybeFullIPv6Addr m fromIPv6Addr
                "arpa"      -> out maybePureIPv6Addr m toIP6ARPA
                "pure"      -> out maybePureIPv6Addr m fromIPv6Addr
                _           -> hPutStrLn stderr "See help" >> exitFailure
  where
    putRandAddr p = do
        r <- randIPv6AddrWithPrefix $
                 if p == "" then Nothing else Just $ T.pack p
        case r of
            Nothing -> TIO.putStrLn "Bad prefix"
            Just a  -> TIO.putStrLn $ fromIPv6Addr a
    out t i o =
        if i /= ""
            then do
                let p = T.pack i
                case t p of
                    Nothing -> hPutStrLn stderr 
                        ("'" `T.append` p `T.append` "' is not an IPv6 address")
                        >> exitFailure
                    Just a  -> TIO.putStrLn (o a) >> exitSuccess
            else Prelude.putStrLn "See help" >> exitFailure

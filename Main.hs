{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

import           Control.Monad          (replicateM_)
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO (hPutStrLn, putStrLn)
import           System.Console.CmdArgs
import           System.Exit
import           System.IO              (stderr)

import           Text.IPv6Addr

data Input = Input
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
      &= typ " [canonical|pure|full|arpa|unc|random]" &= help "Default : canonical (RFC 5952)"
    , quantity = 1
      &= help "Amount of random addresses to generate"
      &= typ " <Integer>"
    , prefix = ""
      &= typ " <Prefix>"
      &= help "Set a prefix for random addresses generation"
    } &= summary "ip6addr version 0.5.1.2 (C) Michel Boucey 2015-2016"
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
    Input {..} <- cmdArgs ip6addrInput
    if output == "random"
        then replicateM_ quantity (putRandAddr prefix) >> exitSuccess
        else case output of
                 "canonical" -> out maybeIPv6Addr address fromIPv6Addr
                 "pure"      -> out maybePureIPv6Addr address fromIPv6Addr
                 "full"      -> out maybeFullIPv6Addr address fromIPv6Addr
                 "arpa"      -> out maybeIP6ARPA address id
                 "unc"       -> out maybeUNC address id
                 _           -> TIO.hPutStrLn stderr "See help" >> exitFailure
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
                    Nothing -> TIO.hPutStrLn stderr
                        ("'" <> p <> "' is not an IPv6 address")
                        >> exitFailure
                    Just a  -> TIO.putStrLn (o a) >> exitSuccess
            else Prelude.putStrLn "See help" >> exitFailure
    maybeUNC t =
        maybe Nothing
              (\a -> Just $ (T.concatMap trans $ fromIPv6Addr a) <> ".ipv6-literal.net")
              (maybePureIPv6Addr t)
      where
        trans ':' = "-"
        trans c   = T.pack [c]
    maybeIP6ARPA t =
        maybe Nothing
              (\a -> Just $ T.reverse (T.concatMap trans $ fromIPv6Addr a) <> "IP6.ARPA.")
              (maybeFullIPv6Addr t)
      where
        trans ':' = T.empty
        trans c   = "." <> T.pack [c]

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import           Control.Monad       (replicateM_)
import qualified Data.Text           as T (pack)
import qualified Data.Text.IO        as TIO (hPutStrLn, putStrLn)
import           Data.Version        (showVersion)
import           Options.Applicative
import           Paths_ip6addr       (version)
import           System.Exit
import           System.IO           (stderr)
import           Text.IPv6Addr

data Output
  = Canonical
  | NoIPv4
  | FullLength
  | PTR
  | UNC
  | Random
  deriving (Eq)

data Options =
  Options
    { output   :: Output
    , quantity :: Int
    , prefix   :: String
    , address  :: String
    }

main :: IO ()
main = do
  Options{..} <- execParser opts
  case output of
    Canonical  -> out maybeIPv6Addr address unIPv6Addr
    NoIPv4     -> out maybePureIPv6Addr address unIPv6Addr
    FullLength -> out maybeFullIPv6Addr address unIPv6Addr
    PTR        -> out maybeIP6ARPA address id
    UNC        -> out maybeUNC address id
    Random     -> replicateM_ quantity (putRandAddr prefix) >> exitSuccess
  where
    putRandAddr p = do
      r <- randIPv6AddrWithPrefix (if p == mempty then Nothing else Just (T.pack p))
      case r of
        Just a  -> TIO.putStrLn (unIPv6Addr a)
        Nothing -> TIO.putStrLn "Bad prefix"
    out t i o =
      if i /= mempty
        then do
          let p = T.pack i
          case t p of
            Nothing ->
              TIO.hPutStrLn stderr ("'" <> p <> "' is not an IPv6 address") >> exitFailure
            Just a  -> TIO.putStrLn (o a) >> exitSuccess
        else Prelude.putStrLn "See help" >> exitFailure
    maybeUNC t = toUNC <$> maybePureIPv6Addr t
    maybeIP6ARPA t = toIP6ARPA <$> maybeFullIPv6Addr t

opts :: ParserInfo Options
opts = info (parseOptions <**> helper)
  ( fullDesc
    <> progDesc "ip6addr"
    <> header ( "ip6addr " <> showVer <> " (c) Michel Boucey 2011-2024")
  )

parseOptions :: Parser Options
parseOptions =
  Options
    <$>
      (flag Canonical Canonical
        ( short 'c'
          <> long "canonical"
          <> help "In conformation with RFC 5952 (default)"
        )
   <|>
      flag' NoIPv4
        ( short 'n'
          <> long "no-ipv4"
          <> help "Force the removal of IPv4 address if necessary"
        )
   <|>
      flag' FullLength
        ( short 'f'
          <> long "full-length"
          <> help "Full IPv6 address length"
        )
   <|>
      flag' PTR
        ( short 'p'
          <> long "ptr"
          <> help "PTR reverse mapping"
        )
   <|>
      flag' UNC
        ( short 'w'
          <> long "windows-unc"
          <> help "Windows UNC path name"
        )
   <|>
      flag' Random
        ( short 'r'
          <> long "random"
          <> help "Random generation"
        )
     )
   <*>
      option auto
        ( short 'q'
          <> long "quantity"
          <> help "Amount of random addresses to generate"
          <> value 1
        )
   <*>
      option auto
        ( short 's'
          <> long "prefix"
          <> help "Set a prefix for random addresses generation"
          <> value ""
        )
   <*>
      argument str (metavar "<IPv6 address>" <> value "")

showVer :: String
showVer = "v" <> showVersion version


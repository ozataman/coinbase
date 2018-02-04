{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8           as CBS
import           Data.Maybe
import           Data.Time
import           Data.UUID.Types
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import qualified Network.WebSockets              as WS
import           System.Environment

import           Coinbase.Exchange.MarketData
import           Coinbase.Exchange.Private
import           Coinbase.Exchange.Socket
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core
import           Coinbase.Exchange.Types.Private
import           Coinbase.Exchange.Types.Socket

main :: IO ()
main = printSocket -- putStrLn "Use GHCi."

btc :: ProductId
btc = "BTC-USD"

btcChannel = ChannelSubscription FullChannel [btc]

start :: Maybe UTCTime
start = Just $ parseTimeOrError True defaultTimeLocale "%FT%X%z" "2015-04-12T20:22:37+0000"

end :: Maybe UTCTime
end = Just $ parseTimeOrError True defaultTimeLocale "%FT%X%z" "2015-04-23T20:22:37+0000"

mkConf :: IO ExchangeConf
mkConf = do
        mgr     <- newManager tlsManagerSettings
        tKey    <- liftM CBS.pack $ getEnv "GDAX_KEY"
        tSecret <- liftM CBS.pack $ getEnv "GDAX_SECRET"
        tPass   <- liftM CBS.pack $ getEnv "GDAX_PASSPHRASE"

        sbox    <- getEnv "GDAX_SANDBOX"
        let apiType  = case sbox of
                        "FALSE" -> Live
                        "TRUE"  -> Sandbox
                        _       -> error "Coinbase sandbox option must be either: TRUE or FALSE (all caps)"

        case mkToken tKey tSecret tPass of
            Right tok -> return $ ExchangeConf mgr (Just tok) apiType
            Left   er -> error $ show er

withCoinbase :: Exchange a -> IO a
withCoinbase act = do
        conf <- mkConf
        res <- runExchange conf act
        case res of
            Right s -> return s
            Left  f -> error $ show f

printSocket :: IO ()
printSocket = do
        conf <- mkConf
        subscribe conf Live [btcChannel] $ \conn -> do
            putStrLn "Connected."
            _ <- forkIO $ forever $ do
                ds <- WS.receiveData conn
                let res = eitherDecode ds
                case res :: Either String ExchangeMessage of
                    Left er -> print er
                    Right v -> print v
            _ <- forever $ threadDelay (1000000 * 60)
            return ()

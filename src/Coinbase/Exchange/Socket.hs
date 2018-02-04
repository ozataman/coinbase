{-# LANGUAGE OverloadedStrings #-}

module Coinbase.Exchange.Socket
    ( mkAuth
    , subscribe
    , module Coinbase.Exchange.Types.Socket
    ) where

-------------------------------------------------------------------------------
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Crypto.Hash
import           Data.Aeson
import           Data.Byteable
import qualified Data.ByteString.Base64         as Base64
import qualified Data.ByteString.Char8          as CBS
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import           Data.Time.Clock.POSIX
import           Network.Socket
import qualified Network.WebSockets             as WS
import           Text.Printf
import           Wuss
-------------------------------------------------------------------------------
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core
import           Coinbase.Exchange.Types.Socket

-------------------------------------------------------------------------------
mkAuth :: ExchangeConf -> IO Auth
mkAuth conf = do
    let meth = "GET"
        p = "/users/self/verify"
    case authToken conf of
        Just tok -> do
            time <-
                liftM (realToFrac . utcTimeToPOSIXSeconds) (liftIO getCurrentTime) >>= \t ->
                    return . CBS.pack $ printf "%.0f" (t :: Double)
            let presign = CBS.concat [time, meth, CBS.pack p]
                sign = Base64.encode $ toBytes (hmac (secret tok) presign :: HMAC SHA256)
            return
                Auth
                { authSignature = T.decodeUtf8 sign
                , authKey = T.decodeUtf8 $ key tok
                , authPassphrase = T.decodeUtf8 $ passphrase tok
                , authTimestamp = T.decodeUtf8 time
                }
        Nothing -> throw $ AuthenticationRequiredFailure $ T.pack p

-------------------------------------------------------------------------------
subscribe :: Maybe ExchangeConf -> ApiType -> [ChannelSubscription] -> WS.ClientApp a -> IO a
subscribe conf atype chans app =
    withSocketsDo $ do
        auth <- maybe (pure Nothing) (fmap Just . mkAuth) conf
        runSecureClient location 443 "/" $ \conn -> do
            WS.sendTextData conn $ encode (Subscribe auth chans)
            app conn
  where
    location =
        case atype of
            Sandbox -> sandboxSocket
            Live    -> liveSocket

-------------------------------------------------------------------------------
-- | Enable/disable heartbeat anytime you want in your 'ClientApp'
setHeartbeat :: Bool -> WS.ClientApp ()
setHeartbeat b conn = WS.sendTextData conn (encode $ SetHeartbeat b)

{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Coinbase.Exchange.Types.Socket where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Data.Aeson.Types             hiding (Error)
import           Data.Data
import qualified Data.HashMap.Strict          as H
import           Data.Text                    (Text)
import           Data.Time
import           GHC.Generics

-------------------------------------------------------------------------------
import           Coinbase.Exchange.Types.Core hiding (OrderStatus (..))
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Fields to send along for websocket feed authentication
data Auth = Auth
  { authSignature  :: Text
  , authKey        :: Text
  , authPassphrase :: Text
  , authTimestamp  :: Text
  } deriving (Eq, Show, Read, Data, Typeable, Generic, NFData)

instance ToJSON Auth where
  toEncoding = genericToEncoding defaultOptions

-------------------------------------------------------------------------------
-- | Messages we can send to the exchange
data SendExchangeMessage
  = Subscribe (Maybe Auth) [ChannelSubscription]
  | SetHeartbeat Bool
  deriving (Eq, Show, Read, Data, Typeable, Generic)

instance NFData SendExchangeMessage

-------------------------------------------------------------------------------
-- | Messages they send back to us
data ExchangeMessage
  = Heartbeat
    { msgTime        :: UTCTime
    , msgProductId   :: ProductId
    , msgSequence    :: Sequence
    , msgLastTradeId :: TradeId }
  | ReceivedLimit
    { msgTime      :: UTCTime
    , msgProductId :: ProductId
    , msgSequence  :: Sequence
    , msgOrderId   :: OrderId
    , msgSide      :: Side
    , msgClientOid :: Maybe ClientOrderId
    , msgPrice     :: Price
    , msgSize      :: Size
    , msgUserId    :: Maybe UserId
    , msgProfileId :: Maybe ProfileId }
  | ReceivedMarket
    { msgTime         :: UTCTime
    , msgProductId    :: ProductId
    , msgSequence     :: Sequence
    , msgOrderId      :: OrderId
    , msgSide         :: Side
    , msgClientOid    :: Maybe ClientOrderId
    -- market orders have no price and are bounded by either size, funds or both
    , msgMarketBounds :: (Either Size (Maybe Size, Cost))
    , msgUserId       :: Maybe UserId
    , msgProfileId    :: Maybe ProfileId }
  | Open
    { msgTime          :: UTCTime
    , msgProductId     :: ProductId
    , msgSequence      :: Sequence
    , msgOrderId       :: OrderId
    , msgSide          :: Side
    , msgRemainingSize :: Size
    , msgPrice         :: Price
    , msgUserId        :: Maybe UserId
    , msgProfileId     :: Maybe ProfileId }
  | Match
    { msgTime           :: UTCTime
    , msgProductId      :: ProductId
    , msgSequence       :: Sequence
    , msgSide           :: Side
    , msgTradeId        :: TradeId
    , msgMakerOrderId   :: OrderId
    , msgTakerOrderId   :: OrderId
    , msgSize           :: Size
    , msgPrice          :: Price
    , msgUserId         :: Maybe UserId
    , msgProfileId      :: Maybe ProfileId
    , msgTakerUserId    :: Maybe UserId
    , msgTakerProfileId :: Maybe ProfileId }
  | Done
    { msgTime         :: UTCTime
    , msgProductId    :: ProductId
    , msgSequence     :: Sequence
    , msgOrderId      :: OrderId
    , msgSide         :: Side
    , msgReason       :: Reason
    -- It is possible for these next two fields to be Nothing separately
    -- Filled market orders limited by funds will not have a price but may have remaining_size
    -- Filled limit orders may have a price but not a remaining_size (assumed zero)
    -- CURRENTLY ** `remaining_size` reported in Done messages is sometimes incorrect **
    -- This appears to be bug at GDAX. I've told them about it.
    , msgMaybePrice   :: Maybe Price
    , msgMaybeRemSize :: Maybe Size
    , msgUserId       :: Maybe UserId
    , msgProfileId    :: Maybe ProfileId }
  | ChangeLimit
    { msgTime      :: UTCTime
    , msgProductId :: ProductId
    , msgSequence  :: Sequence
    , msgOrderId   :: OrderId
    , msgSide      :: Side
    , msgPrice     :: Price
    , msgNewSize   :: Size
    , msgOldSize   :: Size
    , msgUserId    :: Maybe UserId
    , msgProfileId :: Maybe ProfileId }
  | ChangeMarket
    { msgTime      :: UTCTime
    , msgProductId :: ProductId
    , msgSequence  :: Sequence
    , msgOrderId   :: OrderId
    , msgSide      :: Side
    , msgNewFunds  :: Cost
    , msgOldFunds  :: Cost
    , msgUserId    :: Maybe UserId
    , msgProfileId :: Maybe ProfileId }
  | Activate
    { msgTime            :: UTCTime
    , msgProductId       :: ProductId
    , msgOrderId         :: OrderId
    , msgSide            :: Side
    , msgStopType        :: StopType
    , msgStopPrice       :: Price
    , msgMaybeLimitPrice :: Maybe Price
    , msgFunds           :: Cost
    , msgTakerFeeRate    :: CoinScientific
    , msgUserId          :: Maybe UserId
    , msgProfileId       :: Maybe ProfileId }
  | Error
    { msgMessage :: Text }
  | Subscriptions
    { msgChannels :: [ChannelSubscription]
    }
  | Ticker
    { msgTime      :: UTCTime
    , msgProductId :: ProductId
    , msgSequence  :: Sequence
    , msgTradeId   :: TradeId
    , msgPrice     :: Price
    , msgSide      :: Side
    , msgSize      :: Size
    , msgBestBid   :: Price
    , msgBestAsk   :: Price
    }
  | Snapshot
    { msgProductId :: ProductId
    , msgBids      :: [(Price, Size)]
    , msgAsks      :: [(Price, Size)]
    }
  | L2Update
    { msgProductId :: ProductId
    , msgChanges   :: [(Side, Price, Size)]
    }
  deriving (Eq, Show, Read, Data, Typeable, Generic)


data ChannelType
  = Level2Channel
  | HeartbeatChannel
  | TickerChannel
  | FullChannel
  deriving (Eq,Show,Read,Data,Typeable,Generic)


instance ToJSON ChannelType where
  toJSON s = toJSON $ case s of
    Level2Channel    -> "level2" :: String
    HeartbeatChannel -> "heartbeat"
    TickerChannel    -> "ticker"
    FullChannel      -> "full"


instance FromJSON ChannelType where
  parseJSON (String "level2")    = pure Level2Channel
  parseJSON (String "heartbeat") = pure HeartbeatChannel
  parseJSON (String "ticker")    = pure TickerChannel
  parseJSON (String "full")      = pure FullChannel
  parseJSON _                    = mzero


data ChannelSubscription = ChannelSubscription
  { csChannel  :: ChannelType
  , csProducts :: [ProductId]
  } deriving (Eq,Show,Read,Data,Typeable,Generic)


instance ToJSON ChannelSubscription where
  toJSON ChannelSubscription{..} = object
    [ "name" .= csChannel
    , "product_ids" .= csProducts
    ]


instance FromJSON ChannelSubscription where
  parseJSON (Object m) = ChannelSubscription
    <$> m .: "name"
    <*> (do ls <- m .: "product_ids"
            forM ls $ \ (String pn) -> pure (ProductId pn))
  parseJSON _ = mzero


-------------------------------------------------------------------------------
instance NFData ExchangeMessage
instance NFData ChannelType
instance NFData ChannelSubscription
-------------------------------------------------------------------------------



-----------------------------
instance FromJSON ExchangeMessage where
  parseJSON (Object m) = do
    msgtype <- m .: "type"
        -- TO DO: `HeartbeatReq` and `Subscribe` message types are missing as those are
        -- never received by the client.
    case (msgtype :: String) of
      "hearbeat" ->
        Heartbeat <$> m .: "time" <*> m .: "product_id" <*> m .: "sequence" <*>
        m .: "last_trade_id"
      "open" ->
        Open <$> m .: "time" <*> m .: "product_id" <*> m .: "sequence" <*>
        m .: "order_id" <*>
        m .: "side" <*>
        m .: "remaining_size" <*>
        m .: "price" <*>
        m .:? "user_id" <*>
        m .:? "profile_id"
      "done" ->
        Done <$> m .: "time" <*> m .: "product_id" <*> m .: "sequence" <*>
        m .: "order_id" <*>
        m .: "side" <*>
        m .: "reason" <*>
        m .:? "price" <*>
        m .:? "remaining_size" <*>
        m .:? "user_id" <*>
        m .:? "profile_id"
      "match" ->
        Match <$> m .: "time" <*> m .: "product_id" <*> m .: "sequence" <*>
        m .: "side" <*>
        m .: "trade_id" <*>
        m .: "maker_order_id" <*>
        m .: "taker_order_id" <*>
        m .: "size" <*>
        m .: "price" <*>
        m .:? "user_id" <*>
        m .:? "profile_id" <*>
        m .:? "taker_user_id" <*>
        m .:? "taker_profile_id"
      "change" -> do
        ms <- m .:? "price"
        let market =
              ChangeMarket <$> m .: "time" <*> m .: "product_id" <*>
              m .: "sequence" <*>
              m .: "order_id" <*>
              m .: "side" <*>
              m .: "new_funds" <*>
              m .: "old_funds" <*>
              m .:? "user_id" <*>
              m .:? "profile_id"
            limit =
              ChangeLimit <$> m .: "time" <*> m .: "product_id" <*>
              m .: "sequence" <*>
              m .: "order_id" <*>
              m .: "side" <*>
              m .: "price" <*>
              m .: "new_size" <*>
              m .: "old_size" <*>
              m .:? "user_id" <*>
              m .:? "profile_id"
        case (ms :: Maybe Price) of
          Nothing -> market <|> limit
          Just _  -> limit <|> market
      "received" -> do
        typ <- m .: "order_type"
        mcid <- m .:? "client_oid"
        case typ of
          Limit ->
            ReceivedLimit <$> m .: "time" <*> m .: "product_id" <*>
            m .: "sequence" <*>
            m .: "order_id" <*>
            m .: "side" <*>
            pure (mcid :: Maybe ClientOrderId) <*>
            m .: "price" <*>
            m .: "size" <*>
            m .:? "user_id" <*>
            m .:? "profile_id"
          Market ->
            ReceivedMarket <$> m .: "time" <*> m .: "product_id" <*>
            m .: "sequence" <*>
            m .: "order_id" <*>
            m .: "side" <*>
            pure mcid <*>
                                        -- I can't try to parse "size" or "funds" with (.:?) here, their type is CoinScientific
                                        -- but the fields may be "size":null and that will fail the (m .:? "size") parser.
            (do ms <- m .:?? "size"
                mf <- m .:?? "funds"
                case (ms, mf) of
                  (Nothing, Nothing) -> mzero
                  (Just s, Nothing)  -> return $ Left s
                  (Nothing, Just f)  -> return $ Right (Nothing, f)
                  (Just s, Just f)   -> return $ Right (Just s, f)) <*>
            m .:? "user_id" <*>
            m .:? "profile_id"
      "activate" ->
        Activate <$> m .: "time" <*> m .: "product_id" <*> m .: "order_id" <*>
        m .: "side" <*>
        m .: "stop_type" <*>
        m .: "stop_price" <*>
        m .:? "limit_price" <*>
        m .: "funds" <*>
        m .: "taker_fee_rate" <*>
        m .:? "user_id" <*>
        m .:? "profile_id"
      "error" -> error (show m)
      "subscriptions" -> Subscriptions <$> m .: "channels"
      "ticker" -> Ticker
        <$> m .: "time"
        <*> m .: "product_id"
        <*> m .: "sequence"
        <*> m .: "trade_id"
        <*> m .: "price"
        <*> m .: "side"
        <*> m .: "last_size"
        <*> m .: "best_bid"
        <*> m .: "best_ask"
      "snapshot" ->
        let row [s, p] = (,) <$> parseJSON s <*> parseJSON p
        in Snapshot
           <$> m .: "product_id"
           <*> (m .: "bids" >>= mapM row)
           <*> (m .: "asks" >>= mapM row)
      "l2update" ->
        let row [sd, sz, p] = (,,) <$> parseJSON sd <*> parseJSON sz <*> parseJSON p
        in L2Update
           <$> m .: "product_id"
           <*> (m .: "changes" >>= mapM row)

      x -> error ("Unknown message type: " ++ show (x, m))
  parseJSON _ = mzero

---------------------------
-- This is based on the code for Aeson's (.:?) operator. Except, we're more
-- lax than (.:?) and also return 'Nothing' when the field is (JSON) null.
(.:??) :: (FromJSON a) => Object -> Text -> Parser (Maybe a)
obj .:?? key =
  case H.lookup key obj of
    Nothing -> pure Nothing
    Just v ->
      if v == Null
        then pure Nothing
        else obj .:? key

-------------------------------------------------------------------------------
instance ToJSON SendExchangeMessage where
  toJSON (Subscribe auth0 chans) =
    let authBits auth =
          [ "signature" .= authSignature auth
          , "key" .= authKey auth
          , "passphrase" .= authPassphrase auth
          , "timestamp" .= authTimestamp auth
          ]
    in object $
       [ "type" .= ("subscribe" :: Text)
       , "channels" .= chans
       ] ++ maybe [] authBits auth0
  toJSON (SetHeartbeat b) = object ["type" .= ("heartbeat" :: Text), "on" .= b]

-------------------------------------------------------------------------------
-- | Convenience/storage instance; never sent to exchange
instance ToJSON ExchangeMessage where
  toJSON Open {..} =
    object $
    [ "type" .= ("open" :: Text)
    , "time" .= msgTime
    , "product_id" .= msgProductId
    , "sequence" .= msgSequence
    , "order_id" .= msgOrderId
    , "side" .= msgSide
    , "remaining_size" .= msgRemainingSize
    , "price" .= msgPrice
    ] ++
    optionalField "user_id" msgUserId ++ optionalField "profile_id" msgProfileId
  toJSON Done {..} =
    object $
    [ "type" .= ("done" :: Text)
    , "time" .= msgTime
    , "product_id" .= msgProductId
    , "sequence" .= msgSequence
    , "order_id" .= msgOrderId
    , "side" .= msgSide
    , "reason" .= msgReason
    ] ++
    optionalField "price" msgMaybePrice ++
    optionalField "remaining_size" msgMaybeRemSize ++
    optionalField "user_id" msgUserId ++ optionalField "profile_id" msgProfileId
  toJSON Match {..} =
    object $
    [ "type" .= ("match" :: Text)
    , "time" .= msgTime
    , "product_id" .= msgProductId
    , "sequence" .= msgSequence
    , "side" .= msgSide
    , "trade_id" .= msgTradeId
    , "maker_order_id" .= msgMakerOrderId
    , "taker_order_id" .= msgTakerOrderId
    , "size" .= msgSize
    , "price" .= msgPrice
    ] ++
    optionalField "user_id" msgUserId ++
    optionalField "profile_id" msgProfileId ++
    optionalField "taker_user_id" msgTakerUserId ++
    optionalField "taker_profile_id" msgTakerProfileId
  toJSON Error {..} =
    object ["type" .= ("error" :: Text), "message" .= msgMessage]
  toJSON ChangeLimit {..} =
    object $
    [ "type" .= ("change" :: Text)
    , "time" .= msgTime
    , "product_id" .= msgProductId
    , "sequence" .= msgSequence
    , "order_id" .= msgOrderId
    , "side" .= msgSide
    , "new_size" .= msgNewSize
    , "old_size" .= msgOldSize
    , "price" .= msgPrice
    ] ++
    optionalField "user_id" msgUserId ++ optionalField "profile_id" msgProfileId
  toJSON ChangeMarket {..} =
    object $
    [ "type" .= ("change" :: Text)
    , "time" .= msgTime
    , "product_id" .= msgProductId
    , "sequence" .= msgSequence
    , "order_id" .= msgOrderId
    , "side" .= msgSide
    , "new_funds" .= msgNewFunds
    , "old_funds" .= msgOldFunds
    ] ++
    optionalField "user_id" msgUserId ++ optionalField "profile_id" msgProfileId
  toJSON ReceivedLimit {..} =
    object $
    [ "type" .= ("received" :: Text)
    , "time" .= msgTime
    , "product_id" .= msgProductId
    , "sequence" .= msgSequence
    , "order_id" .= msgOrderId
    , "side" .= msgSide
    , "size" .= msgSize
    , "price" .= msgPrice
    , "order_type" .= Limit
    ] ++
    optionalField "client_id" msgClientOid ++
    optionalField "user_id" msgUserId ++ optionalField "profile_id" msgProfileId
  toJSON ReceivedMarket {..} =
    object $
    [ "type" .= ("received" :: Text)
    , "time" .= msgTime
    , "product_id" .= msgProductId
    , "sequence" .= msgSequence
    , "order_id" .= msgOrderId
    , "side" .= msgSide
    , "order_type" .= Market
    ] ++
    size ++
    funds ++
    optionalField "client_id" msgClientOid ++
    optionalField "user_id" msgUserId ++ optionalField "profile_id" msgProfileId
    where
      (size, funds) =
        case msgMarketBounds of
          Left s -> (["size" .= s], [])
          Right (ms, f) ->
            case ms of
              Nothing -> ([], ["funds" .= f])
              Just s' -> (["size" .= s'], ["funds" .= f])
  toJSON Activate {..} =
    object $
    [ "type" .= ("activate" :: Text)
    , "time" .= msgTime
    , "product_id" .= msgProductId
    , "order_id" .= msgOrderId
    , "side" .= msgSide
    , "stop_type" .= msgStopType
    , "stop_price" .= msgStopPrice
    , "funds" .= msgFunds
    , "taker_fee_rate" .= msgTakerFeeRate
    ] ++
    optionalField "limit_price" msgMaybeLimitPrice ++
    optionalField "user_id" msgUserId ++ optionalField "profile_id" msgProfileId

optionalField :: (ToJSON v) => Text -> Maybe v -> [Pair]
optionalField key maybeValue =
  case maybeValue of
    Nothing    -> []
    Just value -> [key .= value]

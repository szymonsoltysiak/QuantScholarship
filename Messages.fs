module Messages

open Configuration
open Model
open Trades

//this is currently payment specific, once we add other trades we will need to re-model this
//other trades will have fields that payment doesn't have
type TradeChangeMsg =
    | NewName of TradeID * string
    | NewPrincipal of TradeID * string
    | NewCurrency of TradeID * string
    | NewExpiry of TradeID * string

/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | AddPayment
    | RemoveTrade of TradeID
    | TradeChange of TradeChangeMsg
    | RecalculateAll
    | LoadData
    | GotConfig of JsonConfig
    | ConfigChange of string * string
    | GotMarketData of JsonConfig
    | MarketDataChange of string * string
    | Warning of string
    | Error of exn
    | ClearError

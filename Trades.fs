module Trades
open Payment

type Trade = 
    | Payment of PaymentRecord 
    | OptionCall of OptionCallRecord

type TradeID = System.Guid

let newTradeID () : TradeID= System.Guid.NewGuid()

module Trades
open Payment

type Trade = 
    | Payment of PaymentRecord 
    | OptionCall of OptionRecord
    | OptionPut of OptionRecord

type TradeID = System.Guid

let newTradeID () : TradeID= System.Guid.NewGuid()

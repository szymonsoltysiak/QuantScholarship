module Trades
open Payment

type Trade = Payment of PaymentRecord

type TradeID = System.Guid

let newTradeID () : TradeID= System.Guid.NewGuid()

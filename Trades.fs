module Trades
open Payment

type Trade = 
    | Payment of PaymentRecord 
    | OptionCall of OptionCallRecord
    | OptionCallMonteCarlo of OptionCallMonteCarloRecord
    | OptionPut of OptionPutRecord
    | OptionPutMonteCarlo of OptionPutMonteCarloRecord

type TradeID = System.Guid

let newTradeID () : TradeID= System.Guid.NewGuid()

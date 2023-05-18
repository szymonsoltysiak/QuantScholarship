module Valuation
open Trades
open Payment

let valuateTrade config marketData (trade : Trade) : Trade =
  match trade with
  | Payment p -> 
      let inputs = 
        { Trade = p
          Data = config
          MarketData = marketData
        }
      let vm = PaymentValuationModel(inputs)
      Payment { p with Value = Some <| vm.Calculate()}




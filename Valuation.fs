module Valuation
open Trades
open Payment

let valuateTrade config marketData (trade : Trade) : Trade =
  match trade with
  | Payment p -> 
    let inputs: PaymentValuationInputs = 
      { Trade = p
        Data = config
        MarketData = marketData
      }
    let vm = PaymentValuationModel(inputs)
    Payment { p with Value = Some <| vm.Calculate()}

  | OptionCall o ->
    let inputs: OptionCallValuationInputs = 
      { Trade = o
        Data = config
        MarketData = marketData
      }
    let vm = OptionCallValuationModel(inputs)
    OptionCall { o with Value = Some <| vm.Calculate()}

  | OptionCallMonteCarlo o ->
    let inputs: OptionCallMonteCarloValuationInputs = 
      { Trade = o
        Data = config
        MarketData = marketData
      }
    let vm = OptionCallValuationModelMonteCarlo(inputs)
    OptionCallMonteCarlo { o with Value = Some <| vm.Calculate()}




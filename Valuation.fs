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
    OptionCall { o with Value = Some <| vm.Calculate()
                        Delta = Some <| vm.CalculateDelta()}

  | OptionCallMonteCarlo o ->
    let inputs: OptionCallMonteCarloValuationInputs = 
      { Trade = o
        Data = config
        MarketData = marketData
      }
    let vm = OptionCallValuationModelMonteCarlo(inputs)
    OptionCallMonteCarlo { o with Value = Some <| vm.Calculate()}

  | OptionPut o ->
    let inputs: OptionPutValuationInputs = 
      { Trade = o
        Data = config
        MarketData = marketData
      }
    let vm = OptionPutValuationModel(inputs)
    OptionPut { o with Value = Some <| vm.Calculate()
                       Delta = Some <| vm.CalculateDelta()}

  | OptionPutMonteCarlo o ->
    let inputs: OptionPutMonteCarloValuationInputs = 
      { Trade = o
        Data = config
        MarketData = marketData
      }
    let vm = OptionPutValuationModelMonteCarlo(inputs)
    OptionPutMonteCarlo { o with Value = Some <| vm.Calculate()}








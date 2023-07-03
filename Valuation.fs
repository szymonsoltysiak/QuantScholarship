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
                        ValueMC = Some <| vm.CalculateMC()
                        Delta = Some <| vm.CalculateDelta()}

  | OptionPut o ->
    let inputs: OptionPutValuationInputs = 
      { Trade = o
        Data = config
        MarketData = marketData
      }
    let vm = OptionPutValuationModel(inputs)
    OptionPut { o with Value = Some <| vm.Calculate()
                       ValueMC = Some <| vm.CalculateMC()
                       Delta = Some <| vm.CalculateDelta()}









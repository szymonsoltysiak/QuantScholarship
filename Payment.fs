module Payment
open System
open Configuration
open Money
open MathNet.Numerics.Distributions

(* Model for Payment trade. *)
type PaymentRecord =
    {
        TradeName : string
        Expiry    : DateTime
        Currency  : string
        Principal : int64
        Value     : Money option
    }
    
    (* Simple utility method for creating a random payment. *)
    static member sysRandom = System.Random()
    static member Random(marketData : MarketData) = 
        (* We pick a random currency either from given short list, or from valuation::knownCurrencies config key *)
        let knownCurrenciesDefault = [| "EUR"; "USD"; "PLN"; |]
        
        let knownCurrencies = if marketData.ContainsKey "valuation::knownCurrencies" 
                              then marketData.["valuation::knownCurrencies"].Split([|' '|])
                              else knownCurrenciesDefault
        
        {
            TradeName = sprintf "Payment%04d" (PaymentRecord.sysRandom.Next(9999))
            Expiry    = (DateTime.Now.AddMonths (PaymentRecord.sysRandom.Next(1, 6))).Date
            Currency  = knownCurrencies.[ PaymentRecord.sysRandom.Next(knownCurrencies.Length) ]
            Principal = int64 (PaymentRecord.sysRandom.Next())
            Value = None
        }

(* Complete set of data required for valuation *)
type PaymentValuationInputs = 
    {
        Trade : PaymentRecord
        Data : Configuration
        MarketData: MarketData
    }

(* The valuation model for Payment. We may have multiple valuation models implementations per given trade type, or have a valuation model that handles multiple trade types. *)
type PaymentValuationModel(inputs: PaymentValuationInputs) = 
    (* Calculate() method returns a value of given trade. This one is very simple, yet demonstrates some concepts.
    
    It will try to return the result in the global default currency as configured by valuation::baseCurrency key.

    If the valuation::baseCurrency is not defined or we are unable to obtain the FX rate FX::<targetCcy><tradeCcy>, 
    we simply return the value using the trade currency.

    *)
    member this.Calculate() : Money = 
        let tradeCcy = inputs.Trade.Currency

        let targetCcy = match inputs.MarketData.TryFind "valuation::baseCurrency" with
                         | Some ccy -> ccy
                         | None -> tradeCcy

        let fxRateKey = sprintf "FX::%s%s" targetCcy tradeCcy

        let fxRate = if inputs.Data.ContainsKey fxRateKey then float inputs.Data.[ fxRateKey ] else 1.0 // lookup FX rate
        let finalCcy = if inputs.Data.ContainsKey fxRateKey then targetCcy else tradeCcy
        
        { Value = (float inputs.Trade.Principal) / fxRate; Currency = finalCcy }


type OptionCallRecord =
    {
        TradeName    : string
        StockPrice   : float
        StrikePrice  : float
        Expiry       : DateTime
        InterestRate : float
        Volatility   : float
        Value        : Money option
    }
    (* Simple utility method for creating a random option call *)
    static member sysRandom = System.Random()
    static member Random(marketData : MarketData) =
        let price=System.Math.Round((OptionCallRecord.sysRandom.NextDouble()*100.0),2)
        {
            TradeName = sprintf "OptionCall%04d" (OptionCallRecord.sysRandom.Next(9999))
            StockPrice = price
            StrikePrice = System.Math.Round(price*1.1,2)
            Expiry = (DateTime.Now.AddMonths (OptionCallRecord.sysRandom.Next(1, 6))).Date
            Volatility = OptionCallRecord.sysRandom.NextDouble()
            InterestRate = OptionCallRecord.sysRandom.NextDouble()
            Value = None
        }

type OptionCallValuationInputs = 
    {
        Trade : OptionCallRecord
        Data : Configuration
        MarketData: MarketData
    }

type OptionCallValuationModel(inputs: OptionCallValuationInputs) = 
    member this.Calculate() : Money = 
        let currency = match inputs.MarketData.TryFind "valuation::baseCurrency" with
                         | Some ccy -> ccy
                         | None -> "USD"
        let gaussianCDF (x: float) =
            Normal.CDF(0.0, 1.0, x)
        
        let S=inputs.Trade.StockPrice
        let K=inputs.Trade.StrikePrice
        let T=inputs.Trade.Expiry.Subtract(DateTime.Now).TotalDays/365.
        let r=inputs.Trade.InterestRate
        let sigma=inputs.Trade.Volatility


        let d1 = (log(S / K) + (r + sigma * sigma / 2.0) * T) / (sigma * sqrt(T))
        let d2 = d1 - sigma * sqrt(T)
        let callPrice = S * gaussianCDF(d1) - K * exp(-r * T) * gaussianCDF(d2)

        { Value = callPrice; Currency = currency}

type OptionCallMonteCarloRecord =
    {
        TradeName    : string
        StockPrice   : float
        StrikePrice  : float
        Expiry       : DateTime
        InterestRate : float
        Volatility   : float
        Value        : Money option
    }
    static member sysRandom = System.Random()
    static member Random(marketData : MarketData) =
        let price=System.Math.Round((OptionCallRecord.sysRandom.NextDouble()*100.0),2)
        {
            TradeName = sprintf "OptionCall%04d" (OptionCallRecord.sysRandom.Next(9999))
            StockPrice = price
            StrikePrice = System.Math.Round(price*1.1,2)
            Expiry = (DateTime.Now.AddMonths (OptionCallRecord.sysRandom.Next(1, 6))).Date
            Volatility = OptionCallRecord.sysRandom.NextDouble()
            InterestRate = OptionCallRecord.sysRandom.NextDouble()
            Value = None
        }

type OptionCallMonteCarloValuationInputs = 
    {
        Trade : OptionCallMonteCarloRecord
        Data : Configuration
        MarketData: MarketData
    }

type OptionCallValuationModelMonteCarlo(inputs: OptionCallMonteCarloValuationInputs) = 
    member this.Calculate() : Money =  
        let currency =
            match inputs.MarketData.TryFind "valuation::baseCurrency" with
            | Some ccy -> ccy
            | None -> "USD"

        let S = inputs.Trade.StockPrice
        let K = inputs.Trade.StrikePrice
        let years = inputs.Trade.Expiry.Subtract(DateTime.Now).TotalDays / 365.0
        let drift = inputs.Trade.InterestRate
        let vol = inputs.Trade.Volatility

        let steps = 365  
        let N = 
            match inputs.MarketData.TryFind "monteCarlo::runs" with
            | Some runs -> int runs
            | None -> 100
        let rand = Normal.Sample(0.0, 1.0)

        let generatePath () =
            let dt = years / float steps
            let rec loop t s vs =
                if t >= years then s, vs
                else
                    let z1 = Normal.Sample(0.0, 1.0)
                    let s' = s * exp((drift - vol ** 2.0 / 2.0) * dt + vol * sqrt(dt) * z1)
                    let vs' = vs + (s' / s - 1.0) ** 2.0 / dt
                    loop (t + dt) s' vs'
            loop 0.0 S 0.0
            
        let paths = List.init N (fun _ -> generatePath())
        let prices = paths |> List.map (fun (s, _) -> s)
        let averagePrice = List.average prices

        // Calculate option value based on average price
        let optionValue = max (averagePrice - K) 0.0

        { Value = optionValue; Currency = currency}    







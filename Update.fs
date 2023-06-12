module Update

open Configuration
open Elmish
open Messages
open Model
open Payment
open System
open System.Net.Http
open System.Net.Http.Json
open Trades
open Valuation


let changeTrade (trades : Map<TradeID,UITrade>) id f =
        match Map.tryFind id trades with
        | Some t -> 
            match f t with
            | Some t' -> Map.add id t' trades, Cmd.none
            | None -> trades, Cmd.ofMsg <| Warning (sprintf "could not update trade %s (%A)" t.Name id)
        | None -> trades, Cmd.ofMsg <| Warning (sprintf "could not find trade %A" id)

let tradeChangeUpdate (model : Model) = function
    | NewName (id,name) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | Payment p -> Some <| Payment { p with TradeName = name}
                                | OptionCall p -> Some <| OptionCall {p with TradeName = name}
                                | OptionCallMonteCarlo p -> Some <| OptionCallMonteCarlo {p with TradeName = name}
                            )
            )

    | NewPrincipal (id,principal) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | Payment p -> 
                                    Int64.TryParse(principal)
                                    |> Utils.ofBool
                                    |> Option.map (fun principal ->
                                            Payment { p with Principal = principal})
                                | OptionCall o ->
                                    None
                                | OptionCallMonteCarlo o ->
                                    None
                            )
            )

    | NewExpiry (id,expiry) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | Payment p -> 
                                    DateTime.TryParse(expiry)
                                    |> Utils.ofBool
                                    |> Option.map (fun expiry ->
                                            Payment { p with Expiry = expiry})
                                | OptionCall p ->
                                    DateTime.TryParse(expiry)
                                    |> Utils.ofBool
                                    |> Option.map (fun expiry ->
                                            OptionCall { p with Expiry = expiry})
                                | OptionCallMonteCarlo p ->
                                    DateTime.TryParse(expiry)
                                    |> Utils.ofBool
                                    |> Option.map (fun expiry ->
                                            OptionCallMonteCarlo { p with Expiry = expiry})
                            )
            )

    | NewCurrency (id,ccy) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | Payment p -> Some <| Payment { p with Currency = ccy}
                                | OptionCall o -> None
                                | OptionCallMonteCarlo o -> None))
    
    | NewStockPrice (id,price) ->
        changeTrade model.trades id
                (Trades.tryMap ( function
                                | OptionCall p -> 
                                    Utils.tryParseCustomFormat(price)
                                    |> Utils.ofBool
                                    |> Option.map (fun price ->
                                            OptionCall { p with StockPrice = price})
                                | OptionCallMonteCarlo p -> 
                                    Utils.tryParseCustomFormat(price)
                                    |> Utils.ofBool
                                    |> Option.map (fun price ->
                                            OptionCallMonteCarlo { p with StockPrice = price})                                            
                                | Payment p -> None
                                )            
                )

    | NewStrikePrice (id, price) ->
        changeTrade model.trades id
                 (Trades.tryMap ( function
                                | OptionCall p -> 
                                    Utils.tryParseCustomFormat(price)
                                    |> Utils.ofBool
                                    |> Option.map (fun price ->
                                            OptionCall { p with StrikePrice = price})
                                | OptionCallMonteCarlo p -> 
                                    Utils.tryParseCustomFormat(price)
                                    |> Utils.ofBool
                                    |> Option.map (fun price ->
                                            OptionCallMonteCarlo { p with StrikePrice = price})                                            
                                | Payment p -> None))

    | NewIntrestRate (id, rate) ->
        changeTrade model.trades id
                (Trades.tryMap ( function
                                | OptionCall p ->
                                    Utils.tryParseCustomFormat(rate)
                                    |> Utils.ofBool
                                    |> Option.map (fun rate ->
                                            OptionCall { p with InterestRate = rate})
                                | OptionCallMonteCarlo p ->
                                    Utils.tryParseCustomFormat(rate)
                                    |> Utils.ofBool
                                    |> Option.map (fun rate ->
                                            OptionCallMonteCarlo { p with InterestRate = rate})                                            
                                | Payment p -> None))
                                
    | NewVolatility (id, vol) ->
        changeTrade model.trades id
                (Trades.tryMap ( function
                                | OptionCall p -> 
                                    Utils.tryParseCustomFormat(vol)
                                    |> Utils.ofBool
                                    |> Option.map (fun vol ->
                                            OptionCall { p with Volatility = vol})
                                | OptionCallMonteCarlo p -> 
                                    Utils.tryParseCustomFormat(vol)
                                    |> Utils.ofBool
                                    |> Option.map (fun vol ->
                                            OptionCallMonteCarlo { p with Volatility = vol})                                            
                                | Payment p -> None))


let update (http: HttpClient) message model =
    match message with
    | SetPage page ->
        { model with page = page }, Cmd.none
    | AddPayment ->
        let newPayment = Trades.wrap (Payment <| PaymentRecord.Random(model.configuration))
        let newTrades = Map.add newPayment.id newPayment model.trades
        { model with trades = newTrades }, Cmd.none
    | AddOptionCall ->
        let newOptionCall = Trades.wrap (OptionCall <| OptionCallRecord.Random(model.configuration))
        let newTrades = Map.add newOptionCall.id newOptionCall model.trades
        { model with trades = newTrades}, Cmd.none
    | AddOptionCallMonte ->
        let newOptionCallMonte = Trades.wrap (OptionCallMonteCarlo <| OptionCallMonteCarloRecord.Random(model.configuration))
        let newTrades = Map.add newOptionCallMonte.id newOptionCallMonte model.trades
        { model with trades = newTrades}, Cmd.none
    | RemoveTrade(tradeId) ->
        let newTrades = Map.remove tradeId model.trades
        { model with trades = newTrades }, Cmd.none
    | TradeChange msg ->
        let newTrades,cmd = tradeChangeUpdate model msg
        { model with trades = newTrades }, Cmd.batch [cmd; Cmd.ofMsg RecalculateAll] 
    | ConfigChange (key,value) ->
        let config = model.configuration
        let config' = Map.add key value config
        { model with configuration = config'}, Cmd.none
    | MarketDataChange (key,value) ->
        let md = model.marketData
        let md' = Map.add key value md
        { model with marketData = md'}, Cmd.ofMsg RecalculateAll
    | GotMarketData response ->
        let c = response |> 
                Array.collect (fun cat -> 
                        cat.Config 
                        |> Array.map (fun {Key = k; Value = v} ->
                            sprintf "%s::%s" cat.Category k, v))
                |> Map.ofArray
        { model with marketData = c }, Cmd.none
    | LoadData ->
        let getConfig() = http.GetFromJsonAsync<JsonConfig>("/configuration.json")
        let conf = Cmd.OfTask.either getConfig () GotConfig Error
        let getMDConfig() = http.GetFromJsonAsync<JsonConfig>("/marketDataConfig.json")
        let mdConf = Cmd.OfTask.either getMDConfig () GotMarketData Error
        { model with configuration = Map.empty }, Cmd.batch [conf; mdConf]
    | GotConfig response -> 
        let c = response |> 
                Array.collect (fun cat -> 
                        cat.Config 
                        |> Array.map (fun {Key = k; Value = v} ->
                            sprintf "%s::%s" cat.Category k, v))
                |> Map.ofArray
        { model with configuration = c }, Cmd.none
    | RecalculateAll ->
        let trades =
             model.trades
             |> Map.map (fun _ -> Trades.map <| valuateTrade model.marketData model.configuration)
        { model with trades = trades }, Cmd.none
    | Error exn ->
        { model with error = Some exn.Message }, Cmd.none
    | Warning err ->
        { model with error = Some err }, Cmd.none
    | ClearError ->
        { model with error = None }, Cmd.none

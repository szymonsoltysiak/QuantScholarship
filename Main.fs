module Pricer.Client.Main

open Bolero
open Elmish
open Microsoft.AspNetCore.Components
open Messages
open Model
open System.Net.Http
open Update
open View

/// Connects the routing system to the Elmish application.
let router = Router.infer SetPage (fun model -> model.page)

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    [<Inject>]
    member val HttpClient = Unchecked.defaultof<HttpClient> with get, set

    override this.Program =
        let update = update this.HttpClient
        Program.mkProgram (fun _ -> Model.Initial, Cmd.ofMsg LoadData ) update (view router)
        |> Program.withRouter router
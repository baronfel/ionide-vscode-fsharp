namespace Ionide.VSCode.FSharp

open Fable.Import
open Fable.Import.vscode

open DTO

module Help =

    let getHelp () =
        let te = window.activeTextEditor
        let doc = te.document
        let pos = te.selection.start

        promise {
            let! res = LanguageService.f1Help (doc.fileName) (int pos.line) (int pos.character)
            let api =
                res.Data.Replace("#ctor", "-ctor")

            let uri = Uri.parse (sprintf "https://docs.microsoft.com/en-us/dotnet/api/%s" api)

            return! commands.executeCommand("vscode.open", uri)
        } |> ignore


    let activate (context : ExtensionContext) =
        let registerCommand com (f : unit -> _) =
            vscode.commands.registerCommand(com, f |> objfy2)
            |> context.subscriptions.Add

        registerCommand "fsharp.getHelp" getHelp

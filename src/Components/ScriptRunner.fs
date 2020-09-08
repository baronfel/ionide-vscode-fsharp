namespace Ionide.VSCode.FSharp

open Fable.Import.vscode
open global.Node
module node = Node.Api

module ScriptRunner =

    let private runFile () =
        let scriptFile = window.activeTextEditor.document.fileName
        let scriptDir = node.path.dirname(scriptFile)

        promise {
            let! (fsiBinary, fsiParameters) = Fsi.fsiBinaryAndParameters ()
            let flatArgs  =
                fsiParameters
                |> Array.map (sprintf "\"%s\"")
                |> String.concat " "
            let (shellCmd, shellArgs, textToSend) =
                match node.os.``type``() with
                | "Windows_NT" ->
                    ("cmd.exe",
                     [| "/Q"; "/K" |],
                     sprintf "cd \"%s\" && cls && \"%s\" %s \"%s\" && pause && exit" scriptDir fsiBinary flatArgs scriptFile)
                | _ ->
                    ("sh",
                     [||],
                     sprintf "cd \"%s\" && clear && \"%s\" %s \"%s\" && echo \"Press enter to close script...\" && read && exit" scriptDir fsiBinary flatArgs scriptFile)

            let title = node.path.basename scriptFile
            let terminal = window.createTerminal(title, shellCmd, shellArgs)
            terminal.sendText(textToSend)
            terminal.show ()
        }


    let activate (context : ExtensionContext) =
        commands.registerCommand("fsharp.scriptrunner.run", runFile |> objfy2) |> context.subscriptions.Add

namespace Ionide.VSCode.FSharp

open System
open Fable.Import.vscode

[<RequireQualifiedAccess>]
module CodeRange =

    type CodeRange = Fable.Import.vscode.Range

    /// Converts Range DTO to VS Code Range.
    let fromDTO (range : DTO.Range) : CodeRange =
        CodeRange (float range.StartLine - 1.,
                   float range.StartColumn - 1.,
                   float range.EndLine - 1.,
                   float range.EndColumn - 1.)

    let fromSimplifiedNameRange (range : DTO.Range) : CodeRange =
        CodeRange (float range.StartLine - 1.,
                   float range.StartColumn - 2.,
                   float range.EndLine - 1.,
                   float range.EndColumn - 2.)

    /// Converts Declaration DTO of a specific `length` to VS Code Range.
    let fromDeclaration (decl : DTO.Declaration) (length : float) : CodeRange =
        CodeRange (float decl.Line - 1.,
                   float decl.Column - 1.,
                   float decl.Line - 1.,
                   float decl.Column + length - 1.)

    /// Converts SymbolUse DTO to VS Code Range.
    let fromSymbolUse (su : DTO.SymbolUse) : CodeRange =
        CodeRange (float su.StartLine - 1.,
                   float su.StartColumn - 1.,
                   float su.EndLine - 1.,
                   float su.EndColumn - 1.)

    let fromError (error : DTO.Error) : CodeRange =
        CodeRange (float error.StartLine - 1.,
                   float error.StartColumn - 1.,
                   float error.EndLine - 1.,
                   float error.EndColumn - 1.)

[<RequireQualifiedAccess>]
module String =

    let trim (s : string) = s.Trim()

    let replace (oldVal : string) (newVal : string) (str : string) : string =
        match str with
        | null -> null
        | _ -> str.Replace (oldVal, newVal)

    let split separator (s : string) = s.Split separator

    let endWith ending (s : string) = s.EndsWith ending

    let startWith ending (s : string) = s.StartsWith ending

    let quote (s : string) =
        let isQuoted (s : string) = s.StartsWith @"""" && s.EndsWith @""""
        let containsWhitespace = Seq.exists (fun c -> c = ' ' || c = '\t' || c = '\n'  )
        let quote = sprintf @"""%s"""
        match s with
        | s when s |> isQuoted |> not && s |> containsWhitespace -> quote s
        | s -> s


[<RequireQualifiedAccess>]
module Option =

    let fill (def : 'a) (x : 'a option) : 'a =
        match x with
        | Some x -> x
        | None -> def

[<RequireQualifiedAccess>]
module Document =
    let (|FSharp|CSharp|VB|Other|) (document : TextDocument) =
        if document.languageId = "fsharp" then FSharp
        else if document.languageId = "csharp" then CSharp
        else if document.languageId = "vb" then VB
        else Other

[<RequireQualifiedAccess>]
module Configuration =

    let tryGet key =
        let configuredValue = workspace.getConfiguration().get(key)
        if configuredValue = "" then None else Some configuredValue

    let get defaultValue key =
        workspace.getConfiguration().get(key, defaultValue)

    let getInContext context defaultValue key =
        workspace.getConfiguration(?resource = Some context).get(key, defaultValue)

    /// write the value to the given key in the workspace configuration
    let set key value =
        workspace.getConfiguration().update(key, value, false)

    /// write the value to the given key in the global configuration
    let setGlobal key value =
        workspace.getConfiguration().update(key, value, true)

[<AutoOpen>]
module Utils =
    open Fable.Core

    let isNotNull o = o |> unbox <> null

    type System.Collections.Generic.Dictionary<'key, 'value> with
        [<Emit("$0.has($1) ? $0.get($1) : null")>]
        member this.TryGet(key: 'key): 'value option = jsNative

[<AutoOpen>]
module JS =
    open Fable.Core
    open Fable.Import.Node

    /// Schedules execution of a one-time callback after delay milliseconds.
    /// Returns a Timeout for use with `clearTimeout`.
    [<Emit("setTimeout($0, $1)")>]
    let setTimeout (callback : unit -> unit) (delay : float) : Base.NodeJS.Timer = jsNative

    /// Cancels a Timeout object created by `setTimeout`.
    [<Emit("clearTimeout($0)")>]
    let clearTimeout (timeout : Base.NodeJS.Timer) : unit = jsNative

    [<Emit("debugger")>]
    let debugger () : unit = failwith "JS Only"

    type JsObject =
        [<Emit("$0[$1]")>]
        member __.get<'a>(key : string) : 'a = jsNative

        [<Emit("$0.hasOwnProperty($1)?$0[$1]:null")>]
        member __.tryGet<'a>(key : string) : Option<'a> = jsNative

        [<Emit("$0.hasOwnProperty($1)")>]
        member __.hasOwnProperty(key : string) : bool = jsNative

        [<Emit("$0[$1]=$2")>]
        member __.set<'a>(key : string, value : 'a) = jsNative

        [<Emit("$0[$1]=$2")>]
        member __.set<'a>(key : string, value : 'a option) = jsNative

        [<Emit("delete $0[$1]")>]
        member __.delete(key : string) : unit = jsNative

        [<Emit("{}")>]
        static member empty : JsObject = jsNative

    type JsObjectAsDictionary<'a> =
        [<Emit("$0[$1]")>]
        member __.get(key : string) : 'a = jsNative

        [<Emit("$0.hasOwnProperty($1)?$0[$1]:null")>]
        member __.tryGet(key : string) : Option<'a> = jsNative

        [<Emit("$0.hasOwnProperty($1)")>]
        member __.hasOwnProperty(key : string) : bool = jsNative

        [<Emit("$0[$1]=$2")>]
        member __.set(key : string, value : 'a) = jsNative

        [<Emit("$0[$1]=$2")>]
        member __.set(key : string, value : 'a option) = jsNative

        [<Emit("delete $0[$1]")>]
        member __.delete(key : string) : unit = jsNative

        [<Emit("{}")>]
        static member empty : JsObjectAsDictionary<'a> = jsNative

    let inline undefined<'a> = unbox<'a> ()

[<AutoOpen>]
module Patterns =

    let (|StartsWith|_|) (pat : string) (str : string)  =
        match str with
        | null -> None
        | _ when str.StartsWith pat -> Some str
        | _ -> None

    let (|Contains|_|) (pat : string) (str : string)  =
        match str with
        | null -> None
        | _ when str.Contains pat -> Some str
        | _ -> None

[<RequireQualifiedAccess>]
module Array =

    let splitAt (n : int) (xs : 'a[]) : 'a[] * 'a[] =
        match xs with
        | [||] | [|_|] -> xs, [||]
        | _ when n >= xs.Length || n < 0 -> xs, [||]
        | _ -> xs.[0..n-1], xs.[n..]

module Markdown =

    open System.Text.RegularExpressions

    let private stringReplacePatterns =
        [ "&lt;", "<"
          "&gt;", ">"
          "&quot;", "\""
          "&apos;", "'"
          "&amp;", "&"
          "<summary>", "**Description**\n\n"
          "</summary>", "\n"
          "<para>", "\n"
          "</para>", "\n"
          "<remarks>", ""
          "</remarks>", "\n" ]

    let private regexReplacePatterns =
        let r pat = Regex(pat, RegexOptions.IgnoreCase)

        let code (strings : string array) =
            let str = strings.[0]
            if str.Contains("\n") then
                "```forceNoHighlight" + str + "```"
            else
                "`" + str + "`"
        let returns = Array.item 0 >> sprintf "\n**Returns**\n\n%s"

        let param (s : string[]) = sprintf "* `%s`: %s"(s.[0].Substring(1, s.[0].Length - 2)) s.[1]

        [ r"<c>((?:(?!<c>)(?!<\/c>)[\s\S])*)<\/c>", code
          r"""<see\s+cref=(?:'[^']*'|"[^"]*")>((?:(?!<\/see>)[\s\S])*)<\/see>""", code
          r"""<param\s+name=('[^']*'|"[^"]*")>((?:(?!<\/param>)[\s\S])*)<\/param>""", param
          r"""<typeparam\s+name=('[^']*'|"[^"]*")>((?:(?!<\/typeparam>)[\s\S])*)<\/typeparam>""", param
          r"""<exception\s+cref=('[^']*'|"[^"]*")>((?:(?!<\/exception>)[\s\S])*)<\/exception>""", param
          r"""<a\s+href=('[^']*'|"[^"]*")>((?:(?!<\/a>)[\s\S])*)<\/a>""", fun s -> (s.[0].Substring(1, s.[0].Length - 2))
          r"<returns>((?:(?!<\/returns>)[\s\S])*)<\/returns>", returns ]

    /// Helpers to create a new section in the markdown comment
    let private suffixXmlKey (tag : string) (value : string) (str : string) =
        match str.IndexOf(tag) with
        | x when x <> -1 ->
            let insertAt =
                if str.Chars(x - 1) = ' ' then
                    x - 1
                else
                    x
            str.Insert(insertAt, value)
        | _ -> str

    let private suffixTypeparam = suffixXmlKey "<typeparam" "\n**Type parameters**\n\n"

    let private suffixException = suffixXmlKey "<exception" "\n**Exceptions**\n\n"

    let private suffixParam = suffixXmlKey "<param" "\n**Parameters**\n\n"

    /// Replaces XML tags with Markdown equivalents.
    /// List of standard tags: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/xml-documentation
    let private replaceXml (str : string) : string =
        let str =
            str
            |> suffixTypeparam
            |> suffixException
            |> suffixParam

        let res =
            regexReplacePatterns
            |> List.fold (fun res (regex : Regex, formatter : string[] -> string) ->
                // repeat replacing with same pattern to handle nested tags, like `<c>..<c>..</c>..</c>`
                let rec loop res : string =
                    match regex.Match res with
                    | m when m.Success ->
                        let splitted =
                            m.Groups
                            |> Seq.cast<Group>
                            |> Seq.map (fun g -> g.Value)
                            |> Seq.toArray
                            |> Array.splitAt 1
                        match splitted with
                        | [| firstGroup |], otherGroups ->
                            loop <| res.Replace(firstGroup, formatter otherGroups)
                        | _ -> failwithf "Error in regex pattern for nested xml tags with %s" res
                    | _ -> res
                loop res
            ) str

        stringReplacePatterns
        |> List.fold (fun (res : string) (oldValue, newValue) ->
            res.Replace(oldValue, newValue)
        ) res

    let private normalizeLeadingSpace (content : string) =
        content
            .Replace("\r\n", "\n")
            .Split('\n')
        |> Array.map(fun line ->
            if line.Length > 1 && line.[0] = ' ' then
                line.[1..]
            else
                line
        )
        |> String.concat "\n"

    let createCommentBlock (comment : string) : MarkdownString =
        comment
        |> replaceXml
        |> normalizeLeadingSpace
        |> (fun v -> MarkdownString v)

    let createCommentString (comment : string) : string =
        comment
        |> replaceXml
        |> normalizeLeadingSpace

module Promise =

    open Fable.Import.JS
    open Ionide.VSCode.Helpers

    let suppress (pr : Promise<'T>) =
        pr |> Ionide.VSCode.Helpers.Promise.catch (fun _ -> promise { () })

    let executeForAll f items =
        match items with
        | [] -> Ionide.VSCode.Helpers.Promise.lift (null |> unbox)
        | [x] -> f x
        | x::tail ->
            tail |> List.fold (fun acc next -> acc |> Ionide.VSCode.Helpers.Promise.bind (fun _ -> f next)) (f x)

    /// flattens a result-returning call by throwing and tostringing the error
    let inline mapResult (f: 'a -> Result<'b, _>) (p: Promise<'a>) : Promise<'b> =
        p.``then``(fun r -> match f r with | Ok o -> o | Error e -> failwith (string e))

module Event =

    let invoke (listener : 'T -> _) (event : Fable.Import.vscode.Event<'T>) =
        event.Invoke(unbox<System.Func<_, _>>(fun a -> listener a))

module Context =

    open Fable.Import

    let set<'a> (name : string) (value : 'a) =
        vscode.commands.executeCommand("setContext", name, value) |> ignore

    let cachedSetter<'a when 'a : equality> (name : string) =
        let mutable current : 'a option = None
        fun (value : 'a) ->
            if current <> Some value then
                set name value
                current <- Some value

open Fable.Import
open Fable.Core
open Ionide.VSCode.Helpers

[<AllowNullLiteral>]
type ShowStatus private (panel : WebviewPanel, body : string) as this =
    let renderPage (body : string) =
        sprintf
            """<!DOCTYPE html>
<html lang="en">
<head>
</head>
<body>
%s
</body>
</html>""" body

    let mutable _disposables : ResizeArray<Disposable> = ResizeArray<Disposable>()

    static let mutable instance : ShowStatus = null

    do
        panel.onDidDispose.Invoke((fun () ->
            this.Dispose()
            null
        ), this, _disposables)
        |> ignore

        panel.onDidChangeViewState.Invoke((fun _ev ->
            if panel.visible then
                this.Update()
            null
        ), this, _disposables)
        |> ignore

        this.Update()

    static member ViewType = "project_status"

    static member CreateOrShow(projectPath : string, projectName : string) =
        let title = sprintf "Project %s status" projectName
        let panel = vscode.window.createWebviewPanel(ShowStatus.ViewType, title, U2.Case1 vscode.ViewColumn.One)

        promise {
            let uri = vscode.Uri.parse(sprintf "fsharp-workspace:projects/status?path=%s" (JS.encodeURIComponent(projectPath)))
            let! doc = vscode.workspace.openTextDocument uri
            return doc.getText()
        }
        |> Promise.onSuccess (fun bodyStr ->
            printfn "%s" bodyStr
            instance <- new ShowStatus(panel, bodyStr)
        )
        |> Promise.onFail (fun err ->
            JS.console.error("ShowStatus.CreateOrShow failed:\n", err)
            vscode.window.showErrorMessage("We couldn't generate the status report")
            |> ignore
        )
        |> ignore

    member __.Dispose() =
        instance <- null
        panel.dispose() |> ignore

        for disposable in _disposables do
            if isNotNull disposable then
                disposable.dispose() |> ignore

        _disposables <- ResizeArray<Disposable>()

    member __.Update() =
        panel.webview.html <- renderPage body

[<RequireQualifiedAccess>]
module VSCodeExtension =

    let private extensionName =
#if IONIDE_EXPERIMENTAL
        "experimental-fsharp"
#else
        "ionide-fsharp"
#endif

    let ionidePluginPath () =

        let capitalize (s: string) =
            sprintf "%c%s" (s.[0] |> Char.ToUpper) (s.Substring(1))

        let oldExtensionName = capitalize extensionName

        try
            (VSCode.getPluginPath (sprintf "Ionide.%s" extensionName))
        with
        | _ -> (VSCode.getPluginPath (sprintf "Ionide.%s" oldExtensionName))

    let workbenchViewId () =
        sprintf "workbench.view.extension.%s" extensionName

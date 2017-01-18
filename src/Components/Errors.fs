namespace Ionide.VSCode.FSharp

open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.vscode
open Fable.Import.Node

open DTO
open Ionide.VSCode.Helpers
open System.Text.RegularExpressions

module Diagnostics = 
    type DiagnosticProvider(diagnosticExtractor : ParseResult -> string -> JS.Promise<(Diagnostic * string) seq>, ?name : string) = 
        let mutable currentDiagnostic = 
            match name with
            | Some s -> languages.createDiagnosticCollection s
            | None -> languages.createDiagnosticCollection ()
        
        let mapResult (ev : ParseResult) =
            ev.Data.File, diagnosticExtractor ev window.activeTextEditor.document.fileName

        let parse path text version =
            promise {
                let! ev = LanguageService.parse path text version
                if isNotNull ev then
                    let (file, p) = mapResult ev
                    let! diags = p
                    (Uri.parse file, diags |> Seq.map fst |> ResizeArray) |> currentDiagnostic.set
            }

        let parseFile (file : TextDocument) =
            match file with
            | Document.FSharp ->
                let path = file.fileName
                let prom = Project.find path
                match prom with
                | Some p -> p
                            |> Project.load
                            |> Promise.bind (fun _ -> parse path (file.getText ()) file.version)
                | None -> parse path (file.getText ()) file.version
            | _ -> Promise.lift (null |> unbox)

        let mutable timer = None

        let handleChange (event : TextDocumentChangeEvent) =
            timer |> Option.iter(clearTimeout)
            timer <- Some (setTimeout((fun _ ->
                match event.document with
                | Document.FSharp ->  parse (event.document.fileName) (event.document.getText ()) event.document.version
                | _ -> promise { () } ), 1000.))

        let handlerSave (doc : TextDocument) =
            match doc with
            | Document.FSharp ->
                promise {
                    let! (res : ParseResult) = LanguageService.parseProjects doc.fileName
                    let (_,mapped) = res |> mapResult
                    currentDiagnostic.clear ()
                    let! mapped = mapped
                    mapped
                    |> Seq.groupBy snd
                    |> Seq.iter (fun (fn, errors) ->
                        let errs = errors |> Seq.map fst |> ResizeArray
                        currentDiagnostic.set(Uri.file fn, errs) )
                }
            | _ -> Promise.empty

        let handlerOpen (event : TextEditor) =
            if JS.isDefined event then
                parseFile event.document
            else
                Promise.lift ()

        member x.activate (disposables: Disposable[]) =
            workspace.onDidChangeTextDocument $ (handleChange,(), disposables) |> ignore
            workspace.onDidSaveTextDocument $ (handlerSave , (), disposables) |> ignore
            window.onDidChangeActiveTextEditor $ (handlerOpen, (), disposables) |> ignore

            match window.visibleTextEditors |> Seq.toList with
            | [] -> Promise.lift (null |> unbox)
            | [x] -> parseFile x.document
                    |> Promise.bind (fun _ -> handlerSave x.document)
            | x::tail ->
                tail
                |> List.fold (fun acc e -> acc |> Promise.bind(fun _ -> parseFile e.document)) (parseFile x.document)
                |> Promise.bind (fun _ -> handlerSave x.document )
        
module Errors =
    let mutable private currentDiagnostic = languages.createDiagnosticCollection ()

    let extractErrors (parseResult : ParseResult) fileName = 
        parseResult.Data.Errors
        |> Seq.distinctBy (fun error -> error.Severity, error.StartLine, error.StartColumn)
        |> Seq.choose (fun error ->
            try
                if fileName |> String.startWith "\\" then None else
                let range = CodeRange.fromError error
                let loc = Location (Uri.file error.FileName, range |> Case1)
                let severity = if error.Severity = "Error" then 0 else 1
                (Diagnostic(range, error.Message, unbox severity), error.FileName) |> Some
            with
            | _ -> None )
        |> Promise.lift

    let activate disposables = 
        let diagnosticProvider = Diagnostics.DiagnosticProvider(extractErrors)
        diagnosticProvider.activate disposables

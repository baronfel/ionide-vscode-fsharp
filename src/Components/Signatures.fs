namespace Ionide.VSCode.FSharp

open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.vscode
open Fable.Import.Node

open DTO
open Ionide.VSCode.Helpers

/// module that inspects lines for parameters and return types missing signatures and registers diagnostics to insert them
module Signatures =
    
    let symbolsToSig (doc : TextDocument) (symbols : Symbols []) : Diagnostic [] = [||]

    let extractSignatureDiagnostic (pr : ParseResult) (fileName : string) = 
        promise {
            let! result = LanguageService.declarations window.activeTextEditor.document.fileName
            let diags = symbolsToSig window.activeTextEditor.document result.Data
            return diags |> Seq.map (fun d -> d, window.activeTextEditor.document.fileName)
        }


    let activate disposables = 
        let provider = Diagnostics.DiagnosticProvider(extractSignatureDiagnostic, "signatures")
        provider.activate disposables
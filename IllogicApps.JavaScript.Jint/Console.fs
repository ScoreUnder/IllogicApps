module IllogicApps.JavaScript.Jint.Console

open System
open Jint.Native
open Jint.Runtime.Descriptors
open Jint.Runtime.Interop

let makeConsoleObj engine =
    let obj = JsObject(engine)

    obj.FastSetProperty(
        "log",
        PropertyDescriptor(
            ClrFunction(
                engine,
                "log",
                (fun this args ->
                    // TODO: Not a strictly compliant definition :)
                    args |> Seq.map _.ToString() |> String.concat " " |> Console.WriteLine
                    JsValue.Undefined)
            ),
            false,
            true,
            false
        )
    )

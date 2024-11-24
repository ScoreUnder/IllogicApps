module internal IllogicApps.Compat.LogicAppUnit.InternalHelper

let inline sanitiseNull (def: 'a) (value: 'a) = if isNull value then def else value

module ResultEx =
    let inline recover ([<InlineIfLambda>] fo) ([<InlineIfLambda>] fe) =
        function
        | Ok x -> fo x
        | Error e -> fe e

namespace LogicAppUnit

open System
open System.Collections.Generic
open System.Net.Http

type MockRequest() =
    member val Timestamp = DateTime.Now with get, set
    member val RequestUri: Uri = null with get, set
    member val Method: HttpMethod = null with get, set
    member val Headers: Dictionary<string, IEnumerable<string>> = null with get, set
    member val Content: string = null with get, set
    member val ContentHeaders: Dictionary<string, IEnumerable<string>> = null with get, set

module IllogicApps.Core.ExternalServiceTypes

open System.Text.Json
open System.Text.Json.Nodes

type HttpRequest() =
    override this.ToString() = JsonSerializer.Serialize(this)
    member val Method = "" with get, set
    member val Uri = "" with get, set
    member val Headers = Map.empty<string, string> with get, set
    member val QueryParameters = Map.empty<string, string> with get, set
    member val Body: string option = None with get, set
    member val Cookie: string option = None with get, set
    member val Authentication: JsonObject option = None with get, set

type HttpRequestReply() =
    override this.ToString() = JsonSerializer.Serialize(this)
    member val StatusCode = 0 with get, set
    member val Headers = Map.empty with get, set
    member val Body: JsonNode = JsonValue.Create(null) with get, set

type ExternalServiceRequestType = HttpRequest of HttpRequest * (HttpRequestReply ref)

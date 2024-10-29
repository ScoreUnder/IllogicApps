module IllogicApps.Simulator.Test.XmlConversionTests

open System.Text.Json
open System.Text.Json.Nodes
open System.Xml
open NUnit.Framework
open Swensen.Unquote

open IllogicApps.Core.JsonUtil
open TestSimUtil

[<Test>]
let StringifiedJsonOfXmlTest () =
    let expected =
        "{\"root\":{\"#text\":[\"a\",\"c\",\"f\"],\"#comment\":[],\"e\":{\"#cdata-section\":\"Testing\"}}}"

    let expr =
        "@{json(xml('<root>a<!--b-->c<!--d--><e><![CDATA[Testing]]><!-- one big comment --></e>f</root>'))}"

    test <@ expected.Equals(jsonToObject (testExpressionEvaluation expr)) @>


[<TestCase("@json(xml('<root>a<!--b-->c<!--d--><e><![CDATA[Testing]]><!-- one big comment --></e>f</root>'))",
           "{\"root\":{\"#text\":[\"a\",\"c\",\"f\"],\"#comment\":[],\"e\":{\"#cdata-section\":\"Testing\"}}}")>]
[<TestCase("@json(xml('<root>ac<!--big comment--><e><!-- one big comment --></e>f</root>'))",
           "{\"root\":{\"#text\":[\"ac\",\"f\"],\"e\":{}}}")>]
[<TestCase("@json(xml('<root>aaa<e><![CDATA[Testing]]><!-- one big comment --></e>f</root>'))",
           "{\"root\":{\"#text\":[\"aaa\",\"f\"],\"e\":{\"#cdata-section\":\"Testing\"}}}")>]
[<TestCase("@json(xml('<root>aaa<!-- one big comment --><e><![CDATA[Testing]]></e>f</root>'))",
           "{\"root\":{\"#text\":[\"aaa\",\"f\"],\"e\":{\"#cdata-section\":\"Testing\"}}}")>]
[<TestCase("@json(xml('<root>aaa<e><![CDATA[Testing]]></e>f<!-- one big comment --></root>'))",
           "{\"root\":{\"#text\":[\"aaa\",\"f\"],\"e\":{\"#cdata-section\":\"Testing\"}}}")>]
[<TestCase("@json(xml('<root><node1 attr=\"x\"/><node2 attr2=\"y\"><innermost innerattr=\"o&quot;h\">wow<break/>wowow</innermost></node2></root>'))",
           "{ \"root\": { \"node1\": { \"@attr\": \"x\" }, \"node2\": { \"@attr2\": \"y\", \"innermost\": { \"@innerattr\": \"o\\\"h\", \"#text\": [ \"wow\", \"wowow\" ], \"break\": null } } } }")>]
let JsonOfXmlTest expr (expected: string) =
    let expected = JsonSerializer.Deserialize<JsonNode>(expected)
    test <@ jsonsEqual expected (testExpressionEvaluation expr) @>

[<TestCase("@{xml('<root>a<!--b-->c<!--d--><e><![CDATA[Testing]]><!-- one big comment --></e>f</root>')}",
           "<root>a<!--b-->c<!--d--><e><![CDATA[Testing]]><!-- one big comment --></e>f</root>")>]
[<TestCase("@{xml('<root><node1 attr=\"x\"/><node2 attr2=\"y\"><innermost innerattr=\"o&quot;h\">wow</innermost></node2></root>')}",
           "<root><node1 attr=\"x\" /><node2 attr2=\"y\"><innermost innerattr=\"o&quot;h\">wow</innermost></node2></root>")>]
let StringifiedXmlTest expr expected =
    test <@ expected.Equals(jsonToObject (testExpressionEvaluation expr)) @>

[<TestCase("@xml('<root>a<!--b-->c<!--d--><e><![CDATA[Testing]]><!-- one big comment --></e>f</root>')",
           "{ \"$content-type\": \"application/xml;charset=utf-8\", \"$content\": \"PHJvb3Q+YTwhLS1iLS0+YzwhLS1kLS0+PGU+PCFbQ0RBVEFbVGVzdGluZ11dPjwhLS0gb25lIGJpZyBjb21tZW50IC0tPjwvZT5mPC9yb290Pg==\" }")>]
[<TestCase("@xml('<root>ac<!--big comment--><e><!-- one big comment --></e>f</root>')",
           "{ \"$content-type\": \"application/xml;charset=utf-8\", \"$content\": \"PHJvb3Q+YWM8IS0tYmlnIGNvbW1lbnQtLT48ZT48IS0tIG9uZSBiaWcgY29tbWVudCAtLT48L2U+Zjwvcm9vdD4=\" }")>]
[<TestCase("@xml('<root>aaa<e><![CDATA[Testing]]><!-- one big comment --></e>f</root>')",
           "{ \"$content-type\": \"application/xml;charset=utf-8\", \"$content\": \"PHJvb3Q+YWFhPGU+PCFbQ0RBVEFbVGVzdGluZ11dPjwhLS0gb25lIGJpZyBjb21tZW50IC0tPjwvZT5mPC9yb290Pg==\" }")>]
[<TestCase("@xml('<root>aaa<!-- one big comment --><e><![CDATA[Testing]]></e>f</root>')",
           "{ \"$content-type\": \"application/xml;charset=utf-8\", \"$content\": \"PHJvb3Q+YWFhPCEtLSBvbmUgYmlnIGNvbW1lbnQgLS0+PGU+PCFbQ0RBVEFbVGVzdGluZ11dPjwvZT5mPC9yb290Pg==\" }")>]
[<TestCase("@xml('<root>aaa<e><![CDATA[Testing]]></e>f<!-- one big comment --></root>')",
           "{ \"$content-type\": \"application/xml;charset=utf-8\", \"$content\": \"PHJvb3Q+YWFhPGU+PCFbQ0RBVEFbVGVzdGluZ11dPjwvZT5mPCEtLSBvbmUgYmlnIGNvbW1lbnQgLS0+PC9yb290Pg==\" }")>]
[<TestCase("@xml('<root><node1 attr=\"x\"/><node2 attr2=\"y\"><innermost innerattr=\"o&quot;h\">wow</innermost></node2></root>')",
           "{ \"$content-type\": \"application/xml;charset=utf-8\", \"$content\": \"PHJvb3Q+PG5vZGUxIGF0dHI9IngiIC8+PG5vZGUyIGF0dHIyPSJ5Ij48aW5uZXJtb3N0IGlubmVyYXR0cj0ibyZxdW90O2giPndvdzwvaW5uZXJtb3N0Pjwvbm9kZTI+PC9yb290Pg==\" }")>]
[<TestCase("@xml('<root><node1 attr=\"x\"/><node2 attr2=\"y\"><innermost innerattr=\"o&quot;h\">wow<break/>wowow</innermost></node2></root>')",
           "{ \"$content-type\": \"application/xml;charset=utf-8\", \"$content\": \"PHJvb3Q+PG5vZGUxIGF0dHI9IngiIC8+PG5vZGUyIGF0dHIyPSJ5Ij48aW5uZXJtb3N0IGlubmVyYXR0cj0ibyZxdW90O2giPndvd3dvd293PGJyZWFrIC8+PC9pbm5lcm1vc3Q+PC9ub2RlMj48L3Jvb3Q+\" }")>]
let ObjectOfXmlTest expr (expected: string) =
    let expected = JsonSerializer.Deserialize<JsonNode>(expected)
    test <@ jsonsEqual expected (testExpressionEvaluation expr) @>

[<TestCase("@xml(json(xml('<root><node1 attr=\"x\"/><node2 attr2=\"y\"><innermost innerattr=\"o&quot;h\">wow<break/>wowow</innermost></node2></root>')))",
           "{ \"$content-type\": \"application/xml;charset=utf-8\", \"$content\": \"PHJvb3Q+PG5vZGUxIGF0dHI9IngiIC8+PG5vZGUyIGF0dHIyPSJ5Ij48aW5uZXJtb3N0IGlubmVyYXR0cj0ibyZxdW90O2giPndvd3dvd293PGJyZWFrIC8+PC9pbm5lcm1vc3Q+PC9ub2RlMj48L3Jvb3Q+\" }")>]
let XmlJsonRoundTrippingTest expr (expected: string) =
    let expected = JsonSerializer.Deserialize<JsonNode>(expected)
    test <@ jsonsEqual expected (testExpressionEvaluation expr) @>

[<Test>]
let XmlOfXmlTest () =
    test
        <@
            jsonsEqual
                (jsonOf
                    [ "$content-type", jsonOf "application/xml;charset=utf-8"
                      "$content", jsonOf "PHJvb3QgLz4=" ])
                (testExpressionEvaluation "@xml(xml('<root/>'))")
        @>

[<Test>]
let XmlOfJsonTest () =
    test
        <@
            jsonsEqual
                (jsonOf
                    [ "$content-type", jsonOf "application/xml;charset=utf-8"
                      "$content", jsonOf "PGNvdz5tb288L2Nvdz+" ])
                (testExpressionEvaluation "@xml(json('{\"cow\":\"moo\"}'))")
        @>

[<TestCase("""@json(xml(json('{"cow":"moo"}')))""", """{"cow":"moo"}""")>]
[<TestCase("""@json(xml(json('{"animals":{"cow":"moo","pig":"oink","birds":["cheep","tweet","quack","caw"]}}')))""",
           """{"animals":{"cow":"moo","pig":"oink","birds":["cheep","tweet","quack","caw"]}}""")>]
[<TestCase("""@json(xml(json('{"animals":{"cow":"moo","pig":"oink","birds":[{"call":"cheep"},{"call":"tweet"},{"call":"quack"},{"call":"caw"}]}}')))""",
           """{"animals":{"cow":"moo","pig":"oink","birds":[{"call":"cheep"},{"call":"tweet"},{"call":"quack"},{"call":"caw"}]}}""")>]
let JsonXmlRoundTrippingTest expr (expected: string) =
    let expected = JsonSerializer.Deserialize<JsonNode>(expected)
    test <@ jsonsEqual expected (testExpressionEvaluation expr) @>

[<Test>]
let JsonToXmlEmptyObjectIsEmptyDocumentTest () =
    test
        <@
            jsonsEqual
                (jsonOf
                    [ "$content-type", jsonOf "application/xml;charset=utf-8"
                      "$content", jsonOf "" ])
                (testExpressionEvaluation "@xml(json('{}'))")
        @>

[<TestCase("""@json(xml(json('{}')))""")>]
[<TestCase("""@json(xml(json('{"hello":"world","oh no":"another root"}')))""")>]
let JsonInvalidXmlRoundTripTest expr =
    raises<XmlException> <@ testExpressionEvaluation expr @>

module IllogicApps.Simulator.Test.XmlConversionTests

open System.Xml
open NUnit.Framework

open IllogicApps.Json
open TestSimUtil

[<TestCase("@{json(xml('<root>a<!--b-->c<!--d--><e><![CDATA[Testing]]><!-- one big comment --></e>f</root>'))}",
           "{\"root\":{\"#text\":[\"a\",\"c\",\"f\"],\"#comment\":[],\"e\":{\"#cdata-section\":\"Testing\"}}}")>]
[<TestCase("@{json(xml('<a><a/>t<a/></a>'))}", "{\"a\":{\"a\":[null,null],\"#text\":\"t\"}}")>]
[<TestCase("@{json(xml('<?xml version=\"1.0\" encoding=\"utf-8\"?><root/>'))}",
           "{\"?xml\":{\"@version\":\"1.0\",\"@encoding\":\"utf-8\"},\"root\":null}")>]
[<TestCase("@{json(xml('<?xml version=\"1.0\" encoding=\"ascii\"?><root/>'))}",
           "{\"?xml\":{\"@version\":\"1.0\",\"@encoding\":\"ascii\"},\"root\":null}")>]
let StringifiedJsonOfXmlTest expr expected =
    testOrTrace expr <@ String expected = testExpressionEvaluation expr @>


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
    testOrTrace expr <@ Parser.parse expected = testExpressionEvaluation expr @>

[<TestCase("@{xml('<root/>')}", "<root />")>]
[<TestCase("@{xml('<root><node1 attr=\"x\"/><node2 attr2=\"y\"><innermost innerattr=\"o&quot;h\">wow</innermost></node2></root>')}",
           "<root><node1 attr=\"x\" /><node2 attr2=\"y\"><innermost innerattr=\"o&quot;h\">wow</innermost></node2></root>")>]
[<TestCase("@{xml('<root><node1 attr=\"x\"/><node2 attr2=\"y\"><innermost innerattr=\"o&quot;h\">wow<break/>wowow</innermost></node2></root>')}",
           "<root><node1 attr=\"x\" /><node2 attr2=\"y\"><innermost innerattr=\"o&quot;h\">wow<break />wowow</innermost></node2></root>")>]
[<TestCase("@{xml('<root>a<!--b-->c<!--d--><e><![CDATA[Testing]]><!-- one big comment --></e>f</root>')}",
           "<root>a<!--b-->c<!--d--><e><![CDATA[Testing]]><!-- one big comment --></e>f</root>")>]
[<TestCase("@{xml('<root>aaa<!-- one big comment --><e><![CDATA[Testing]]></e>f</root>')}",
           "<root>aaa<!-- one big comment --><e><![CDATA[Testing]]></e>f</root>")>]
[<TestCase("@{xml('<root>aaa<e><![CDATA[Testing]]><!-- one big comment --></e>f</root>')}",
           "<root>aaa<e><![CDATA[Testing]]><!-- one big comment --></e>f</root>")>]
[<TestCase("@{xml('<root>aaa<e><![CDATA[Testing]]></e>f<!-- one big comment --></root>')}",
           "<root>aaa<e><![CDATA[Testing]]></e>f<!-- one big comment --></root>")>]
[<TestCase("@{xml('<root>ac<!--big comment--><e><!-- one big comment --></e>f</root>')}",
           "<root>ac<!--big comment--><e><!-- one big comment --></e>f</root>")>]
[<TestCase("@{xml('<?xml version=\"1.0\" encoding=\"utf-8\"?><root/>')}",
           "<?xml version=\"1.0\" encoding=\"utf-8\"?><root />")>]
[<TestCase("@{xml('<root><!-- comment --></root>')}", "<root><!-- comment --></root>")>]
[<TestCase("@{xml('<root attr=\"val\"/>')}", "<root attr=\"val\" />")>]
[<TestCase("@{xml('<root> a <!-- b --> c </root>')}", "<root> a <!-- b --> c </root>")>]
[<TestCase("@{xml('<root> <!-- a --> b <!-- c --> </root>')}", "<root><!-- a --> b <!-- c --></root>")>]
[<TestCase("@{xml('<root><![CDATA[<>&''\"]]></root>')}", "<root><![CDATA[<>&'\"]]></root>")>]
[<TestCase("@{xml('<root><![CDATA[<!--]]>testing<![CDATA[-->]]></root>')}",
           "<root><![CDATA[<!--]]>testing<![CDATA[-->]]></root>")>]
[<TestCase("@{xml('<root><![CDATA[testing]]><![CDATA[testing2]]></root>')}",
           "<root><![CDATA[testing]]><![CDATA[testing2]]></root>")>]
[<TestCase("@{xml('<aaa:root xmlns:aaa=\"test\" />')}", "<aaa:root xmlns:aaa=\"test\" />")>]
[<TestCase("@{xml('<?xml version=\"1.0\" encoding=\"ucs-2\"?><root/>')}",
           "<?xml version=\"1.0\" encoding=\"ucs-2\"?><root />")>]
[<TestCase("@{xml('<?xml version=\"1.0\" encoding=\"ucs-2le\"?><root/>')}",
           "<?xml version=\"1.0\" encoding=\"ucs-2le\"?><root />")>]
[<TestCase("@{xml('<root>&#65;</root>')}", "<root>A</root>")>]
let StringifiedXmlTest expr expected =
    testOrTrace expr <@ String expected = testExpressionEvaluation expr @>

[<TestCase("@{xml(json('{\"ro:ot\": \"text\"}'))}", "<ot>text</ot>")>]
[<TestCase("@{xml(json('{\"ro>ot\": \"text\"}'))}", "<ro_x003E_ot>text</ro_x003E_ot>")>]
[<TestCase("@{xml(json('{\"ro ot\": \"text\"}'))}", "<ro_x0020_ot>text</ro_x0020_ot>")>]
[<TestCase("@{xml(json('{\"ro=ot\": \"text\"}'))}", "<ro_x003D_ot>text</ro_x003D_ot>")>]
[<TestCase("@{xml(json('{\"ro\\\"ot\": \"text\"}'))}", "<ro_x0022_ot>text</ro_x0022_ot>")>]
[<TestCase("@{xml(json('{\"ro/ot\": \"text\"}'))}", "<ro_x002F_ot>text</ro_x002F_ot>")>]
[<TestCase("@{xml(json('{\"root\": \"te\\\"xt\"}'))}", "<root>te\"xt</root>")>]
[<TestCase("@{xml(json('{\"root\": \"te\nxt\"}'))}", "<root>te\nxt</root>")>]
[<TestCase("@{xml(json('{\"root\": \"te<xt\"}'))}", "<root>te&lt;xt</root>")>]
[<TestCase("@{xml(json('{\"root\": \"te&xt\"}'))}", "<root>te&amp;xt</root>")>]
[<TestCase("@{xml(json('{\"root\": {\"@attr\": \"te\\\"xt\"}}'))}", "<root attr=\"te&quot;xt\" />")>]
[<TestCase("@{xml(json('{\"root\": {\"@attr\": \"te\\\">xt\"}}'))}", "<root attr=\"te&quot;&gt;xt\" />")>]
[<TestCase("@{xml(json('{\"root\": {\"#comment\": \"-->test\"}}'))}",
           "<root><_x0023_comment>--&gt;test</_x0023_comment></root>")>]
[<TestCase("@{xml(json('{\"root\": {\"#comment\": [\"-->test\"]}}'))}",
           "<root><_x0023_comment>--&gt;test</_x0023_comment></root>")>]
let StringifiedXmlOfJsonTest expr expected =
    testOrTrace expr <@ String expected = testExpressionEvaluation expr @>

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
           "{ \"$content-type\": \"application/xml;charset=utf-8\", \"$content\": \"PHJvb3Q+PG5vZGUxIGF0dHI9IngiIC8+PG5vZGUyIGF0dHIyPSJ5Ij48aW5uZXJtb3N0IGlubmVyYXR0cj0ibyZxdW90O2giPndvdzxicmVhayAvPndvd293PC9pbm5lcm1vc3Q+PC9ub2RlMj48L3Jvb3Q+\" }")>]
[<TestCase("@xml('<?xml version=\"1.0\" encoding=\"ucs-2le\"?><root/>')",
           """{"$content-type": "application/xml;charset=utf-8","$content":"PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0idWNzLTJsZSI/Pjxyb290IC8+"}""")>]
[<TestCase("@xml('<?xml version=\"1.0\" encoding=\"utf-16\"?><root/>')",
           """{"$content-type":"application/xml;charset=utf-8","$content":"PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0idXRmLTE2Ij8+PHJvb3QgLz4="}""")>]
[<TestCase("@xml('<?xml version=\"1.0\" encoding=\"martian\"?><root/>')",
           """{"$content-type":"application/xml;charset=utf-8","$content":"PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0ibWFydGlhbiI/Pjxyb290IC8+"}""")>]
let ObjectOfXmlTest expr (expected: string) =
    testOrTrace expr <@ Parser.parse expected = testExpressionEvaluation expr @>

[<TestCase("@xml(json(xml('<root><node1 attr=\"x\"/><node2 attr2=\"y\"><innermost innerattr=\"o&quot;h\">wow<break/>wowow</innermost></node2></root>')))",
           "{ \"$content-type\": \"application/xml;charset=utf-8\", \"$content\": \"PHJvb3Q+PG5vZGUxIGF0dHI9IngiIC8+PG5vZGUyIGF0dHIyPSJ5Ij48aW5uZXJtb3N0IGlubmVyYXR0cj0ibyZxdW90O2giPndvd3dvd293PGJyZWFrIC8+PC9pbm5lcm1vc3Q+PC9ub2RlMj48L3Jvb3Q+\" }")>]
let XmlJsonRoundTrippingTest expr (expected: string) =
    testOrTrace expr <@ Parser.parse expected = testExpressionEvaluation expr @>

[<TestCase("@{xml(json(xml('<root/>')))}", "<root />")>]
[<TestCase("@{xml(json(xml('<root><node1 attr=\"x\"/><node2 attr2=\"y\"><innermost innerattr=\"o&quot;h\">wow</innermost></node2></root>')))}",
           "<root><node1 attr=\"x\" /><node2 attr2=\"y\"><innermost innerattr=\"o&quot;h\">wow</innermost></node2></root>")>]
[<TestCase("@{xml(json(xml('<root><node1 attr=\"x\"/><node2 attr2=\"y\"><innermost innerattr=\"o&quot;h\">wow<break/>wowow</innermost></node2></root>')))}",
           "<root><node1 attr=\"x\" /><node2 attr2=\"y\"><innermost innerattr=\"o&quot;h\">wowwowow<break /></innermost></node2></root>")>]
[<TestCase("@{xml(json(xml('<root>a<!--b-->c<!--d--><e><![CDATA[Testing]]><!-- one big comment --></e>f</root>')))}",
           "<root>acf<e><![CDATA[Testing]]></e></root>")>]
[<TestCase("@{xml(json(xml('<root>aaa<!-- one big comment --><e><![CDATA[Testing]]></e>f</root>')))}",
           "<root>aaaf<e><![CDATA[Testing]]></e></root>")>]
[<TestCase("@{xml(json(xml('<root>aaa<e><![CDATA[Testing]]><!-- one big comment --></e>f</root>')))}",
           "<root>aaaf<e><![CDATA[Testing]]></e></root>")>]
[<TestCase("@{xml(json(xml('<root>aaa<e><![CDATA[Testing]]></e>f<!-- one big comment --></root>')))}",
           "<root>aaaf<e><![CDATA[Testing]]></e></root>")>]
[<TestCase("@{xml(json(xml('<root>ac<!--big comment--><e><!-- one big comment --></e>f</root>')))}",
           "<root>acf<e /></root>")>]
[<TestCase("@{xml(json(xml('<?xml version=\"1.0\" encoding=\"utf-8\"?><root/>')))}",
           "<?xml version=\"1.0\" encoding=\"utf-8\"?><root />")>]
[<TestCase("@{xml(json(xml('<root><!-- comment --></root>')))}", "<root />")>]
[<TestCase("@{xml(json(xml('<root attr=\"val\"/>')))}", "<root attr=\"val\" />")>]
[<TestCase("@{xml(json(xml('<root> a <!-- b --> c </root>')))}", "<root> a  c </root>")>]
[<TestCase("@{xml(json(xml('<root> <!-- a --> b <!-- c --> </root>')))}", "<root> b </root>")>]
[<TestCase("@{xml(json(xml('<root><![CDATA[<>&''\"]]></root>')))}", "<root><![CDATA[<>&'\"]]></root>")>]
[<TestCase("@{xml(json(xml('<root><![CDATA[<!--]]>testing<![CDATA[-->]]></root>')))}",
           "<root><![CDATA[<!--]]><![CDATA[-->]]>testing</root>")>]
let StringifiedXmlJsonRoundTrippingTest expr expected =
    testOrTrace expr <@ String expected = testExpressionEvaluation expr @>

[<Test>]
let XmlOfXmlTest () =
    let expr = "@xml(xml('<root/>'))"

    testOrTrace
        expr
        <@
            Object(
                Map.ofSeq
                    [ "$content-type", String "application/xml;charset=utf-8"
                      "$content", String "PHJvb3QgLz4=" ]
            ) = (testExpressionEvaluation expr)
        @>

[<TestCase("@xml(json('{\"cow\":\"moo\"}'))",
           """{"$content-type":"application/xml;charset=utf-8","$content":"PGNvdz5tb288L2Nvdz4="}""")>]
let XmlOfJsonTest expr (expected: string) =
    testOrTrace expr <@ Parser.parse expected = testExpressionEvaluation expr @>

[<TestCase("@{xml(json('{\"root\": {\"#cdata-section\": \"]]>test\"}}'))}")>]
let InvalidXmlOfJsonTest expr =
    raisesOrTrace<XmlException> expr <@ testExpressionEvaluation expr @>

[<TestCase("""@json(xml(json('{"cow":"moo"}')))""", """{"cow":"moo"}""")>]
[<TestCase("""@json(xml(json('{"animals":{"cow":"moo","pig":"oink","birds":["cheep","tweet","quack","caw"]}}')))""",
           """{"animals":{"cow":"moo","pig":"oink","birds":["cheep","tweet","quack","caw"]}}""")>]
[<TestCase("""@json(xml(json('{"animals":{"cow":"moo","pig":"oink","birds":[{"call":"cheep"},{"call":"tweet"},{"call":"quack"},{"call":"caw"}]}}')))""",
           """{"animals":{"cow":"moo","pig":"oink","birds":[{"call":"cheep"},{"call":"tweet"},{"call":"quack"},{"call":"caw"}]}}""")>]
let JsonXmlRoundTrippingTest expr (expected: string) =
    testOrTrace expr <@ Parser.parse expected = testExpressionEvaluation expr @>

[<Test>]
let JsonToXmlEmptyObjectIsEmptyDocumentTest () =
    let expr = "@xml(json('{}'))"

    testOrTrace
        expr
        <@
            Object(
                Map.ofSeq
                    [ "$content-type", String "application/xml;charset=utf-8"
                      "$content", String "" ]
            ) = (testExpressionEvaluation expr)
        @>

[<TestCase("""@json(xml(json('{}')))""")>]
[<TestCase("""@json(xml(json('{"hello":"world","oh no":"another root"}')))""")>]
let JsonInvalidXmlRoundTripTest expr =
    raisesOrTrace<XmlException> expr <@ testExpressionEvaluation expr @>

[<TestCase("@{xml('<?xml version=\"1.0\" encoding=\"utf-8\"?>')}")>]
[<TestCase("@{xml('<!-- comment -->')}")>]
[<TestCase("@{xml('<root> a <!-- b <!-- c --> d --> e </root>')}")>]
[<TestCase("@{xml('<aaa:root />')}")>]
[<TestCase("@{xml('<?xml standalone=\"yes\" encoding=\"ascii\"?><root/>')}")>]
[<TestCase("@{xml('<?xml version=\"1.0\" standalone=\"yes\" encoding=\"ascii\"?><root/>')}")>]
[<TestCase("@{xml('<?xml version=\"1.0\" standalone=\"yes\" encoding=\"utf-8\"?><root/>')}")>]
[<TestCase("@{xml('<?xml version=\"1.0\" standalone=\"no\" encoding=\"utf-8\"?><root/>')}")>]
[<TestCase("@{xml('<?xml encoding=\"ascii\"?><root/>')}")>]
[<TestCase("@{xml('<?xml version=\"1.99999\"?><root/>')}")>]
[<TestCase("@{xml('<?xml version=\"1.1\"?><root/>')}")>]
[<TestCase("@{xml('some text')}")>]
[<TestCase("@xml('<one/><two/><oatmeal>kirby is a pink guy</oatmeal>')")>]
let InvalidXmlTest expr =
    // Some of these cases are valid, strictly speaking, but they're not supported by the Logic Apps XML parser
    raisesOrTrace<XmlException> expr <@ testExpressionEvaluation expr @>

[<TestCase("@{json(xml('<?xml version=\"1.0\" encoding=\"martian\"?><root/>'))}")>]
[<TestCase("@{json(xml('<?xml version=\"1.0\" encoding=\"ucs-2-le\"?><root/>'))}")>]
[<TestCase("@{json(xml('<?xml version=\"1.0\" encoding=\"utf-16\"?><root/>'))}")>]
let InvalidXmlEncodingTest expr =
    raisesOrTrace<XmlException> expr <@ testExpressionEvaluation expr @>

[<TestCase("@xml(binary('<'))", """{"$content-type":"application/xml;charset=utf-8","$content":"PA=="}""")>]
let AllowInvalidXmlOfBinaryTest expr (expected: string) =
    testOrTrace expr <@ Parser.parse expected = testExpressionEvaluation expr @>

[<TestCase("@json(xml(binary('<')))")>]
let InvalidXmlOfBinaryToJsonTest expr =
    raisesOrTrace<XmlException> expr <@ testExpressionEvaluation expr @>

[<TestCase("@binary(xml('<root/>'))", """{"$content-type":"application/octet-stream","$content":"PHJvb3QgLz4="}""")>]
let BinaryOfXmlTest expr (expected: string) =
    testOrTrace expr <@ Parser.parse expected = testExpressionEvaluation expr @>

[<TestCase("@xml(json('{\"$content\":\"dGVzdA==\",\"$content-type\":\"text/plain;charset=ascii\"}'))",
           """{"$content-type":"application/xml;charset=utf-8","$content":"dGVzdA=="}""")>]
let XmlOfBinaryIgnoresContentTypeTest expr (expected: string) =
    testOrTrace expr <@ Parser.parse expected = testExpressionEvaluation expr @>

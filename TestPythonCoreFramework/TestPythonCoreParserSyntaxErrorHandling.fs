
// PythonCoreFramework.Net - UnitTests for correct handling of SyntaxError during parsing of Python Grammar.
// Written by Richard Magnor Stenbro. 

namespace PythonCoreFramework.UnitTests

open Xunit
open PythonCoreFramework


module TestPythonCoreParserSyntaxErrorHandling =

    [<Fact>]
    let ``Illegal atom UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.Comma(0, 1, [| |]), 0 ); ( Token.EOF([| |]), 2 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseAtom() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Comma(0, 1, [| |]), ex.Data0)
                Assert.Equal( "Illegal literal!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``star expr UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.Comma(0, 1, [| |]), 0 ); ( Token.EOF([| |]), 2 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseStarExpr() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Comma(0, 1, [| |]), ex.Data0)
                Assert.Equal( "Expecting '*' in expression!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``not missing in UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.Name(0, 1, "a", [| |]), 0 ); ( Token.Not(2, 5, [| |]), 2 ); ( Token.Name(6, 7, "b", [| |]), 6 ); ( Token.EOF([| |]), 8 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseComparison() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Name(6, 7, "b", [| |]), ex.Data0)
                Assert.Equal( "Missing 'in' in 'not in ' expression!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``lambda UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.Comma(0, 1, [| |]), 0 ); ( Token.EOF([| |]), 2 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseLambda() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Comma(0, 1, [| |]), ex.Data0)
                Assert.Equal( "Expected 'lambda' in lambda expression!", ex.Data1)
        |   _ ->
                Assert.False(false)


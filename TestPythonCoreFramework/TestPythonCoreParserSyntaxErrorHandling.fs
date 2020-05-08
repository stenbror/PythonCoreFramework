
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

    [<Fact>]
    let ``lambda mising colon UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.Lambda(0, 6, [| |]), 0 ); ( Token.Name(7, 8, "a", [| |]), 7 ); ( Token.Name(9, 10, "b", [| |]), 9 ); ( Token.EOF([| |]), 11 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseLambda() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Name(9, 10, "b", [| |]), ex.Data0)
                Assert.Equal( "Expected ':' in lambda expression!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``Test missing else part UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.Name(0, 1, "a", [| |]), 0 ); ( Token.If(2, 4, [| |]), 2 ); ( Token.Name(5, 6, "b", [| |]), 5 ); ( Token.Name(7, 8, "c", [| |]), 7 ); ( Token.EOF([| |]), 9 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseTest() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Name(7, 8, "c", [| |]), ex.Data0)
                Assert.Equal( "Expected 'else' in test expression!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``Tuple missing trailing ')' UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.LeftParen(0, 1, [| |]), 0 ); ( Token.Name(2, 3, "a", [| |]), 2 ); ( Token.Name(4, 5, "b", [| |]), 4 ); ( Token.EOF([| |]), 6 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseTest() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Name(4, 5, "b", [| |]), ex.Data0)
                Assert.Equal( "Missing ')' in expression!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``List missing trailing ']' UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.LeftBracket(0, 1, [| |]), 0 ); ( Token.Name(2, 3, "a", [| |]), 2 ); ( Token.Name(4, 5, "b", [| |]), 4 ); ( Token.EOF([| |]), 6 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseTest() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Name(4, 5, "b", [| |]), ex.Data0)
                Assert.Equal( "Missing ']' in expression!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``List missing trailing '}' UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.LeftCurly(0, 1, [| |]), 0 ); ( Token.Name(2, 3, "a", [| |]), 2 ); ( Token.Name(4, 5, "b", [| |]), 4 ); ( Token.EOF([| |]), 6 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseTest() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Name(4, 5, "b", [| |]), ex.Data0)
                Assert.Equal( "Missing '}' in dictionary!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``Missing trailing ')' in call UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.LeftParen(0, 1, [| |]), 0 ); ( Token.Name(2, 3, "a", [| |]), 2 ); ( Token.Name(4, 5, "b", [| |]), 4 ); ( Token.EOF([| |]), 6 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseTrailer() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Name(4, 5, "b", [| |]), ex.Data0)
                Assert.Equal( "Missing ')' in call expression!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``Missing trailing ']' in index UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.LeftBracket(0, 1, [| |]), 0 ); ( Token.Name(2, 3, "a", [| |]), 2 ); ( Token.Name(4, 5, "b", [| |]), 4 ); ( Token.EOF([| |]), 6 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseTrailer() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Name(4, 5, "b", [| |]), ex.Data0)
                Assert.Equal( "Missing ']' in index expression!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``Missing name after '.' in dot name UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.Dot(0, 1, [| |]), 0 ); ( Token.Dot(1, 2, [| |]), 1 ); ( Token.Name(3, 4, "b", [| |]), 3 ); ( Token.EOF([| |]), 5 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseTrailer() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Dot(1, 2, [| |]), ex.Data0)
                Assert.Equal( "Expecting name literal after '.'", ex.Data1)
        |   _ ->
                Assert.False(false)


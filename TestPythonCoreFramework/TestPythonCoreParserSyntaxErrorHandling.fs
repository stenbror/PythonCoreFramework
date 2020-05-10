
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

    [<Fact>]
    let ``Illegal trailer UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.Comma(0, 1, [| |]), 0 ); ( Token.EOF([| |]), 2 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseTrailer() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Comma(0, 1, [| |]), ex.Data0)
                Assert.Equal( "Expected '(', '[' or '.' in trailer expression!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``subscript missing item with comma UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.Comma(0, 1, [| |]), 0 ); ( Token.EOF([| |]), 2 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseSubscript() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Comma(0, 1, [| |]), ex.Data0)
                Assert.Equal( "Missing subscript item!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``subscript missing item with ']' UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.RightBracket(0, 1, [| |]), 0 ); ( Token.EOF([| |]), 2 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseSubscript() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.RightBracket(0, 1, [| |]), ex.Data0)
                Assert.Equal( "Missing subscript item!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``dictionary entry missing item with ':' UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.Name(0, 1, "a", [| |]), 0 ); ( Token.Name(2, 3, "b", [| |]), 2 ); ( Token.EOF([| |]), 4 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseSubscript() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Name(2, 3, "b", [| |]), ex.Data0)
                Assert.Equal( "Missing ':' in dictionary entry!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``argument not allowed UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.Comma(0, 1, [| |]), 0 ); ( Token.EOF([| |]), 2 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseArgument() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Comma(0, 1, [| |]), ex.Data0)
                Assert.Equal( "Missing argument!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``argument * mising name UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.Mul(0, 1, [| |]), 0 ); ( Token.Comma(2, 3, [| |]), 2 ); ( Token.EOF([| |]), 4 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseArgument() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Comma(2, 3, [| |]), ex.Data0)
                Assert.Equal( "Missing argument!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``argument ** mising name UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.Power(0, 2, [| |]), 0 ); ( Token.Comma(2, 3, [| |]), 2 ); ( Token.EOF([| |]), 4 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseArgument() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Comma(2, 3, [| |]), ex.Data0)
                Assert.Equal( "Missing argument!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``comp for missing for UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.Comma(0, 1, [| |]), 0 ); ( Token.EOF([| |]), 2 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseSyncCompFor() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Comma(0, 1, [| |]), ex.Data0)
                Assert.Equal( "Missing 'for' in comprehension expression!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``comp for missing in UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.For(0, 3, [| |]), 0 ); ( Token.Name(4, 5, "a", [| |]), 4 ); ( Token.Name(7, 8, "b", [| |]), 7 ); ( Token.EOF([| |]), 9 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseSyncCompFor() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Name(7, 8, "b", [| |]), ex.Data0)
                Assert.Equal( "Missing 'in' in for comprehension expression!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``comp if missing if UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.Comma(0, 1, [| |]), 0 ); ( Token.EOF([| |]), 2 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseCompIf() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Comma(0, 1, [| |]), ex.Data0)
                Assert.Equal( "Missing 'if' in comprehension expression!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``yield expr missing yield UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.Comma(0, 1, [| |]), 0 ); ( Token.EOF([| |]), 2 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseYieldExpr() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Comma(0, 1, [| |]), ex.Data0)
                Assert.Equal( "Missing 'yield' in yield expression!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``function suite missing indent UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.Newline(0, 2, [| |]), 0 ); ( Token.Pass(3, 7, [| |]), 3 ); ( Token.EOF([| |]), 8 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseFuncBodySuite() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Pass(3, 7, [| |]), ex.Data0)
                Assert.Equal( "Expecting indentation in function block statement!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``function suite missing newline after typecomment UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.Newline(0, 2, [| |]), 0 ); ( Token.Indent([| |]), 2 ); ( Token.TypeComment(3, 13, "#type: int"), 3 ); ( Token.Pass(14, 18, [| |]), 14 ); ( Token.EOF([| |]), 19 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseFuncBodySuite() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Pass(14, 18, [| |]), ex.Data0)
                Assert.Equal( "Expecting newline after type comment!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``functype missing ')' UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.LeftParen(0, 1, [| |]), 1 ); ( Token.Name(2, 3, "a", [| |]), 2 ); ( Token.Name(4, 5, "b", [| |]), 4 ); ( Token.EOF([| |]), 6 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseFuncType() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Name(4, 5, "b", [| |]), ex.Data0)
                Assert.Equal( "Expecting ')' in func definition!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``functype missing '->' UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.LeftParen(0, 1, [| |]), 1 ); ( Token.Name(2, 3, "a", [| |]), 2 ); ( Token.RightParen(4, 5, [| |]), 4 ); ( Token.Name(6, 7, "b", [| |]), 6 ); ( Token.EOF([| |]), 8 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseFuncType() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Name(6, 7, "b", [| |]), ex.Data0)
                Assert.Equal( "Expecting '->' in func definition!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``functype missing '(' UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.Name(0, 1, "a", [| |]), 0 ); ( Token.RightParen(2, 3, [| |]), 2 ); ( Token.Name(4, 5, "b", [| |]), 4 ); ( Token.EOF([| |]), 6 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseFuncType() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Name(0, 1, "a", [| |]), ex.Data0)
                Assert.Equal( "Expecting '(' in func definition!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``if statement missing 'if' UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.Name(0, 1, "a", [| |]), 0 ); ( Token.Colon(2, 3, [| |]), 2 ); ( Token.Pass(4, 8, [| |]), 4 ); ( Token.Newline(9, 11, [| |]), 9 ); ( Token.EOF([| |]), 12 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseIfStmt() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Name(0, 1, "a", [| |]), ex.Data0)
                Assert.Equal( "Expecting 'if' statement!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``if statement missing ':' UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.If(0, 2, [| |]), 0 ); ( Token.Name(3, 4, "a", [| |]), 3 ); ( Token.Pass(5, 9, [| |]), 5 ); ( Token.Newline(9, 11, [| |]), 9 ); ( Token.EOF([| |]), 12 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseIfStmt() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Pass(5, 9, [| |]), ex.Data0)
                Assert.Equal( "Expecting ':' in if statement!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``elif statement missing ':' UnitTest`` () =
        try
            let lex = new MockTokenizer( [  ( Token.If(0, 2, [| |]), 0 ); ( Token.Name(3, 4, "a", [| |]), 3 ); ( Token.Colon(5, 6, [| |]), 5 ); ( Token.Pass(7, 11, [| |]), 7 ); ( Token.Newline(11, 13, [| |]), 11 );
                                            ( Token.Elif(14, 18, [| |]), 14 ); ( Token.Name(19, 20, "b", [| |]), 19 ); ( Token.Pass(21, 25, [| |]), 21 ); ( Token.Newline(26, 28, [| |]), 26 );
                                            ( Token.EOF([| |]), 26 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseIfStmt() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Pass(21, 25, [| |]), ex.Data0)
                Assert.Equal( "Expecting ':' in elif statement!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``else statement after if statement missing ':' UnitTest`` () =
        try
            let lex = new MockTokenizer( [  ( Token.If(0, 2, [| |]), 0 ); ( Token.Name(3, 4, "a", [| |]), 3 ); ( Token.Colon(5, 6, [| |]), 5 ); ( Token.Pass(7, 11, [| |]), 7 ); ( Token.Newline(11, 13, [| |]), 11 );
                                            ( Token.Else(14, 18, [| |]), 14 ); ( Token.Pass(20, 24, [| |]), 20 ); ( Token.Newline(25, 27, [| |]), 25 );
                                            ( Token.EOF([| |]), 28 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseIfStmt() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Pass(20, 24, [| |]), ex.Data0)
                Assert.Equal( "Expecting ':' in else statement!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``while statement missing 'while' UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.Name(0, 1, "a", [| |]), 0 ); ( Token.Colon(2, 3, [| |]), 2 ); ( Token.Pass(4, 8, [| |]), 4 ); ( Token.Newline(9, 11, [| |]), 9 ); ( Token.EOF([| |]), 12 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseWhileStmt() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Name(0, 1, "a", [| |]), ex.Data0)
                Assert.Equal( "Expecting 'while' statement!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``while statement missing ':' UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.While(0, 5, [| |]), 0 ); ( Token.Name(6, 7, "a", [| |]), 6 ); ( Token.Pass(8, 12, [| |]), 8 ); ( Token.Newline(13, 15, [| |]), 13 ); ( Token.EOF([| |]), 16 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseWhileStmt() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Pass(8, 12, [| |]), ex.Data0)
                Assert.Equal( "Expecting ':' in while statement!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``else statement after while statement missing ':' UnitTest`` () =
        try
            let lex = new MockTokenizer( [  ( Token.While(0, 5, [| |]), 0 ); ( Token.Name(6, 7, "a", [| |]), 6 ); ( Token.Colon(8, 9, [| |]), 8 ); ( Token.Pass(10, 14, [| |]), 10 ); ( Token.Newline(14, 16, [| |]), 12 );
                                            ( Token.Else(16, 20, [| |]), 16 ); ( Token.Pass(22, 26, [| |]), 22 ); ( Token.Newline(27, 29, [| |]), 27 );
                                            ( Token.EOF([| |]), 30 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseWhileStmt() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Pass(22, 26, [| |]), ex.Data0)
                Assert.Equal( "Expecting ':' in else statement!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``for statement missing 'for' UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.Name(0, 1, "a", [| |]), 0 ); ( Token.Colon(2, 3, [| |]), 2 ); ( Token.Pass(4, 8, [| |]), 4 ); ( Token.Newline(9, 11, [| |]), 9 ); ( Token.EOF([| |]), 12 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseForStmt() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Name(0, 1, "a", [| |]), ex.Data0)
                Assert.Equal( "Expecting 'for' statement!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``for statement missing 'in' UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.For(0, 3, [| |]), 0 ); ( Token.Name(6, 7, "a", [| |]), 6 ); ( Token.Pass(8, 12, [| |]), 8 ); ( Token.Newline(13, 15, [| |]), 13 ); ( Token.EOF([| |]), 16 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseForStmt() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Pass(8, 12, [| |]), ex.Data0)
                Assert.Equal( "Expecting 'in' in for statement!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``for statement missing ':' UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.For(0, 3, [| |]), 0 ); ( Token.Name(6, 7, "a", [| |]), 6 ); ( Token.In(8, 10, [| |]), 8 ); ( Token.Name(11, 12, "b", [| |]), 11 ); ( Token.Pass(14, 18, [| |]), 14 ); ( Token.Newline(19, 21, [| |]), 19 ); ( Token.EOF([| |]), 22 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseForStmt() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Pass(14, 18, [| |]), ex.Data0)
                Assert.Equal( "Expecting ':' in for statement!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``else statement after for statement missing ':' UnitTest`` () =
        try
            let lex = new MockTokenizer( [  ( Token.For(0, 3, [| |]), 0 ); ( Token.Name(4, 5, "a", [| |]), 4 ); ( Token.In(6, 8, [| |]), 6 ); ( Token.Name(10, 11, "b", [| |]), 10 ); ( Token.Colon(12, 13, [| |]), 12 ); ( Token.Pass(14, 18, [| |]), 14 ); ( Token.Newline(18, 20, [| |]), 18 );
                                            ( Token.Else(21, 25, [| |]), 21 ); ( Token.Pass(26, 30, [| |]), 26 ); ( Token.Newline(31, 33, [| |]), 31 );
                                            ( Token.EOF([| |]), 34 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseForStmt() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Pass(26, 30, [| |]), ex.Data0)
                Assert.Equal( "Expecting ':' in else statement!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``try statement missing 'try' UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.Name(0, 1, "a", [| |]), 0 ); ( Token.Colon(2, 3, [| |]), 2 ); ( Token.Pass(4, 8, [| |]), 4 ); ( Token.Newline(9, 11, [| |]), 9 ); ( Token.EOF([| |]), 12 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseTryStmt() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Name(0, 1, "a", [| |]), ex.Data0)
                Assert.Equal( "Expecting 'try' in try statement!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``try statement missing ':' UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.Try(0, 3, [| |]), 0 ); ( Token.Pass(4, 8, [| |]), 4 ); ( Token.Newline(9, 11, [| |]), 9 ); ( Token.EOF([| |]), 12 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseTryStmt() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Pass(4, 8, [| |]), ex.Data0)
                Assert.Equal( "Expecting ':' in try statement!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``try statement missing ':' in finally UnitTest`` () =
        try
            let lex = new MockTokenizer( [  ( Token.Try(0, 4, [| |]), 0 ); ( Token.Colon(7, 8, [| |]), 7 ); ( Token.Pass(9, 13, [| |]), 9 ); ( Token.Newline(14, 16, [| |]), 14 );
                                            ( Token.Finally(17, 24, [| |]), 17 ); ( Token.Pass(25, 29, [| |]), 25 ); ( Token.Newline(30, 32, [| |]), 30 );
                                            ( Token.EOF([| |]), 33 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseTryStmt() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Pass(25, 29, [| |]), ex.Data0)
                Assert.Equal( "Expecting ':' in finally statement!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``try statement missing ':' in finally but with except item UnitTest`` () =
        try
            let lex = new MockTokenizer( [  ( Token.Try(0, 4, [| |]), 0 ); ( Token.Colon(7, 8, [| |]), 7 ); ( Token.Pass(9, 13, [| |]), 9 ); ( Token.Newline(14, 16, [| |]), 14 );
                                            ( Token.Except(17, 23, [| |]), 17 );  ( Token.Pass(26, 30, [| |]), 26 ); ( Token.Newline(31, 33, [| |]), 31 );
                                            ( Token.Finally(32, 39, [| |]), 32 ); ( Token.Colon(40, 41, [| |]), 40); ( Token.Pass(42, 46, [| |]), 42 ); ( Token.Newline(47, 49, [| |]), 47 );
                                            ( Token.EOF([| |]), 50 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseTryStmt() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Pass(26, 30, [| |]), ex.Data0)
                Assert.Equal( "Illegal literal!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``except missing ':' UnitTest`` () =
        try
            let lex = new MockTokenizer( [  ( Token.Try(0, 4, [| |]), 0 ); ( Token.Colon(7, 8, [| |]), 7 ); ( Token.Pass(9, 13, [| |]), 9 ); ( Token.Newline(14, 16, [| |]), 14 );
                                            ( Token.Except(17, 23, [| |]), 17 ); ( Token.Colon(24, 25, [| |]), 24 ); ( Token.Pass(26, 30, [| |]), 26 ); ( Token.Newline(31, 33, [| |]), 31 );
                                            ( Token.EOF([| |]), 50 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseTryStmt() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Pass(26, 30, [| |]), ex.Data0)
                Assert.Equal( "", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``except missing ':' but with test argument  UnitTest`` () =
        try
            let lex = new MockTokenizer( [  ( Token.Try(0, 4, [| |]), 0 ); ( Token.Colon(7, 8, [| |]), 7 ); ( Token.Pass(9, 13, [| |]), 9 ); ( Token.Newline(14, 16, [| |]), 14 );
                                            ( Token.Except(17, 23, [| |]), 17 ); ( Token.Name(24, 25, "c", [| |]), 24 ); ( Token.Pass(26, 30, [| |]), 26 ); ( Token.Newline(31, 33, [| |]), 31 );
                                            ( Token.EOF([| |]), 50 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseTryStmt() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Pass(26, 30, [| |]), ex.Data0)
                Assert.Equal( "Expecting ':' in except statement!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``except missing ':' but with test argument and 'as' without expr UnitTest`` () =
        try
            let lex = new MockTokenizer( [  ( Token.Try(0, 4, [| |]), 0 ); ( Token.Colon(7, 8, [| |]), 7 ); ( Token.Pass(9, 13, [| |]), 9 ); ( Token.Newline(14, 16, [| |]), 14 );
                                            ( Token.Except(17, 23, [| |]), 17 ); ( Token.Name(24, 25, "c", [| |]), 24 ); ( Token.As(26, 27, [| |]), 26 ); ( Token.Colon(27, 28, [| |]), 27 ); ( Token.Pass(30, 34, [| |]), 30 ); ( Token.Newline(35, 37, [| |]), 35 );
                                            ( Token.EOF([| |]), 38 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseTryStmt() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Colon(27, 28, [| |]), ex.Data0)
                Assert.Equal( "Missing name literal afer 'as' in except statement!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``else missing ':' in try / execept / else statement UnitTest`` () =
        try
            let lex = new MockTokenizer( [  ( Token.Try(0, 4, [| |]), 0 ); ( Token.Colon(7, 8, [| |]), 7 ); ( Token.Pass(9, 13, [| |]), 9 ); ( Token.Newline(14, 16, [| |]), 14 );
                                            ( Token.Except(17, 23, [| |]), 17 ); ( Token.Name(24, 25, "c", [| |]), 24 ); ( Token.As(26, 27, [| |]), 26 ); ( Token.Name(28, 29, "b", [| |]), 28 ); ( Token.Colon(30, 31, [| |]), 30 ); ( Token.Pass(32, 36, [| |]), 32 ); ( Token.Newline(37, 39, [| |]), 37 );
                                            ( Token.Else(38, 42, [| |]), 38 ); ( Token.Pass(43, 47, [| |]), 43 ); ( Token.Newline(48, 50, [| |]), 48);
                                            ( Token.EOF([| |]), 51 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseTryStmt() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Pass(43, 47, [| |]), ex.Data0)
                Assert.Equal( "Expecting ':' in else statement!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``with statement missing 'with' UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.Name(0, 1, "a", [| |]), 0 ); ( Token.Colon(2, 3, [| |]), 2 ); ( Token.Pass(4, 8, [| |]), 4 ); ( Token.Newline(9, 11, [| |]), 9 ); ( Token.EOF([| |]), 12 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseWithStmt() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Name(0, 1, "a", [| |]), ex.Data0)
                Assert.Equal( "Expecting 'with' in with statement!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``with statement missing ':' UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.With(0, 4, [| |]), 0 ); ( Token.Name(5, 6, "a", [| |]), 5 ); ( Token.Pass(9, 13, [| |]), 9 ); ( Token.Newline(14, 16, [| |]), 14 ); ( Token.EOF([| |]), 17 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseWithStmt() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Pass(9, 13, [| |]), ex.Data0)
                Assert.Equal( "Expecting ':' in with statement!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``with statement with with_item with 'as' missing next argument UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.With(0, 4, [| |]), 0 ); ( Token.Name(5, 6, "a", [| |]), 5 ); ( Token.As(7, 9, [| |]), 7); ( Token.Colon(10, 11, [| |]), 10 ); ( Token.Pass(12, 16, [| |]), 12 ); ( Token.Newline(17, 19, [| |]), 17 ); ( Token.EOF([| |]), 20 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseWithStmt() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Colon(10, 11, [| |]), ex.Data0)
                Assert.Equal( "Illegal literal!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``Suite missing indent token UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.Newline(0, 2, [| |]), 0 ); ( Token.Pass(3, 7, [| |]), 3 ); ( Token.Newline(8, 10, [| |]), 8 ); ( Token.EOF([| |]), 11 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseSuite() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.Pass(3, 7, [| |]), ex.Data0)
                Assert.Equal( "Expecting indentation in statement block!", ex.Data1)
        |   _ ->
                Assert.False(false)

    [<Fact>]
    let ``Suite missing dedent token UnitTest`` () =
        try
            let lex = new MockTokenizer( [ ( Token.Newline(0, 2, [| |]), 0 ); ( Token.Indent([| |]), 3 ); ( Token.Pass(3, 7, [| |]), 3 ); ( Token.Newline(8, 10, [| |]), 8 ); ( Token.EOF([| |]), 11 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            parser.ParseSuite() |> ignore
        with
        |   :? SyntaxError as ex ->
                Assert.Equal( Token.EOF([| |]), ex.Data0)
                Assert.Equal( "Expecting dedentation in statement block!", ex.Data1)
        |   _ ->
                Assert.False(false)

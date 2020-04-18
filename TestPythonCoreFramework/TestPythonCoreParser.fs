
namespace PythonCoreFramework.UnitTests

open Xunit
open PythonCoreFramework

module TestsPythonCoreParser =

    // MOCK of Tokenizer for pure parser testing //////////////////////////////////////////////////////

    type MockTokenizer( nodes : ( Token * int ) list ) =

        let mutable nodeList = nodes

        member this.Next() =
            match nodeList with
            |   head :: tail ->
                    let a, b = head
                    (this :> ITokenizer).Symbol <- a 
                    (this :> ITokenizer).Position <- b
                    nodeList <- tail
            |   [] ->
                    (this :> ITokenizer).Symbol <- Token.EOF( [| |] )
    
        interface ITokenizer with

            member val Symbol : Token = Token.Empty with get, set

            member val Position : int = 0 with get, set

            member this.Advance() =
                    this.Next()


    // UnitTests for Parser with Mock Tokenizer /////////////////////////////////////////////////////// 


    [<Fact>]
    let ``Name literal test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test", [| |]), 0 ); ( Token.EOF([| |]), 5 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Name(0, 5, Token.Name(0, 5, "Test", [| |])), parser.ParseAtom())

    [<Fact>]
    let ``Number literal test`` () =
        let lex = new MockTokenizer( [ ( Token.Number(0, 5, "1234", [| |]), 0 ); ( Token.EOF([| |]), 5 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Number(0, 5, Token.Number(0, 5, "1234", [| |])), parser.ParseAtom())

    [<Fact>]
    let ``String literal test`` () =
        let lex = new MockTokenizer( [ ( Token.String(0, 6, "'who'", [| |]), 0 ); ( Token.EOF([| |]), 6 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.String(0, 6, [| Token.String(0, 6, "'who'", [| |]) |]), parser.ParseAtom())

    [<Fact>]
    let ``Multiple String literal test`` () =
        let lex = new MockTokenizer( [ ( Token.String(0, 6, "'who'", [| |]), 0 ); ( Token.String(7, 12, "'are'", [| |]), 0 ); ( Token.EOF([| |]), 12 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.String(0, 12, [| Token.String(0, 6, "'who'", [| |]); Token.String(7, 12, "'are'", [| |]);  |]), parser.ParseAtom())

    [<Fact>]
    let ``Elipsis literal test`` () =
        let lex = new MockTokenizer( [ ( Token.Elipsis(0, 4, [| |]), 0 ); ( Token.EOF([| |]), 4 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Elipsis(0, 4, Token.Elipsis(0, 4, [| |])), parser.ParseAtom())

    [<Fact>]
    let ``None literal test`` () =
        let lex = new MockTokenizer( [ ( Token.None(0, 4, [| |]), 0 ); ( Token.EOF([| |]), 4 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.None(0, 4, Token.None(0, 4, [| |])), parser.ParseAtom())

    [<Fact>]
    let ``True literal test`` () =
        let lex = new MockTokenizer( [ ( Token.True(0, 4, [| |]), 0 ); ( Token.EOF([| |]), 4 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.True(0, 4, Token.True(0, 4, [| |])), parser.ParseAtom())

    [<Fact>]
    let ``False literal test`` () =
        let lex = new MockTokenizer( [ ( Token.False(0, 5, [| |]), 0 ); ( Token.EOF([| |]), 5 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.False(0, 5, Token.False(0, 5, [| |])), parser.ParseAtom())

    [<Fact>]
    let ``() literal test`` () =
        let lex = new MockTokenizer( [ ( Token.LeftParen(0, 1, [| |]), 0 ); ( Token.RightParen(1, 2, [| |]), 1 ); ( Token.EOF([| |]), 2 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Tuple(0, 2, Token.LeftParen(0, 1, [| |]), ASTNode.Empty, Token.RightParen(1, 2, [| |]) ), parser.ParseAtom())

    [<Fact>]
    let ``[] literal test`` () =
        let lex = new MockTokenizer( [ ( Token.LeftBracket(0, 1, [| |]), 0 ); ( Token.RightBracket(1, 2, [| |]), 1 ); ( Token.EOF([| |]), 2 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.List(0, 2, Token.LeftBracket(0, 1, [| |]), ASTNode.Empty, Token.RightBracket(1, 2, [| |]) ), parser.ParseAtom())

    [<Fact>]
    let ``{} literal test`` () =
        let lex = new MockTokenizer( [ ( Token.LeftCurly(0, 1, [| |]), 0 ); ( Token.RightCurly(1, 2, [| |]), 1 ); ( Token.EOF([| |]), 2 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Dictionary(0, 2, Token.LeftCurly(0, 1, [| |]), [| |], [| |], Token.RightCurly(1, 2, [| |]) ), parser.ParseAtom())

    [<Fact>]
    let ``async name test`` () =
        let lex = new MockTokenizer( [ ( Token.Async(0, 5, [| |]), 0 ); ( Token.Name(7, 11, "Test", [| |]), 7 ); ( Token.EOF([| |]), 11 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.AtomExpr(0, 11, Token.Async(0, 5, [| |]), ASTNode.Name(7, 11, Token.Name(7, 11, "Test", [| |])), [| |]), parser.ParseAtomExpr())

    [<Fact>]
    let ``Name2 literal test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test", [| |]), 0 ); ( Token.EOF([| |]), 5 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Name(0, 5, Token.Name(0, 5, "Test", [| |])), parser.ParseAtomExpr())

    [<Fact>]
    let ``async name with dot name test`` () =
        let lex = new MockTokenizer( [ ( Token.Async(0, 5, [| |]), 0 ); ( Token.Name(7, 11, "Test", [| |]), 7 ); ( Token.Dot(11, 12, [| |]), 11 ); ( Token.Name(12, 16, "Next", [| |]), 12 );    ( Token.EOF([| |]), 16 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.AtomExpr(0, 16, Token.Async(0, 5, [| |]), ASTNode.Name(7, 11, Token.Name(7, 11, "Test", [| |])), [|    ASTNode.DotName(11, 16, Token.Dot(11, 12, [| |]), ASTNode.Name(12, 16, Token.Name(12, 16, "Next", [| |] ) ) )       |]), parser.ParseAtomExpr())

    [<Fact>]
    let ``async name with () test`` () =
        let lex = new MockTokenizer( [ ( Token.Async(0, 5, [| |]), 0 ); ( Token.Name(7, 11, "Test", [| |]), 7 ); ( Token.LeftParen(11, 12, [| |]), 11 ); ( Token.RightParen(12, 13, [| |]), 13 );  ( Token.EOF([| |]), 13 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.AtomExpr(0, 13, Token.Async(0, 5, [| |]), ASTNode.Name(7, 11, Token.Name(7, 11, "Test", [| |])), [|    ASTNode.Call(11, 13,  Token.LeftParen(11, 12, [| |]), ASTNode.Empty, Token.RightParen(12, 13, [| |]) );      |]), parser.ParseAtomExpr())

    [<Fact>]
    let ``async name with [] test`` () =
        let lex = new MockTokenizer( [ ( Token.Async(0, 5, [| |]), 0 ); ( Token.Name(7, 11, "Test", [| |]), 7 ); ( Token.LeftBracket(11, 12, [| |]), 11 ); ( Token.RightBracket(12, 13, [| |]), 13 );  ( Token.EOF([| |]), 13 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.AtomExpr(0, 13, Token.Async(0, 5, [| |]), ASTNode.Name(7, 11, Token.Name(7, 11, "Test", [| |])), [|    ASTNode.Index(11, 13,  Token.LeftBracket(11, 12, [| |]), ASTNode.Empty, Token.RightBracket(12, 13, [| |]) );      |]), parser.ParseAtomExpr())

    [<Fact>]
    let ``** operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.Power(5, 7, [| |]), 7 ); ( Token.Name(8, 12, "Fest", [| |]), 8 );  ( Token.EOF([| |]), 12 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Power(0, 12, ASTNode.Name(0, 7, Token.Name(0, 4, "Test", [| |])), Token.Power(5, 7, [| |]), ASTNode.Name(8, 12, Token.Name(8, 12, "Fest", [| |])) ) , parser.ParsePower())

    [<Fact>]
    let ``unary + operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Plus(0, 1, [| |]), 0 ); ( Token.Name(2, 6, "Test", [| |]), 2 );  ( Token.EOF([| |]), 6 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.UnaryPlus(0, 6, Token.Plus(0, 1, [| |]),   ASTNode.Name(2, 6, Token.Name(2, 6, "Test", [| |]))  ) , parser.ParseFactor())

    [<Fact>]
    let ``unary - operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Minus(0, 1, [| |]), 0 ); ( Token.Name(2, 6, "Test", [| |]), 2 );  ( Token.EOF([| |]), 6 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.UnaryMinus(0, 6, Token.Minus(0, 1, [| |]),   ASTNode.Name(2, 6, Token.Name(2, 6, "Test", [| |]))  ) , parser.ParseFactor())

    [<Fact>]
    let ``unary ~ operator test`` () =
        let lex = new MockTokenizer( [ ( Token.BitInvert(0, 1, [| |]), 0 ); ( Token.Name(2, 6, "Test", [| |]), 2 );  ( Token.EOF([| |]), 6 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.UnaryInvert(0, 6, Token.BitInvert(0, 1, [| |]),   ASTNode.Name(2, 6, Token.Name(2, 6, "Test", [| |]))  ) , parser.ParseFactor())

    [<Fact>]
    let ``* operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.Mul(5, 6, [| |]), 6 ); ( Token.Name(8, 12, "Fest", [| |]), 8 );  ( Token.EOF([| |]), 12 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Mul(0, 12, ASTNode.Name(0, 6, Token.Name(0, 4, "Test", [| |])), Token.Mul(5, 6, [| |]), ASTNode.Name(8, 12, Token.Name(8, 12, "Fest", [| |])) ) , parser.ParseTerm())

    [<Fact>]
    let ``recursive * operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.Mul(5, 6, [| |]), 6 ); ( Token.Name(8, 12, "Fest", [| |]), 8 ); ( Token.Div(14, 15, [| |]), 14 ); ( Token.Name(17, 21, "Fest", [| |]), 17 );  ( Token.EOF([| |]), 22 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Div(0, 22, ASTNode.Mul(0, 14, ASTNode.Name(0, 6, Token.Name(0, 4, "Test", [| |])), Token.Mul(5, 6, [| |]), ASTNode.Name(8, 14, Token.Name(8, 12, "Fest", [| |])) ), Token.Div(14, 15, [| |]), ASTNode.Name(17, 22, Token.Name(17, 21, "Fest", [| |])) ) , parser.ParseTerm())

    [<Fact>]
    let ``/ operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.Div(5, 6, [| |]), 6 ); ( Token.Name(8, 12, "Fest", [| |]), 8 );  ( Token.EOF([| |]), 12 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Div(0, 12, ASTNode.Name(0, 6, Token.Name(0, 4, "Test", [| |])), Token.Div(5, 6, [| |]), ASTNode.Name(8, 12, Token.Name(8, 12, "Fest", [| |])) ) , parser.ParseTerm())

    [<Fact>]
    let ``// operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.FloorDiv(5, 7, [| |]), 7 ); ( Token.Name(8, 12, "Fest", [| |]), 8 );  ( Token.EOF([| |]), 12 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.FloorDiv(0, 12, ASTNode.Name(0, 7, Token.Name(0, 4, "Test", [| |])), Token.FloorDiv(5, 7, [| |]), ASTNode.Name(8, 12, Token.Name(8, 12, "Fest", [| |])) ) , parser.ParseTerm())

    [<Fact>]
    let ``% operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.Modulo(5, 6, [| |]), 6 ); ( Token.Name(8, 12, "Fest", [| |]), 8 );  ( Token.EOF([| |]), 12 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Modulo(0, 12, ASTNode.Name(0, 6, Token.Name(0, 4, "Test", [| |])), Token.Modulo(5, 6, [| |]), ASTNode.Name(8, 12, Token.Name(8, 12, "Fest", [| |])) ) , parser.ParseTerm())

    [<Fact>]
    let ``@ operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.Matrice(5, 6, [| |]), 6 ); ( Token.Name(8, 12, "Fest", [| |]), 8 );  ( Token.EOF([| |]), 12 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Matrice(0, 12, ASTNode.Name(0, 6, Token.Name(0, 4, "Test", [| |])), Token.Matrice(5, 6, [| |]), ASTNode.Name(8, 12, Token.Name(8, 12, "Fest", [| |])) ) , parser.ParseTerm())

    [<Fact>]
    let ``+ operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.Plus(5, 6, [| |]), 6 ); ( Token.Name(8, 12, "Fest", [| |]), 8 );  ( Token.EOF([| |]), 12 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Plus(0, 12, ASTNode.Name(0, 6, Token.Name(0, 4, "Test", [| |])), Token.Plus(5, 6, [| |]), ASTNode.Name(8, 12, Token.Name(8, 12, "Fest", [| |])) ) , parser.ParseArithExpr())

    [<Fact>]
    let ``recursive + / - operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.Plus(5, 6, [| |]), 6 ); ( Token.Name(8, 12, "Fest", [| |]), 8 );  ( Token.Minus(14, 15, [| |]), 14 ); ( Token.Name(16, 20, "Lest", [| |]), 16 ); ( Token.EOF([| |]), 21 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Minus(0, 21, ASTNode.Plus(0, 14, ASTNode.Name(0, 6, Token.Name(0, 4, "Test", [| |])), Token.Plus(5, 6, [| |]), ASTNode.Name(8, 14, Token.Name(8, 12, "Fest", [| |])) )  , Token.Minus(14, 15, [| |]), ASTNode.Name(16, 21, Token.Name(16, 20, "Lest", [| |])) ), parser.ParseArithExpr())

    [<Fact>]
    let ``- operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.Minus(5, 6, [| |]), 6 ); ( Token.Name(8, 12, "Fest", [| |]), 8 );  ( Token.EOF([| |]), 12 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Minus(0, 12, ASTNode.Name(0, 6, Token.Name(0, 4, "Test", [| |])), Token.Minus(5, 6, [| |]), ASTNode.Name(8, 12, Token.Name(8, 12, "Fest", [| |])) ) , parser.ParseArithExpr())

    [<Fact>]
    let ``<< operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.ShiftLeft(5, 7, [| |]), 7 ); ( Token.Name(8, 12, "Fest", [| |]), 8 );  ( Token.EOF([| |]), 12 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.ShiftLeft(0, 12, ASTNode.Name(0, 7, Token.Name(0, 4, "Test", [| |])), Token.ShiftLeft(5, 7, [| |]), ASTNode.Name(8, 12, Token.Name(8, 12, "Fest", [| |])) ) , parser.ParseShiftExpr())

    [<Fact>]
    let ``>> operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.ShiftRight(5, 7, [| |]), 7 ); ( Token.Name(8, 12, "Fest", [| |]), 8 );  ( Token.EOF([| |]), 12 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.ShiftRight(0, 12, ASTNode.Name(0, 7, Token.Name(0, 4, "Test", [| |])), Token.ShiftRight(5, 7, [| |]), ASTNode.Name(8, 12, Token.Name(8, 12, "Fest", [| |])) ) , parser.ParseShiftExpr())

    [<Fact>]
    let ``recursive >> operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.ShiftRight(5, 7, [| |]), 7 ); ( Token.Name(8, 12, "Fest", [| |]), 8 ); ( Token.ShiftRight(14, 16, [| |]), 14 ); ( Token.Name(18, 22, "Fest", [| |]), 18 );  ( Token.EOF([| |]), 23 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.ShiftRight(0, 23, ASTNode.ShiftRight(0, 14, ASTNode.Name(0, 7, Token.Name(0, 4, "Test", [| |])), Token.ShiftRight(5, 7, [| |]), ASTNode.Name(8, 14, Token.Name(8, 12, "Fest", [| |])) ), Token.ShiftRight(14, 16, [| |]), ASTNode.Name(18, 23, Token.Name(18, 22, "Fest", [| |])) ) , parser.ParseShiftExpr())

    [<Fact>]
    let ``& operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.BitAnd(5, 6, [| |]), 6 ); ( Token.Name(8, 12, "Fest", [| |]), 8 );  ( Token.EOF([| |]), 12 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.AndExpr(0, 12, ASTNode.Name(0, 6, Token.Name(0, 4, "Test", [| |])), Token.BitAnd(5, 6, [| |]), ASTNode.Name(8, 12, Token.Name(8, 12, "Fest", [| |])) ) , parser.ParseAndExpr())

    [<Fact>]
    let ``recursive & operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.BitAnd(5, 6, [| |]), 6 ); ( Token.Name(8, 12, "Fest", [| |]), 8 ); ( Token.BitAnd(14, 15, [| |]), 14 ); ( Token.Name(17, 21, "Fest", [| |]), 17 ); ( Token.EOF([| |]), 22 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.AndExpr(0, 22, ASTNode.AndExpr(0, 14, ASTNode.Name(0, 6, Token.Name(0, 4, "Test", [| |])), Token.BitAnd(5, 6, [| |]), ASTNode.Name(8, 14, Token.Name(8, 12, "Fest", [| |])) ), Token.BitAnd(14, 15, [| |]), ASTNode.Name(17, 22, Token.Name(17, 21, "Fest", [| |])) ) , parser.ParseExpr())

    [<Fact>]
    let ``^ operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.BitXor(5, 6, [| |]), 6 ); ( Token.Name(8, 12, "Fest", [| |]), 8 );  ( Token.EOF([| |]), 12 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.XorExpr(0, 12, ASTNode.Name(0, 6, Token.Name(0, 4, "Test", [| |])), Token.BitXor(5, 6, [| |]), ASTNode.Name(8, 12, Token.Name(8, 12, "Fest", [| |])) ) , parser.ParseXorExpr())

    [<Fact>]
    let ``recursive ^ operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.BitXor(5, 6, [| |]), 6 ); ( Token.Name(8, 12, "Fest", [| |]), 8 ); ( Token.BitXor(14, 15, [| |]), 14 ); ( Token.Name(17, 21, "Fest", [| |]), 17 ); ( Token.EOF([| |]), 22 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.XorExpr(0, 22, ASTNode.XorExpr(0, 14, ASTNode.Name(0, 6, Token.Name(0, 4, "Test", [| |])), Token.BitXor(5, 6, [| |]), ASTNode.Name(8, 14, Token.Name(8, 12, "Fest", [| |])) ), Token.BitXor(14, 15, [| |]), ASTNode.Name(17, 22, Token.Name(17, 21, "Fest", [| |])) ) , parser.ParseExpr())

    [<Fact>]
    let ``| operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.BitOr(5, 6, [| |]), 6 ); ( Token.Name(8, 12, "Fest", [| |]), 8 );  ( Token.EOF([| |]), 12 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.OrExpr(0, 12, ASTNode.Name(0, 6, Token.Name(0, 4, "Test", [| |])), Token.BitOr(5, 6, [| |]), ASTNode.Name(8, 12, Token.Name(8, 12, "Fest", [| |])) ) , parser.ParseExpr())

    [<Fact>]
    let ``recursive | operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.BitOr(5, 6, [| |]), 6 ); ( Token.Name(8, 12, "Fest", [| |]), 8 ); ( Token.BitOr(14, 15, [| |]), 14 ); ( Token.Name(17, 21, "Fest", [| |]), 17 ); ( Token.EOF([| |]), 22 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.OrExpr(0, 22, ASTNode.OrExpr(0, 14, ASTNode.Name(0, 6, Token.Name(0, 4, "Test", [| |])), Token.BitOr(5, 6, [| |]), ASTNode.Name(8, 14, Token.Name(8, 12, "Fest", [| |])) ), Token.BitOr(14, 15, [| |]), ASTNode.Name(17, 22, Token.Name(17, 21, "Fest", [| |])) ) , parser.ParseExpr())

    [<Fact>]
    let ``* expr operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Mul(0, 1, [| |]), 0 ); ( Token.Name(2, 6, "Test", [| |]), 2 );  ( Token.EOF([| |]), 6 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.StarExpr(0, 6, Token.Mul(0, 1, [| |]),   ASTNode.Name(2, 6, Token.Name(2, 6, "Test", [| |]))  ) , parser.ParseStarExpr())

    [<Fact>]
    let ``< operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.Less(5, 6, [| |]), 6 ); ( Token.Name(8, 12, "Fest", [| |]), 8 );  ( Token.EOF([| |]), 12 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Less(0, 12, ASTNode.Name(0, 6, Token.Name(0, 4, "Test", [| |])), Token.Less(5, 6, [| |]), ASTNode.Name(8, 12, Token.Name(8, 12, "Fest", [| |])) ) , parser.ParseComparison())

    [<Fact>]
    let ``> operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.Greater(5, 6, [| |]), 6 ); ( Token.Name(8, 12, "Fest", [| |]), 8 );  ( Token.EOF([| |]), 12 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Greater(0, 12, ASTNode.Name(0, 6, Token.Name(0, 4, "Test", [| |])), Token.Greater(5, 6, [| |]), ASTNode.Name(8, 12, Token.Name(8, 12, "Fest", [| |])) ) , parser.ParseComparison())

    [<Fact>]
    let ``recursive > <  operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.Greater(5, 6, [| |]), 6 ); ( Token.Name(8, 12, "Fest", [| |]), 8 ); ( Token.Less(9, 10, [| |]), 9 ); ( Token.Name(11, 15, "Lest", [| |]), 11 ); ( Token.EOF([| |]), 15 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Less(0, 15, ASTNode.Greater(0, 9, ASTNode.Name(0, 6, Token.Name(0, 4, "Test", [| |])), Token.Greater(5, 6, [| |]), ASTNode.Name(8, 9, Token.Name(8, 12, "Fest", [| |])) ) ,Token.Less(9, 10, [| |]), ASTNode.Name(11, 15, Token.Name(11, 15, "Lest", [| |]))    ) , parser.ParseComparison())

    [<Fact>]
    let ``<= operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.LessEqual(5, 7, [| |]), 7 ); ( Token.Name(8, 12, "Fest", [| |]), 8 );  ( Token.EOF([| |]), 12 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.LessEqual(0, 12, ASTNode.Name(0, 7, Token.Name(0, 4, "Test", [| |])), Token.LessEqual(5, 7, [| |]), ASTNode.Name(8, 12, Token.Name(8, 12, "Fest", [| |])) ) , parser.ParseComparison())

    [<Fact>]
    let ``>= operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.GreaterEqual(5, 7, [| |]), 7 ); ( Token.Name(8, 12, "Fest", [| |]), 8 );  ( Token.EOF([| |]), 12 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.GreaterEqual(0, 12, ASTNode.Name(0, 7, Token.Name(0, 4, "Test", [| |])), Token.GreaterEqual(5, 7, [| |]), ASTNode.Name(8, 12, Token.Name(8, 12, "Fest", [| |])) ) , parser.ParseComparison())

    [<Fact>]
    let ``<> or != operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.NotEqual(5, 7, [| |]), 7 ); ( Token.Name(8, 12, "Fest", [| |]), 8 );  ( Token.EOF([| |]), 12 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.NotEqual(0, 12, ASTNode.Name(0, 7, Token.Name(0, 4, "Test", [| |])), Token.NotEqual(5, 7, [| |]), ASTNode.Name(8, 12, Token.Name(8, 12, "Fest", [| |])) ) , parser.ParseComparison())

    [<Fact>]
    let ``== operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.Equal(5, 7, [| |]), 7 ); ( Token.Name(8, 12, "Fest", [| |]), 8 );  ( Token.EOF([| |]), 12 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Equal(0, 12, ASTNode.Name(0, 7, Token.Name(0, 4, "Test", [| |])), Token.Equal(5, 7, [| |]), ASTNode.Name(8, 12, Token.Name(8, 12, "Fest", [| |])) ) , parser.ParseComparison())

    [<Fact>]
    let ``in operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.In(5, 7, [| |]), 7 ); ( Token.Name(8, 12, "Fest", [| |]), 8 );  ( Token.EOF([| |]), 12 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.In(0, 12, ASTNode.Name(0, 7, Token.Name(0, 4, "Test", [| |])), Token.In(5, 7, [| |]), ASTNode.Name(8, 12, Token.Name(8, 12, "Fest", [| |])) ) , parser.ParseComparison())

    [<Fact>]
    let ``is operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.Is(5, 7, [| |]), 7 ); ( Token.Name(8, 12, "Fest", [| |]), 8 );  ( Token.EOF([| |]), 12 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Is(0, 12, ASTNode.Name(0, 7, Token.Name(0, 4, "Test", [| |])), Token.Is(5, 7, [| |]), ASTNode.Name(8, 12, Token.Name(8, 12, "Fest", [| |])) ) , parser.ParseComparison())

    [<Fact>]
    let ``is not operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.Is(5, 7, [| |]), 7 ); ( Token.Not(8, 11, [| |]), 11 ); ( Token.Name(12, 16, "Fest", [| |]), 12 );  ( Token.EOF([| |]), 16 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.IsNot(0, 16, ASTNode.Name(0, 7, Token.Name(0, 4, "Test", [| |])), Token.Is(5, 7, [| |]), Token.Not(8, 11, [| |]), ASTNode.Name(12, 16, Token.Name(12, 16, "Fest", [| |])) ) , parser.ParseComparison())

    [<Fact>]
    let ``not in operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.Not(5, 8, [| |]), 8 ); ( Token.In(9, 11, [| |]), 11 ); ( Token.Name(12, 16, "Fest", [| |]), 12 );  ( Token.EOF([| |]), 16 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.NotIn(0, 16, ASTNode.Name(0, 8, Token.Name(0, 4, "Test", [| |])), Token.Not(5, 8, [| |]), Token.In(9, 11, [| |]), ASTNode.Name(12, 16, Token.Name(12, 16, "Fest", [| |])) ) , parser.ParseComparison())

    [<Fact>]
    let ``not test operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Not(0, 3, [| |]), 0 ); ( Token.Name(4, 8, "Test", [| |]), 4 );  ( Token.EOF([| |]), 8 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.NotTest(0, 8, Token.Not(0, 3, [| |]),   ASTNode.Name(4, 8, Token.Name(4, 8, "Test", [| |]))  ) , parser.ParseNotTest())

    [<Fact>]
    let ``not not test operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Not(0, 3, [| |]), 0 ); ( Token.Not(4, 7, [| |]), 4 ); ( Token.Name(9, 13, "Test", [| |]), 9 );  ( Token.EOF([| |]), 13 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.NotTest(0, 13, Token.Not(0, 3, [| |]), ASTNode.NotTest(4, 13, Token.Not(4, 7, [| |]),   ASTNode.Name(9, 13, Token.Name(9, 13, "Test", [| |]))  ) ) , parser.ParseNotTest())

    [<Fact>]
    let ``and test operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.And(5, 8, [| |]), 8 ); ( Token.Name(9, 13, "Fest", [| |]), 9 );  ( Token.EOF([| |]), 13 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.AndTest(0, 13, ASTNode.Name(0, 8, Token.Name(0, 4, "Test", [| |])), Token.And(5, 8, [| |]), ASTNode.Name(9, 13, Token.Name(9, 13, "Fest", [| |])) ) , parser.ParseAndTest())

    [<Fact>]
    let ``recursive and test operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.And(5, 8, [| |]), 8 ); ( Token.Name(9, 13, "Fest", [| |]), 9 ); ( Token.And(14, 17, [| |]), 14 ); ( Token.Name(18, 22, "Tall", [| |]), 18 );   ( Token.EOF([| |]), 22 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.AndTest(0, 22, ASTNode.AndTest(0, 14, ASTNode.Name(0, 8, Token.Name(0, 4, "Test", [| |])), Token.And(5, 8, [| |]), ASTNode.Name(9, 14, Token.Name(9, 13, "Fest", [| |])) ) , Token.And(14, 17, [| |]), ASTNode.Name(18, 22, Token.Name(18, 22, "Tall", [| |])) ) , parser.ParseAndTest())

    [<Fact>]
    let ``or test operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.Or(5, 7, [| |]), 7 ); ( Token.Name(8, 12, "Fest", [| |]), 8 );  ( Token.EOF([| |]), 12 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.OrTest(0, 12, ASTNode.Name(0, 7, Token.Name(0, 4, "Test", [| |])), Token.Or(5, 7, [| |]), ASTNode.Name(8, 12, Token.Name(8, 12, "Fest", [| |])) ) , parser.ParseOrTest())

    [<Fact>]
    let ``recursive or test operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.Or(5, 7, [| |]), 7 ); ( Token.Name(8, 12, "Fest", [| |]), 8 ); ( Token.Or(13, 15, [| |]), 13 ); ( Token.Name(16, 20, "Tall", [| |]), 16 );  ( Token.EOF([| |]), 20 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.OrTest( 0, 20, ASTNode.OrTest(0, 13, ASTNode.Name(0, 7, Token.Name(0, 4, "Test", [| |])), Token.Or(5, 7, [| |]), ASTNode.Name(8, 13, Token.Name(8, 12, "Fest", [| |])) ) , Token.Or(13, 15, [| |]), ASTNode.Name(16, 20, Token.Name(16, 20, "Tall", [| |])) ) , parser.ParseOrTest())

    [<Fact>]
    let ``lambda nocond expression test`` () =
        let lex = new MockTokenizer( [ ( Token.Lambda(0, 6, [| |]), 0 ); ( Token.Colon(6, 7, [| |]), 6 ); ( Token.Name(8, 12, "Fest", [| |]), 8 );  ( Token.EOF([| |]), 12 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Lambda(0, 12, Token.Lambda(0, 6, [| |]), ASTNode.Empty, Token.Colon(6, 7, [| |]), ASTNode.Name(8, 12, Token.Name(8, 12, "Fest", [| |])))  , parser.ParseTestNoCond())

    [<Fact>]
    let ``lambda expression test`` () =
        let lex = new MockTokenizer( [ ( Token.Lambda(0, 6, [| |]), 0 ); ( Token.Colon(6, 7, [| |]), 6 ); ( Token.Name(8, 12, "Fest", [| |]), 8 );  ( Token.EOF([| |]), 12 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Lambda(0, 12, Token.Lambda(0, 6, [| |]), ASTNode.Empty, Token.Colon(6, 7, [| |]), ASTNode.Name(8, 12, Token.Name(8, 12, "Fest", [| |])))  , parser.ParseTest())

    [<Fact>]
    let ``Single Test rule test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test", [| |]), 0 ); ( Token.EOF([| |]), 5 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Name(0, 5, Token.Name(0, 5, "Test", [| |])), parser.ParseTest())

    [<Fact>]
    let ``Complex Test rule test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test", [| |]), 0 ); ( Token.If(6, 8, [| |]), 6 ); ( Token.Name(9, 12, "Tut", [| |]), 9 ); ( Token.Else(13, 17, [| |]), 13 ); ( Token.Name(14, 18, "Fest", [| |]), 14 ); ( Token.EOF([| |]), 18 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Test(0, 18, ASTNode.Name(0, 6, Token.Name(0, 5, "Test", [| |])), Token.If(6, 8, [| |]), ASTNode.Name(9, 13, Token.Name(9, 12, "Tut", [| |])), Token.Else(13, 17, [| |]), ASTNode.Name(14, 18, Token.Name(14, 18, "Fest", [| |])) ), parser.ParseTest())

    [<Fact>]
    let ``NamedExpr operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 4, "Test", [| |]), 0 ); ( Token.ColonAssign(5, 7, [| |]), 7 ); ( Token.Name(8, 12, "Fest", [| |]), 8 );  ( Token.EOF([| |]), 12 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.NamedExpr(0, 12, ASTNode.Name(0, 7, Token.Name(0, 4, "Test", [| |])), Token.ColonAssign(5, 7, [| |]), ASTNode.Name(8, 12, Token.Name(8, 12, "Fest", [| |])) ) , parser.ParseNamedExpr())

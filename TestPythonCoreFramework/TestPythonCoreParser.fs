
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

    // Expression /////////////////////////////////////////////////////////////////////////////////////////////////////

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
    let ``recursive unary - operator test`` () =
        let lex = new MockTokenizer( [ ( Token.Minus(0, 1, [| |]), 0 ); ( Token.Minus(1, 2, [| |]), 1 );  ( Token.Name(3, 7, "Test", [| |]), 3 );  ( Token.EOF([| |]), 8 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.UnaryMinus(0, 8, Token.Minus(0, 1, [| |]),  ASTNode.UnaryMinus(1, 8, Token.Minus(1, 2, [| |]), ASTNode.Name(3, 8, Token.Name(3, 7, "Test", [| |]))  ) ) , parser.ParseFactor())

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
    let ``Matrice operator test`` () =
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

    [<Fact>]
    let ``Subscript single test`` () =
        let lex = new MockTokenizer( [ ( Token.Number(0, 1, "0", [||]), 0 );  ( Token.EOF([| |]), 2 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Subscript(0, 2, ASTNode.Number(0, 2, Token.Number(0, 1, "0", [| |]) ), Token.Empty, ASTNode.Empty, Token.Empty, ASTNode.Empty) , parser.ParseSubscript())

    [<Fact>]
    let ``Subscript double test`` () =
        let lex = new MockTokenizer( [ ( Token.Number(0, 1, "0", [||]), 0 ); ( Token.Colon(2, 3, [| |]), 2 ); ( Token.Number(3, 4, "8", [||]), 3);  ( Token.EOF([| |]), 5 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Subscript(0, 5, ASTNode.Number(0, 2, Token.Number(0, 1, "0", [| |]) ), Token.Colon(2, 3, [| |]), ASTNode.Number(3, 5, Token.Number(3, 4, "8", [| |])), Token.Empty, ASTNode.Empty) , parser.ParseSubscript())

    [<Fact>]
    let ``Subscript tripple test`` () =
        let lex = new MockTokenizer( [ ( Token.Number(0, 1, "0", [||]), 0 ); ( Token.Colon(2, 3, [| |]), 2 ); ( Token.Number(3, 4, "8", [||]), 3); ( Token.Colon(5, 6, [| |]), 5 ); ( Token.Number(7, 8, "2", [||]), 7);  ( Token.EOF([| |]), 9 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Subscript(0, 9, ASTNode.Number(0, 2, Token.Number(0, 1, "0", [| |]) ), Token.Colon(2, 3, [| |]), ASTNode.Number(3, 5, Token.Number(3, 4, "8", [| |])), Token.Colon(5, 6, [| |]), ASTNode.Number(7, 9, Token.Number(7, 8, "2", [| |]))) , parser.ParseSubscript())

    [<Fact>]
    let ``Subscript single empty test`` () =
        let lex = new MockTokenizer( [ ( Token.Colon(0, 1, [||]), 0 ); ( Token.Comma(2, 3, [| |]), 2 );  ( Token.EOF([| |]), 3 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Subscript(0, 2, ASTNode.Empty, Token.Colon(0, 1, [| |]), ASTNode.Empty, Token.Empty, ASTNode.Empty) , parser.ParseSubscript())

    [<Fact>]
    let ``Subscript double empty test`` () =
        let lex = new MockTokenizer( [ ( Token.Colon(0, 1, [||]), 0 ); ( Token.Colon(2, 3, [| |]), 2 ); ( Token.RightBracket(4, 5, [| |]), 4);  ( Token.EOF([| |]), 6 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Subscript(0, 4, ASTNode.Empty, Token.Colon(0, 1, [| |]), ASTNode.Empty, Token.Colon(2, 3, [| |]), ASTNode.Empty) , parser.ParseSubscript())

    [<Fact>]
    let ``Subscript list double empty test`` () =
        let lex = new MockTokenizer( [ ( Token.Colon(0, 1, [||]), 0 ); ( Token.Colon(2, 3, [| |]), 2 ); ( Token.Comma(4, 5, [| |]), 4); ( Token.Colon(6, 7, [||]), 6 ); ( Token.RightBracket(8, 9, [| |]), 8);  ( Token.EOF([| |]), 10 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SubscriptList(0, 8, [|
                                                        ASTNode.Subscript(0, 4, ASTNode.Empty, Token.Colon(0, 1, [| |]), ASTNode.Empty, Token.Colon(2, 3, [| |]), ASTNode.Empty);
                                                        ASTNode.Subscript(6, 8, ASTNode.Empty, Token.Colon(6, 7, [| |]), ASTNode.Empty, Token.Empty, ASTNode.Empty);
                                                        |], [| Token.Comma(4, 5, [| |]) |]) , parser.ParseSubscriptList())

    [<Fact>]
    let ``Subscript list double empty with trailing comma test`` () =
        let lex = new MockTokenizer( [ ( Token.Colon(0, 1, [||]), 0 ); ( Token.Colon(2, 3, [| |]), 2 ); ( Token.Comma(4, 5, [| |]), 4); ( Token.Colon(6, 7, [||]), 6 ); ( Token.Comma(8, 9, [| |]), 8); ( Token.RightBracket(9, 10, [| |]), 9);  ( Token.EOF([| |]), 11 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SubscriptList(0, 9, [|
                                                        ASTNode.Subscript(0, 4, ASTNode.Empty, Token.Colon(0, 1, [| |]), ASTNode.Empty, Token.Colon(2, 3, [| |]), ASTNode.Empty);
                                                        ASTNode.Subscript(6, 8, ASTNode.Empty, Token.Colon(6, 7, [| |]), ASTNode.Empty, Token.Empty, ASTNode.Empty);
                                                        |], [| Token.Comma(4, 5, [| |]); Token.Comma(8, 9, [| |]) |]) , parser.ParseSubscriptList())

    [<Fact>]
    let ``Single entry exprlist with trailing comma test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Comma(6, 7, [| |]), 6 ); ( Token.In(8, 10, [| |]), 8 ); ( Token.EOF([| |]), 11 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.ExprList(0, 8, [| ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])) |], [| Token.Comma(6, 7, [| |]) |]), parser.ParseExprList())

    [<Fact>]
    let ``Single entry exprlist test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.EOF([| |]), 6 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.ExprList(0, 6, [| ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])) |], [| |]), parser.ParseExprList())

    [<Fact>]
    let ``Double entry exprlist with star expr entry as number two test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Comma(6, 7, [| |]), 6 ); (Token.Mul(9, 10, [| |]), 9); ( Token.Name(10, 15, "Test2", [||]), 10); ( Token.In(12, 14, [| |]), 16 ); ( Token.EOF([| |]), 17 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.ExprList(0, 16, [| 
                                                    ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])); 
                                                    ASTNode.StarExpr(9, 16, Token.Mul(9, 10, [| |]), ASTNode.Name(10, 16, Token.Name(10, 15, "Test2", [| |])) )
                                              |], 
                                              [| 
                                                    Token.Comma(6, 7, [| |]) 
                                              |]), parser.ParseExprList())

    [<Fact>]
    let ``Single entry testlist with trailing comma test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Comma(6, 7, [| |]), 6 ); ( Token.SemiColon(8, 9, [| |]), 8 ); ( Token.EOF([| |]), 10 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.TestList(0, 8, [| ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])) |], [| Token.Comma(6, 7, [| |]) |]), parser.ParseTestList())

    [<Fact>]
    let ``Single entry testlist test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.EOF([| |]), 6 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.TestList(0, 6, [| ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])) |], [| |]), parser.ParseTestList())

    [<Fact>]
    let ``Double entry testlist test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Comma(6, 7, [| |]), 6 ); ( Token.Name(10, 15, "Test2", [||]), 10); ( Token.EOF([| |]), 17 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.TestList(0, 17, [| 
                                                    ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])); 
                                                    ASTNode.Name(10, 17, Token.Name(10, 15, "Test2", [| |]))
                                              |], 
                                              [| 
                                                    Token.Comma(6, 7, [| |]) 
                                              |]), parser.ParseTestList())

    [<Fact>]
    let ``Argument ( test ) test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.EOF([| |]), 6 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Argument(0, 6 , ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |]) ), Token.Empty, ASTNode.Empty )  , parser.ParseArgument())

    [<Fact>]
    let ``Argument ( test = test ) test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Assign(7, 8, [| |]), 7 ); ( Token.Name(10, 15, "Test2", [| |]), 10 ); ( Token.EOF([| |]), 16 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Argument(0, 16 , ASTNode.Name(0, 7, Token.Name(0, 5, "Test1", [| |]) ), Token.Assign(7, 8, [| |]), ASTNode.Name(10, 16, Token.Name(10, 15, "Test2", [| |])) )  , parser.ParseArgument())

    [<Fact>]
    let ``Argument ( test := test ) test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.ColonAssign(7, 9, [| |]), 7 ); ( Token.Name(10, 15, "Test2", [| |]), 10 ); ( Token.EOF([| |]), 16 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Argument(0, 16 , ASTNode.Name(0, 7, Token.Name(0, 5, "Test1", [| |]) ), Token.ColonAssign(7, 9, [| |]), ASTNode.Name(10, 16, Token.Name(10, 15, "Test2", [| |])) )  , parser.ParseArgument())

    [<Fact>]
    let ``Argument ( *test ) test`` () =
        let lex = new MockTokenizer( [ ( Token.Mul(0, 1, [| |]), 0 ); ( Token.Name(1, 6, "Test1", [| |]), 1 ); ( Token.EOF([| |]), 7 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Argument(0, 7 , ASTNode.Empty, Token.Mul(0, 1, [| |]), ASTNode.Name(0, 7, Token.Name(1, 6, "Test1", [| |]) ) )  , parser.ParseArgument())

    [<Fact>]
    let ``Argument ( **test ) test`` () =
        let lex = new MockTokenizer( [ ( Token.Power(0, 2, [| |]), 0 ); ( Token.Name(2, 7, "Test1", [| |]), 2 ); ( Token.EOF([| |]), 8 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Argument(0, 8 , ASTNode.Empty, Token.Power(0, 2, [| |]), ASTNode.Name(0, 8, Token.Name(2, 7, "Test1", [| |]) ) )  , parser.ParseArgument())

    [<Fact>]
    let ``Argument ( test for ... ) test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.For(7, 10, [| |]), 7 ); ( Token.Name(11, 12, "a", [| |]), 11 ); ( Token.In(14, 15, [| |]), 14 ); ( Token.Name(16, 17, "b", [| |]), 16 ); ( Token.EOF([| |]), 18 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Argument(0, 18 , ASTNode.Name(0, 7, Token.Name(0, 5, "Test1", [| |]) ), Token.Empty, 
                                                    ASTNode.SyncCompFor(7, 18,  Token.For(7, 10, [| |]), 
                                                                                ASTNode.ExprList( 11, 14, [| ASTNode.Name(11, 14, Token.Name(11, 12, "a", [| |])) |], [| |] ),
                                                                                Token.In(14, 15, [| |]), 
                                                                                ASTNode.Name(16, 18, Token.Name(16, 17, "b", [| |])),
                                                                                ASTNode.Empty
                                                                        )                          
                    )  , parser.ParseArgument())

    [<Fact>]
    let ``Argument ( test async for ... ) test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Async(7, 12, [| |]) , 7 ); ( Token.For(14, 17, [| |]), 14 ); ( Token.Name(18, 19, "a", [| |]), 18 ); ( Token.In(21, 22, [| |]), 21 ); ( Token.Name(23, 24, "b", [| |]), 23 ); ( Token.EOF([| |]), 24 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Argument(0, 24 , ASTNode.Name(0, 7, Token.Name(0, 5, "Test1", [| |]) ), Token.Empty, 
                                                    ASTNode.CompFor(7, 24, Token.Async(7, 12, [| |]),
                                                                        ASTNode.SyncCompFor(14, 24, Token.For(14, 17, [| |]), 
                                                                                                    ASTNode.ExprList( 18, 21, [| ASTNode.Name(18, 21, Token.Name(18, 19, "a", [| |])) |], [| |] ),
                                                                                                    Token.In(21, 22, [| |]), 
                                                                                                    ASTNode.Name(23, 24, Token.Name(23, 24, "b", [| |])),
                                                                                                    ASTNode.Empty
                                                                                            )    
                                                                    )                      
                    )  , parser.ParseArgument())

    [<Fact>]
    let ``Argumentlist with single entry test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.EOF([| |]), 6 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.ArgumentList(0, 6, [| 
                                                    ASTNode.Argument(0, 6, 
                                                            ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])),
                                                            Token.Empty,
                                                            ASTNode.Empty)
                                                |], [| |]), parser.ParseArgsList())

    [<Fact>]
    let ``Argumentlist with single entry with trailing comma test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Comma(6, 7, [| |]), 6); ( Token.RightParen(7, 8, [| |]), 7); ( Token.EOF([| |]), 8 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.ArgumentList(0, 7, [| 
                                                    ASTNode.Argument(0, 6, 
                                                            ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])),
                                                            Token.Empty,
                                                            ASTNode.Empty)
                                                |], [| Token.Comma(6, 7, [| |]) |]), parser.ParseArgsList())

    [<Fact>]
    let ``Argumentlist with multiple entries test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Comma(6, 7, [| |]), 6); ( Token.Name(7, 8, "a", [| |]), 7); ( Token.EOF([| |]), 8 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.ArgumentList(0, 8, [| 
                                                    ASTNode.Argument(0, 6, 
                                                            ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])),
                                                            Token.Empty,
                                                            ASTNode.Empty);
                                                    ASTNode.Argument(7, 8, 
                                                            ASTNode.Name(7, 8, Token.Name(7, 8, "a", [| |])),
                                                            Token.Empty,
                                                            ASTNode.Empty)
                                                |], [| Token.Comma(6, 7, [| |]) |]), parser.ParseArgsList())

    [<Fact>]
    let ``Testlist compfor single entry test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Comma(6, 7, [| |]), 6); ( Token.RightBracket(8, 9, [| |]), 8); ( Token.EOF([| |]), 10 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.TestList(0, 8, [| 
                                                    ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |]))
                                                |], [| Token.Comma(6, 7, [| |]) |]), parser.ParseTestListComp())

    [<Fact>]
    let ``Testlist compfor with for entry test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.For(7, 10, [| |]), 7 ); ( Token.Name(11, 12, "a", [| |]), 11 ); ( Token.In(14, 15, [| |]), 14 ); ( Token.Name(16, 17, "b", [| |]), 16 ); ( Token.EOF([| |]), 18 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.TestList(0, 18, [| 
                                                    ASTNode.Name(0, 7, Token.Name(0, 5, "Test1", [| |]));
                                                    ASTNode.SyncCompFor(7, 18,  Token.For(7, 10, [| |]), 
                                                        ASTNode.ExprList( 11, 14, [| ASTNode.Name(11, 14, Token.Name(11, 12, "a", [| |])) |], [| |] ),
                                                        Token.In(14, 15, [| |]), 
                                                        ASTNode.Name(16, 18, Token.Name(16, 17, "b", [| |])),
                                                        ASTNode.Empty
                                                        )
                                                |], [|  |]), parser.ParseTestListComp())

    [<Fact>]
    let ``Testlist compfor with async for entry test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Async(7, 12, [| |]) , 7 ); ( Token.For(14, 17, [| |]), 14 ); ( Token.Name(18, 19, "a", [| |]), 18 ); ( Token.In(21, 22, [| |]), 21 ); ( Token.Name(23, 24, "b", [| |]), 23 ); ( Token.EOF([| |]), 25 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.TestList(0, 25, [| 
                                                    ASTNode.Name(0, 7, Token.Name(0, 5, "Test1", [| |]));

                                                    ASTNode.CompFor(7, 25, Token.Async(7, 12, [||]),
                                                                        ASTNode.SyncCompFor(14, 25,  
                                                                            Token.For(14, 17, [| |]), 
                                                                            ASTNode.ExprList( 18, 21, [| ASTNode.Name(18, 21, Token.Name(18, 19, "a", [| |])) |], [| |] ),
                                                                            Token.In(21, 22, [| |]), 
                                                                            ASTNode.Name(23, 25, Token.Name(23, 24, "b", [| |])),
                                                                            ASTNode.Empty
                                                                    )
                                                          )              
                                                |], [|  |]), parser.ParseTestListComp())

    [<Fact>]
    let ``Double entry in TestListComp with star expr entry as number two test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Comma(6, 7, [| |]), 6 ); (Token.Mul(9, 10, [| |]), 9); ( Token.Name(10, 15, "Test2", [||]), 10); ( Token.In(12, 14, [| |]), 16 ); ( Token.EOF([| |]), 17 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.TestList(0, 16, [| 
                                                    ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])); 
                                                    ASTNode.StarExpr(9, 16, Token.Mul(9, 10, [| |]), ASTNode.Name(10, 16, Token.Name(10, 15, "Test2", [| |])) )
                                              |], 
                                              [| 
                                                    Token.Comma(6, 7, [| |]) 
                                              |]), parser.ParseTestListComp())

    [<Fact>]
    let ``Testlist compfor single entry with NamedExpr test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.ColonAssign(6, 8, [| |]), 6);  ( Token.Name(10, 15, "Test2", [| |]), 10 );  ( Token.Comma(16, 17, [| |]), 16); ( Token.RightBracket(18, 19, [| |]), 18); ( Token.EOF([| |]), 20 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.TestList(0, 18, [| 
                                                    ASTNode.NamedExpr(0, 16, 
                                                        ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])),
                                                        Token.ColonAssign(6, 8, [||]),
                                                        ASTNode.Name(10, 16, Token.Name(10, 15, "Test2", [| |])) )
                                                |], [| Token.Comma(16, 17, [| |]) |]), parser.ParseTestListComp())

    [<Fact>]
    let ``yield expr test`` () =
        let lex = new MockTokenizer( [ ( Token.Yield(0, 5, [| |]), 0 ); ( Token.Name(6, 11, "Test1", [| |]), 6 ); ( Token.ColonAssign(12, 14, [| |]), 12);  ( Token.Name(16, 21, "Test2", [| |]), 16 );  ( Token.Comma(22, 23, [| |]), 22); ( Token.SemiColon(24, 25, [| |]), 24); ( Token.EOF([| |]), 25 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.YieldExpr(0, 24,  Token.Yield(0, 5, [| |]),                        
                                                ASTNode.TestList(6, 24, [| 
                                                    ASTNode.NamedExpr(6, 22, 
                                                        ASTNode.Name(6, 12, Token.Name(6, 11, "Test1", [| |])),
                                                        Token.ColonAssign(12, 14, [||]),
                                                        ASTNode.Name(16, 22, Token.Name(16, 21, "Test2", [| |])) )
                                                |], [| Token.Comma(22, 23, [| |]) |]) ), parser.ParseYieldExpr())

    [<Fact>]
    let ``yield from expr test`` () =
        let lex = new MockTokenizer( [ ( Token.Yield(0, 5, [| |]), 0 ); ( Token.From(7, 11, [| |]), 7 ); ( Token.Name(12, 17, "Test1", [| |]), 12 ); ( Token.EOF([| |]), 13 ) ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.YieldFromExpr(0, 13,  Token.Yield(0, 5, [| |]), Token.From(7, 11, [| |]),
                                                        ASTNode.Name(12, 13, Token.Name(12, 17, "Test1", [| |]) ) ), parser.ParseYieldExpr())

    [<Fact>]
    let ``Testlist compfor with async for entry with if expr test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Async(7, 12, [| |]) , 7 ); ( Token.For(14, 17, [| |]), 14 ); ( Token.Name(18, 19, "a", [| |]), 18 ); ( Token.In(21, 22, [| |]), 21 ); ( Token.Name(23, 24, "b", [| |]), 23 ); ( Token.If( 26, 28, [| |]), 26 ); ( Token.Name(30, 35, "Test2", [| |]), 30 ); ( Token.EOF([| |]), 36 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.TestList(0, 36, [| 
                                                    ASTNode.Name(0, 7, Token.Name(0, 5, "Test1", [| |]));

                                                    ASTNode.CompFor(7, 36, Token.Async(7, 12, [||]),
                                                                        ASTNode.SyncCompFor(14, 36,  
                                                                            Token.For(14, 17, [| |]), 
                                                                            ASTNode.ExprList( 18, 21, [| ASTNode.Name(18, 21, Token.Name(18, 19, "a", [| |])) |], [| |] ),
                                                                            Token.In(21, 22, [| |]), 
                                                                            ASTNode.Name(23, 26, Token.Name(23, 24, "b", [| |])),
                                                                            ASTNode.CompIf( 26, 36, Token.If(26, 28, [| |]),
                                                                                                ASTNode.Name(30, 36, Token.Name(30, 35, "Test2", [| |])),
                                                                                                ASTNode.Empty 
                                                                                           )
                                                                    )
                                                          )              
                                                |], [|  |]), parser.ParseTestListComp())

    [<Fact>]
    let ``Testlist compfor with async for entry with additional async for expr test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Async(7, 12, [| |]) , 7 ); ( Token.For(14, 17, [| |]), 14 ); ( Token.Name(18, 19, "a", [| |]), 18 ); ( Token.In(21, 22, [| |]), 21 ); ( Token.Name(23, 24, "b", [| |]), 23 ); ( Token.Async( 26, 31, [| |]), 31 ); ( Token.For(33, 35, [| |]), 30 ); ( Token.Name(36, 41, "tall1", [| |]), 36 ); ( Token.In(42, 44, [| |]), 42); ( Token.Name(45, 50, "tall2", [| |]), 45 ); ( Token.EOF([| |]), 51 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.TestList(0, 51, [| 
                                                    ASTNode.Name(0, 7, Token.Name(0, 5, "Test1", [| |]));

                                                    ASTNode.CompFor(7, 51, Token.Async(7, 12, [||]),
                                                                        ASTNode.SyncCompFor(14, 51,  
                                                                            Token.For(14, 17, [| |]), 
                                                                            ASTNode.ExprList( 18, 21, [| ASTNode.Name(18, 21, Token.Name(18, 19, "a", [| |])) |], [| |] ),
                                                                            Token.In(21, 22, [| |]), 
                                                                            ASTNode.Name(23, 31, Token.Name(23, 24, "b", [| |])),
                                                                            
                                                                            ASTNode.CompFor(31, 51, Token.Async(26, 31, [||]),
                                                                                                    ASTNode.SyncCompFor(30, 51,  
                                                                                                        Token.For(33, 35, [| |]), 
                                                                                                        ASTNode.ExprList( 36, 42, [| ASTNode.Name(36, 42, Token.Name(36, 41, "tall1", [| |])) |], [| |] ),
                                                                                                        Token.In(42, 44, [| |]), 
                                                                                                        ASTNode.Name(45, 51, Token.Name(45, 50, "tall2", [| |])),
                                                                                                        ASTNode.Empty
                                                                                            )
                                                                                    ) 
                                                                    )
                                                          )              
                                                |], [|  |]), parser.ParseTestListComp())

    [<Fact>]
    let ``Testlist compfor with async for entry with additional for expr test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Async(7, 12, [| |]) , 7 ); ( Token.For(14, 17, [| |]), 14 ); ( Token.Name(18, 19, "a", [| |]), 18 ); ( Token.In(21, 22, [| |]), 21 ); ( Token.Name(23, 24, "b", [| |]), 23 ); ( Token.For(33, 35, [| |]), 30 ); ( Token.Name(36, 41, "tall1", [| |]), 36 ); ( Token.In(42, 44, [| |]), 42); ( Token.Name(45, 50, "tall2", [| |]), 45 ); ( Token.EOF([| |]), 51 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.TestList(0, 51, [| 
                                                    ASTNode.Name(0, 7, Token.Name(0, 5, "Test1", [| |]));

                                                    ASTNode.CompFor(7, 51, Token.Async(7, 12, [||]),
                                                                        ASTNode.SyncCompFor(14, 51,  
                                                                            Token.For(14, 17, [| |]), 
                                                                            ASTNode.ExprList( 18, 21, [| ASTNode.Name(18, 21, Token.Name(18, 19, "a", [| |])) |], [| |] ),
                                                                            Token.In(21, 22, [| |]), 
                                                                            ASTNode.Name(23, 30, Token.Name(23, 24, "b", [| |])),
                                                                            
                                                                                    ASTNode.SyncCompFor(30, 51,  
                                                                                        Token.For(33, 35, [| |]), 
                                                                                        ASTNode.ExprList( 36, 42, [| ASTNode.Name(36, 42, Token.Name(36, 41, "tall1", [| |])) |], [| |] ),
                                                                                        Token.In(42, 44, [| |]), 
                                                                                        ASTNode.Name(45, 51, Token.Name(45, 50, "tall2", [| |])),
                                                                                        ASTNode.Empty)
                                                                    )
                                                          )              
                                                |], [|  |]), parser.ParseTestListComp())

    [<Fact>]
    let ``Dictionary single entry test`` () =
        let lex = new MockTokenizer( [ ( Token.LeftCurly(0, 1, [| |]), 0 ); ( Token.Name(2, 7, "Test1", [| |]), 2 ); ( Token.Colon(8, 9, [| |]) , 8 ); ( Token.Name(10, 11, "a", [| |]), 10 ); ( Token.RightCurly(12, 13, [| |]), 12 ); ( Token.EOF( [| |] ), 14 )  ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Dictionary(0, 14, 
                                            Token.LeftCurly(0, 1, [| |]), 
                                            [|
                                                ASTNode.DictionaryEntry(2, 12, ASTNode.Name(2, 8, Token.Name(2, 7, "Test1", [| |])), Token.Colon(8, 9, [| |]), ASTNode.Name(10, 12, Token.Name(10, 11,"a", [| |]) ) )
                                            |], 
                                            [| |],
                                            Token.RightCurly(12, 13, [||]) ), parser.ParseAtom())

    [<Fact>]
    let ``Dictionary single entry with trailing comma test`` () =
        let lex = new MockTokenizer( [ ( Token.LeftCurly(0, 1, [| |]), 0 ); ( Token.Name(2, 7, "Test1", [| |]), 2 ); ( Token.Colon(8, 9, [| |]) , 8 ); ( Token.Name(10, 11, "a", [| |]), 10 ); ( Token.Comma(12, 13, [| |]), 12 ); ( Token.RightCurly(13, 14, [| |]), 13 ); ( Token.EOF( [| |] ), 15 )  ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Dictionary(0, 15, 
                                            Token.LeftCurly(0, 1, [| |]), 
                                            [|
                                                ASTNode.DictionaryEntry(2, 12, ASTNode.Name(2, 8, Token.Name(2, 7, "Test1", [| |])), Token.Colon(8, 9, [| |]), ASTNode.Name(10, 12, Token.Name(10, 11,"a", [| |]) ) )
                                            |], 
                                            [| Token.Comma(12, 13, [| |]) |],
                                            Token.RightCurly(13, 14, [| |]) ), parser.ParseAtom())

    [<Fact>]
    let ``Dictionary single entry with comp for test`` () =
        let lex = new MockTokenizer( [ ( Token.LeftCurly(0, 1, [| |]), 0 ); ( Token.Name(2, 7, "Test1", [| |]), 2 ); ( Token.Colon(8, 9, [| |]) , 8 ); ( Token.Name(10, 11, "a", [| |]), 10 ); ( Token.For(12, 15, [| |]), 12 ); ( Token.Name(16, 17, "a", [||]), 16 ); ( Token.In(18, 20, [||]), 18 ); ( Token.Name(21, 22, "b", [||]), 21 ); ( Token.RightCurly(22, 23, [| |]), 22 ); ( Token.EOF( [| |] ), 24 )  ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Dictionary(0, 24, 
                                            Token.LeftCurly(0, 1, [| |]), 
                                            [|
                                                ASTNode.DictionaryEntry(2, 12, ASTNode.Name(2, 8, Token.Name(2, 7, "Test1", [| |])), Token.Colon(8, 9, [| |]), ASTNode.Name(10, 12, Token.Name(10, 11,"a", [| |]) ) );
                                                ASTNode.DictionaryEntry(2, 22, ASTNode.Empty, Token.Empty, ASTNode.SyncCompFor( 12, 22,
                                                                                                                                Token.For(12, 15, [| |]),
                                                                                                                                ASTNode.ExprList (16, 18, [| ASTNode.Name(16, 18, Token.Name(16, 17, "a", [||])) |], [| |]),
                                                                                                                                Token.In(18, 20, [| |]),
                                                                                                                                ASTNode.Name(21, 22, Token.Name(21, 22, "b", [| |])),
                                                                                                                                ASTNode.Empty
                                                                                                                                ))
                                            |], 
                                            [| |],
                                            Token.RightCurly(22, 23, [||]) ), parser.ParseAtom())

    [<Fact>]
    let ``Dictionary single entry with comp async for test`` () =
        let lex = new MockTokenizer( [ ( Token.LeftCurly(0, 1, [| |]), 0 ); ( Token.Name(2, 7, "Test1", [| |]), 2 ); ( Token.Colon(8, 9, [| |]) , 8 ); ( Token.Name(10, 11, "a", [| |]), 10 ); ( Token.Async(12, 17, [| |]), 12 ); ( Token.For(18, 21, [| |]), 18 ); ( Token.Name(22, 23, "a", [||]), 22 ); ( Token.In(24, 26, [||]), 24 ); ( Token.Name(27, 28, "b", [||]), 27 ); ( Token.RightCurly(29, 30, [| |]), 29 ); ( Token.EOF( [| |] ), 31 )  ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Dictionary(0, 31, 
                                            Token.LeftCurly(0, 1, [| |]), 
                                            [|
                                                ASTNode.DictionaryEntry(2, 12, ASTNode.Name(2, 8, Token.Name(2, 7, "Test1", [| |])), Token.Colon(8, 9, [| |]), ASTNode.Name(10, 12, Token.Name(10, 11,"a", [| |]) ) );
                                                ASTNode.DictionaryEntry(2, 29, ASTNode.Empty, Token.Empty, ASTNode.CompFor(12, 29, Token.Async(12, 17, [| |]),
                                                                                                                                        ASTNode.SyncCompFor( 18, 29,
                                                                                                                                                Token.For(18, 21, [| |]),
                                                                                                                                                ASTNode.ExprList (22, 24, [| ASTNode.Name(22, 24, Token.Name(22, 23, "a", [||])) |], [| |]),
                                                                                                                                                Token.In(24, 26, [| |]),
                                                                                                                                                ASTNode.Name(27, 29, Token.Name(27, 28, "b", [| |])),
                                                                                                                                                ASTNode.Empty
                                                                                                                                                )) )
                                            |], 
                                            [| |],
                                            Token.RightCurly(29, 30, [||]) ), parser.ParseAtom())


    [<Fact>]
    let ``Dictionary single power entry test`` () =
        let lex = new MockTokenizer( [ ( Token.LeftCurly(0, 1, [| |]), 0 ); ( Token.Power(2, 4, [| |]), 2 ); ( Token.Name(5, 6, "a", [| |]), 5 ); ( Token.RightCurly(7, 8, [| |]), 7 ); ( Token.EOF( [| |] ), 9 )  ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Dictionary(0, 9, 
                                            Token.LeftCurly(0, 1, [| |]), 
                                            [|
                                                ASTNode.DictionaryEntry(2, 7, ASTNode.Empty, Token.Power(2, 4, [| |]), ASTNode.Name(5, 7, Token.Name(5, 6,"a", [| |]) ) )
                                            |], 
                                            [| |],
                                            Token.RightCurly(7, 8, [||]) ), parser.ParseAtom())

    [<Fact>]
    let ``Dictionary single entry with following power entry test`` () =
        let lex = new MockTokenizer( [ ( Token.LeftCurly(0, 1, [| |]), 0 ); ( Token.Name(2, 7, "Test1", [| |]), 2 ); ( Token.Colon(8, 9, [| |]) , 8 ); ( Token.Name(10, 11, "a", [| |]), 10 ); ( Token.Comma(12, 13, [| |]), 12 ); ( Token.Power(14, 15, [| |]), 14 ); ( Token.Name(16, 17, "a", [| |]), 16 ); ( Token.RightCurly(18, 19, [| |]), 18 ); ( Token.EOF( [| |] ), 20 )  ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Dictionary(0, 20, 
                                            Token.LeftCurly(0, 1, [| |]), 
                                            [|
                                                ASTNode.DictionaryEntry(2, 12, ASTNode.Name(2, 8, Token.Name(2, 7, "Test1", [| |])), Token.Colon(8, 9, [| |]), ASTNode.Name(10, 12, Token.Name(10, 11,"a", [| |]) ) );
                                                ASTNode.DictionaryEntry(2, 18, ASTNode.Empty, Token.Power(14, 15, [| |]), ASTNode.Name(16, 18, Token.Name(16, 17,"a", [| |]) ) )
                                            |], 
                                            [| Token.Comma(12, 13, [| |]) |],
                                            Token.RightCurly(18, 19, [| |]) ), parser.ParseAtom())

    [<Fact>]
    let ``Set single entry test`` () =
        let lex = new MockTokenizer( [ ( Token.LeftCurly(0, 1, [| |]), 0 ); ( Token.Name(2, 7, "Test1", [| |]), 2 ); ( Token.RightCurly(9, 10, [| |]), 9 ); ( Token.EOF( [| |] ), 11 )  ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Set(0, 11, 
                                            Token.LeftCurly(0, 1, [| |]), 
                                            [|
                                                ASTNode.SetEntry(2, 9, ASTNode.Name(2, 9, Token.Name(2, 7, "Test1", [| |])) )
                                            |], 
                                            [| |],
                                            Token.RightCurly(9, 10, [||]) ), parser.ParseAtom())

    [<Fact>]
    let ``Set single entry with trailing comma test`` () =
        let lex = new MockTokenizer( [ ( Token.LeftCurly(0, 1, [| |]), 0 ); ( Token.Name(2, 7, "Test1", [| |]), 2 ); ( Token.Comma(8, 9, [| |]), 8 ); ( Token.RightCurly(9, 10, [| |]), 9 ); ( Token.EOF( [| |] ), 11 )  ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Set(0, 11, 
                                            Token.LeftCurly(0, 1, [| |]), 
                                            [|
                                                ASTNode.SetEntry(2, 8, ASTNode.Name(2, 8, Token.Name(2, 7, "Test1", [| |])) )
                                            |], 
                                            [| Token.Comma(8, 9, [| |]) |],
                                            Token.RightCurly(9, 10, [||]) ), parser.ParseAtom())

    [<Fact>]
    let ``Set single entry with comp for test`` () =
        let lex = new MockTokenizer( [ ( Token.LeftCurly(0, 1, [| |]), 0 ); ( Token.Name(2, 7, "Test1", [| |]), 2 ); ( Token.For(8, 11, [| |]), 8 ); ( Token.Name(12, 13, "a", [||]), 12 ); ( Token.In(14, 16, [||]), 14 ); ( Token.Name(17, 18, "b", [||]), 17 ); ( Token.RightCurly(19, 20, [| |]), 19 ); ( Token.EOF( [| |] ), 21 )  ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Set(0, 21, 
                                            Token.LeftCurly(0, 1, [| |]), 
                                            [|
                                                ASTNode.SetEntry(2, 8, ASTNode.Name(2, 8, Token.Name(2, 7, "Test1", [| |])) );
                                                ASTNode.SetEntry(2, 19, ASTNode.SyncCompFor( 8, 19,
                                                                            Token.For(8, 11, [| |]),
                                                                            ASTNode.ExprList (12, 14, [| ASTNode.Name(12, 14, Token.Name(12, 13, "a", [||])) |], [| |]),
                                                                            Token.In(14, 16, [| |]),
                                                                            ASTNode.Name(17, 19, Token.Name(17, 18, "b", [| |])),
                                                                            ASTNode.Empty
                                                                            ))
                                            |], 
                                            [| |],
                                            Token.RightCurly(19, 20, [||]) ), parser.ParseAtom())

    [<Fact>]
    let ``Set single entry with comp async for test`` () =
        let lex = new MockTokenizer( [ ( Token.LeftCurly(0, 1, [| |]), 0 ); ( Token.Name(2, 7, "Test1", [| |]), 2 ); ( Token.Async(8, 13, [| |]), 8 ); ( Token.For(14, 17, [| |]), 14 ); ( Token.Name(18, 19, "a", [||]), 18 ); ( Token.In(20, 22, [||]), 20 ); ( Token.Name(23, 24, "b", [||]), 23 ); ( Token.RightCurly(25, 26, [| |]), 25 ); ( Token.EOF( [| |] ), 27 )  ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Set(0, 27, 
                                            Token.LeftCurly(0, 1, [| |]), 
                                            [|
                                                ASTNode.SetEntry(2, 8, ASTNode.Name(2, 8, Token.Name(2, 7, "Test1", [| |])) );
                                                ASTNode.SetEntry(2, 25,
                                                                    ASTNode.CompFor(8, 25,
                                                                                        Token.Async(8, 13, [| |]), 
                                                                                            ASTNode.SyncCompFor( 14, 25,
                                                                                                                    Token.For(14, 17, [| |]),
                                                                                                                    ASTNode.ExprList (18, 20, [| ASTNode.Name(18, 20, Token.Name(18, 19, "a", [||])) |], [| |]),
                                                                                                                    Token.In(20, 22, [| |]),
                                                                                                                    ASTNode.Name(23, 25, Token.Name(23, 24, "b", [| |])),
                                                                                                                    ASTNode.Empty
                                                                                            )
                                                                    ))                        
                                            |], 
                                            [| |],
                                            Token.RightCurly(25, 26, [||]) ), parser.ParseAtom())

    [<Fact>]
    let ``Set single entry *name test`` () =
        let lex = new MockTokenizer( [ ( Token.LeftCurly(0, 1, [| |]), 0 ); ( Token.Mul(2, 3, [| |] ), 2 ); ( Token.Name(3, 8, "Test1", [| |]), 3 ); ( Token.RightCurly(9, 10, [| |]), 9 ); ( Token.EOF( [| |] ), 11 )  ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Set(0, 11, 
                                            Token.LeftCurly(0, 1, [| |]), 
                                            [|
                                                ASTNode.SetEntry(2, 9, ASTNode.StarExpr(2, 9, Token.Mul(2, 3, [| |]), ASTNode.Name(3, 9, Token.Name(3, 8, "Test1", [| |])) ) )
                                            |], 
                                            [| |],
                                            Token.RightCurly(9, 10, [||]) ), parser.ParseAtom())

    [<Fact>]
    let ``Set single entry followed by star exor entry test`` () =
        let lex = new MockTokenizer( [ ( Token.LeftCurly(0, 1, [| |]), 0 ); ( Token.Name(2, 7, "Test1", [| |]), 2 ); ( Token.Comma(8, 9, [| |]), 8 ); ( Token.Mul(10, 11, [| |] ), 10 ); ( Token.Name(12, 17, "Test1", [| |]), 12 ); ( Token.RightCurly(18, 19, [| |]), 18 ); ( Token.EOF( [| |] ), 19 )  ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Set(0, 19, 
                                            Token.LeftCurly(0, 1, [| |]), 
                                            [|
                                                ASTNode.SetEntry(2, 8, ASTNode.Name(2, 8, Token.Name(2, 7, "Test1", [| |])) );
                                                ASTNode.SetEntry(2, 18, ASTNode.StarExpr(10, 18, Token.Mul(10, 11, [| |]), ASTNode.Name(12, 18, Token.Name(12, 17, "Test1", [| |])) ) )
                                            |], 
                                            [| Token.Comma(8, 9, [| |]) |],
                                            Token.RightCurly(18, 19, [||]) ), parser.ParseAtom())

    [<Fact>]
    let ``( single entry) literal test`` () =
        let lex = new MockTokenizer( [ ( Token.LeftParen(0, 1, [| |]), 0 ); ( Token.Name(2, 3, "a", [| |]), 2 ); ( Token.RightParen(4, 5, [| |]), 4 ); ( Token.EOF([| |]), 6 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Tuple(0, 6, 
                                        Token.LeftParen(0, 1, [| |]), 
                                        ASTNode.TestList(2, 4, [| ASTNode.Name(2, 4, Token.Name(2, 3, "a", [| |])) |], [| |]), 
                                        Token.RightParen(4, 5, [| |]) ), parser.ParseAtom())

    [<Fact>]
    let ``( single entry yield ) literal test`` () =
        let lex = new MockTokenizer( [ ( Token.LeftParen(0, 1, [| |]), 0 ); ( Token.Yield(2, 7, [| |]), 2 ); ( Token.Name(8, 9, "a", [| |]), 8 );  ( Token.RightParen(10, 11, [| |]), 10 ); ( Token.EOF([| |]), 12 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.Tuple(0, 12, 
                                        Token.LeftParen(0, 1, [| |]), 
                                        ASTNode.YieldExpr(2, 10, Token.Yield(2, 7, [| |]), ASTNode.TestList(8, 10, [| ASTNode.Name(8, 10, Token.Name(8, 9, "a", [| |])) |], [| |]) ), 
                                        Token.RightParen(10, 11, [| |]) ), parser.ParseAtom())

    [<Fact>]
    let ``[ single entry ] literal test`` () =
        let lex = new MockTokenizer( [ ( Token.LeftBracket(0, 1, [| |]), 0 ); ( Token.Name(2, 3, "a", [| |]), 2 ); ( Token.RightBracket(4, 5, [| |]), 4 ); ( Token.EOF([| |]), 6 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.List(0, 6, 
                        Token.LeftBracket(0, 1, [| |]), 
                        ASTNode.TestList(2, 4, [| ASTNode.Name(2, 4, Token.Name(2, 3, "a", [| |])) |], [| |]), 
                        Token.RightBracket(4, 5, [| |]) ), parser.ParseAtom())

    [<Fact>]
    let ``async name with ( single entry ) test`` () =
        let lex = new MockTokenizer( [ ( Token.Async(0, 5, [| |]), 0 ); ( Token.Name(7, 11, "Test", [| |]), 7 ); ( Token.LeftParen(11, 12, [| |]), 11 ); ( Token.Name(13, 17, "Test", [| |]), 13 ); ( Token.RightParen(18, 19, [| |]), 18 );  ( Token.EOF([| |]), 20 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.AtomExpr(0, 20, Token.Async(0, 5, [| |]), ASTNode.Name(7, 11, Token.Name(7, 11, "Test", [| |])), 
                                                                            [|    ASTNode.Call(11, 20,  
                                                                                        Token.LeftParen(11, 12, [| |]), 
                                                                                        ASTNode.ArgumentList(13, 18, 
                                                                                            [|
                                                                                                ASTNode.Argument(13, 18, ASTNode.Name(13, 18, Token.Name(13, 17, "Test", [| |])), Token.Empty, ASTNode.Empty )
                                                                                            |], [| |]), 
                                                                                        Token.RightParen(18, 19, [| |]) );      |]), parser.ParseAtomExpr())

    [<Fact>]
    let ``async name with [ single entry ] test`` () =
        let lex = new MockTokenizer( [ ( Token.Async(0, 5, [| |]), 0 ); ( Token.Name(7, 11, "Test", [| |]), 7 ); ( Token.LeftBracket(11, 12, [| |]), 11 ); ( Token.Name(13, 17, "Test", [| |]), 13 );  ( Token.RightBracket(18, 19, [| |]), 18 );  ( Token.EOF([| |]), 20 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.AtomExpr(0, 20, Token.Async(0, 5, [| |]), ASTNode.Name(7, 11, Token.Name(7, 11, "Test", [| |])), 
                                                                [|    ASTNode.Index(11, 20,  
                                                                        Token.LeftBracket(11, 12, [| |]), 
                                                                        ASTNode.SubscriptList(13, 18, [|
                                                                                                        ASTNode.Subscript(13, 18, ASTNode.Name(13, 18, Token.Name(13, 17, "Test", [| |])), Token.Empty, ASTNode.Empty, Token.Empty, ASTNode.Empty)  
                                                                                                    |], [| |]), 
                                                                        Token.RightBracket(18, 19, [| |]) );      |]), parser.ParseAtomExpr())

// Statement unittests ////////////////////////////////////////////////////////////////////////////////////////////////

    [<Fact>]
    let `` Del Statement test`` () =
        let lex = new MockTokenizer( [ ( Token.Del(0, 3, [| |]), 0 ); ( Token.Name(4, 8, "Test", [| |]), 4 ); ( Token.Newline(9, 11, [| |]), 9 ); ( Token.EOF([| |]), 12 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 12, [|
                                                            ASTNode.Del(0, 9, Token.Del(0, 3, [| |]), ASTNode.ExprList(4, 9, [| ASTNode.Name(4, 9, Token.Name(4, 8, "Test", [| |])) |], [| |]) )
                                                    |], 
                                                    [| |], Token.Newline(9, 11, [| |])), parser.ParseStmt())

    [<Fact>]
    let ``Pass Statement test`` () =
        let lex = new MockTokenizer( [ ( Token.Pass(0, 4, [| |]), 0 ); ( Token.Newline(5, 7, [| |]), 5 ); ( Token.EOF([| |]), 8 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 8, [|
                                                            ASTNode.Pass(0, 5, Token.Pass(0, 4, [| |]))
                                                    |], 
                                                    [| |], Token.Newline(5, 7, [| |])), parser.ParseStmt())

    [<Fact>]
    let ``Break Statement test`` () =
        let lex = new MockTokenizer( [ ( Token.Break(0, 4, [| |]), 0 ); ( Token.Newline(5, 7, [| |]), 5 ); ( Token.EOF([| |]), 8 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        parser.FlowLevel <- 1
        Assert.Equal( ASTNode.SimpleStmtList(0, 8, [|
                                                            ASTNode.Break(0, 5, Token.Break(0, 4, [| |]))
                                                    |], 
                                                    [| |], Token.Newline(5, 7, [| |])), parser.ParseStmt())

    [<Fact>]
    let ``Continue Statement test`` () =
        let lex = new MockTokenizer( [ ( Token.Continue(0, 8, [| |]), 0 ); ( Token.Newline(9, 11, [| |]), 9 ); ( Token.EOF([| |]), 12 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        parser.FlowLevel <- 1
        Assert.Equal( ASTNode.SimpleStmtList(0, 12, [|
                                                            ASTNode.Continue(0, 9, Token.Continue(0, 8, [| |]))
                                                    |], 
                                                    [| |], Token.Newline(9, 11, [| |])), parser.ParseStmt())

    [<Fact>]
    let ``Return Statement test`` () =
        let lex = new MockTokenizer( [ ( Token.Return(0, 6, [| |]), 0 ); ( Token.Newline(7, 9, [| |]), 7 ); ( Token.EOF([| |]), 10 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        parser.FuncFlowLevel <- 1
        Assert.Equal( ASTNode.SimpleStmtList(0, 10, [|
                                                            ASTNode.Return(0, 7, Token.Return(0, 6, [| |]), ASTNode.Empty)
                                                    |], 
                                                    [| |], Token.Newline(7, 9, [| |])), parser.ParseStmt())

    [<Fact>]
    let ``Return expression Statement test`` () =
        let lex = new MockTokenizer( [ ( Token.Return(0, 6, [| |]), 0 ); ( Token.Name(7, 11, "Test", [| |]), 7 ); ( Token.Newline(12, 13, [| |]), 12 ); ( Token.EOF([| |]), 14 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        parser.FuncFlowLevel <- 1
        Assert.Equal( ASTNode.SimpleStmtList(0, 14, [|
                                                            ASTNode.Return(0, 12, Token.Return(0, 6, [| |]), ASTNode.TestList(7, 12, [| ASTNode.Name(7, 12, Token.Name(7, 11, "Test", [| |])) |], [| |]))
                                                    |], 
                                                    [| |], Token.Newline(12, 13, [| |])), parser.ParseStmt())

    [<Fact>]
    let ``Yield Statement test`` () =
        let lex = new MockTokenizer( [ ( Token.Yield(0, 5, [| |]), 0 ); ( Token.Name(6, 10, "Test", [| |]), 6 ); ( Token.Newline(11, 13, [| |]), 11 ); ( Token.EOF([| |]), 14 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        parser.FlowLevel <- 1
        Assert.Equal( ASTNode.SimpleStmtList(0, 14, [|
                                                            ASTNode.YieldExpr(0, 11, Token.Yield(0, 5, [| |]), ASTNode.TestList(6, 11, [| ASTNode.Name(6, 11, Token.Name(6, 10, "Test", [| |])) |], [||]))
                                                    |], 
                                                    [| |], Token.Newline(11, 13, [| |])), parser.ParseStmt())

    [<Fact>]
    let ``Raise Statement test`` () =
        let lex = new MockTokenizer( [ ( Token.Raise(0, 5, [| |]), 0 ); ( Token.Newline(6, 8, [| |]), 6 ); ( Token.EOF([| |]), 9 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        parser.FlowLevel <- 1
        Assert.Equal( ASTNode.SimpleStmtList(0, 9, [|
                                                            ASTNode.Raise(0, 6, Token.Raise(0, 5, [| |]), ASTNode.Empty, Token.Empty, ASTNode.Empty)
                                                    |], 
                                                    [| |], Token.Newline(6, 8, [| |])), parser.ParseStmt())

    [<Fact>]
    let ``Raise Statement with one argument test`` () =
        let lex = new MockTokenizer( [ ( Token.Raise(0, 5, [| |]), 0 ); ( Token.Name(6, 10, "Test", [| |]), 6 ); ( Token.Newline(11, 13, [| |]), 11 ); ( Token.EOF([| |]), 14 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        parser.FlowLevel <- 1
        Assert.Equal( ASTNode.SimpleStmtList(0, 14, [|
                                                            ASTNode.Raise(0, 11, Token.Raise(0, 5, [| |]), ASTNode.Name(6, 11, Token.Name(6, 10, "Test", [| |])), Token.Empty, ASTNode.Empty)
                                                    |], 
                                                    [| |], Token.Newline(11, 13, [| |])), parser.ParseStmt())

    [<Fact>]
    let ``Raise Statement with two argument test`` () =
        let lex = new MockTokenizer( [ ( Token.Raise(0, 5, [| |]), 0 ); ( Token.Name(6, 10, "Test", [| |]), 6 ); ( Token.From(11, 15, [| |]), 11 ); ( Token.Name(16, 20, "Tall", [| |]), 16 ); ( Token.Newline(21, 23, [| |]), 21 ); ( Token.EOF([| |]), 24 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        parser.FlowLevel <- 1
        Assert.Equal( ASTNode.SimpleStmtList(0, 24, [|
                                                            ASTNode.Raise(0, 21, Token.Raise(0, 5, [| |]), ASTNode.Name(6, 11, Token.Name(6, 10, "Test", [| |])), Token.From(11, 15, [| |]), ASTNode.Name(16, 21, Token.Name(16, 20, "Tall", [| |])))
                                                    |], 
                                                    [| |], Token.Newline(21, 23, [| |])), parser.ParseStmt())

    [<Fact>]
    let ``Global Statement with one argument test`` () =
        let lex = new MockTokenizer( [ ( Token.Global(0, 6, [| |]), 0 ); ( Token.Name(7, 11, "Test", [| |]), 7 ); ( Token.Newline(12, 14, [| |]), 12 ); ( Token.EOF([| |]), 15 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 15, [|
                                                            ASTNode.Global(0, 12, Token.Global(0, 6, [| |]), 
                                                                                [|
                                                                                    ASTNode.Name(7, 12, Token.Name(7, 11, "Test", [| |]))
                                                                                |], [| |] )
                                                    |], 
                                                    [| |], Token.Newline(12, 14, [| |])), parser.ParseStmt())

    [<Fact>]
    let ``Global Statement with two argument test`` () =
        let lex = new MockTokenizer( [ ( Token.Global(0, 6, [| |]), 0 ); ( Token.Name(7, 11, "Test", [| |]), 7 ); ( Token.Comma(12, 13, [| |]), 12 ); ( Token.Name(14, 18, "Tall", [| |]), 14 ); ( Token.Newline(19, 21, [| |]), 19 ); ( Token.EOF([| |]), 22 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 22, [|
                                                            ASTNode.Global(0, 19, Token.Global(0, 6, [| |]), 
                                                                                [|
                                                                                    ASTNode.Name(7, 12, Token.Name(7, 11, "Test", [| |]))
                                                                                    ASTNode.Name(14, 19, Token.Name(14, 18, "Tall", [| |]))
                                                                                |], [| Token.Comma(12, 13, [| |]) |] )
                                                    |], 
                                                    [| |], Token.Newline(19, 21, [| |])), parser.ParseStmt())

    [<Fact>]
    let ``Nonlocal Statement with one argument test`` () =
        let lex = new MockTokenizer( [ ( Token.Nonlocal(0, 8, [| |]), 0 ); ( Token.Name(9, 13, "Test", [| |]), 9 ); ( Token.Newline(14, 16, [| |]), 14 ); ( Token.EOF([| |]), 17 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 17, [|
                                                            ASTNode.Nonlocal(0, 14, Token.Nonlocal(0, 8, [| |]), 
                                                                                [|
                                                                                    ASTNode.Name(9, 14, Token.Name(9, 13, "Test", [| |]))
                                                                                |], [| |] )
                                                    |], 
                                                    [| |], Token.Newline(14, 16, [| |])), parser.ParseStmt())

    [<Fact>]
    let ``Nonlocal Statement with two argument test`` () =
        let lex = new MockTokenizer( [ ( Token.Nonlocal(0, 8, [| |]), 0 ); ( Token.Name(9, 13, "Test", [| |]), 9 ); ( Token.Comma(14, 15, [| |]), 14 ); ( Token.Name(16, 20, "Tall", [| |]), 16 ); ( Token.Newline(21, 23, [| |]), 21 ); ( Token.EOF([| |]), 24 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 24, [|
                                                            ASTNode.Nonlocal(0, 21, Token.Nonlocal(0, 8, [| |]), 
                                                                                [|
                                                                                    ASTNode.Name(9, 14, Token.Name(9, 13, "Test", [| |]))
                                                                                    ASTNode.Name(16, 21, Token.Name(16, 20, "Tall", [| |]))
                                                                                |], [| Token.Comma(14, 15, [| |]) |] )
                                                    |], 
                                                    [| |], Token.Newline(21, 23, [| |])), parser.ParseStmt())

    [<Fact>]
    let ``Assert Statement with one argument test`` () =
        let lex = new MockTokenizer( [ ( Token.Assert(0, 6, [| |]), 0 ); ( Token.Name(7, 11, "Test", [| |]), 7 ); ( Token.Newline(12, 14, [| |]), 12 ); ( Token.EOF([| |]), 15 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 15, [|
                                                            ASTNode.Assert(0, 12, Token.Assert(0, 6, [| |]), ASTNode.Name(7, 12, Token.Name(7, 11, "Test", [| |])), Token.Empty, ASTNode.Empty)
                                                    |], 
                                                    [| |], Token.Newline(12, 14, [| |])), parser.ParseStmt())

    [<Fact>]
    let ``Assert Statement with two argument test`` () =
        let lex = new MockTokenizer( [ ( Token.Assert(0, 6, [| |]), 0 ); ( Token.Name(7, 11, "Test", [| |]), 7 ); ( Token.Comma(12, 13, [| |]), 12 ); ( Token.Name(14, 18, "Fest", [| |]), 14 ); ( Token.Newline(19, 21, [| |]), 19 ); ( Token.EOF([| |]), 22 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 22, [|
                                                            ASTNode.Assert(0, 19, Token.Assert(0, 6, [| |]), ASTNode.Name(7, 12, Token.Name(7, 11, "Test", [| |])), Token.Comma(12, 13, [| |]), ASTNode.Name(14, 19, Token.Name(14, 18, "Fest", [| |])))
                                                    |], 
                                                    [| |], Token.Newline(19, 21, [| |])), parser.ParseStmt())

    [<Fact>]
    let ``Import single name test`` () =
        let lex = new MockTokenizer( [ ( Token.Import(0, 6, [| |]), 0 ); ( Token.Name(7, 11, "Test", [| |]), 7 ); ( Token.Newline(12, 14, [| |]), 12 ); ( Token.EOF([| |]), 15 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 15, [| 
                                                        ASTNode.Import(0, 12, Token.Import(0, 6, [| |]), 
                                                                                                    ASTNode.DottedAsNames(7, 12, [|
                                                                                                                                        ASTNode.DottedAsName(7, 12, ASTNode.DottedName(7, 12,   [|
                                                                                                                                                                                                    ASTNode.Name(7, 12, Token.Name(7, 11, "Test", [| |]))
                                                                                                                                                                                                |], 
                                                                                                        
                                                                                                                                                                    [| |]
                                                                                                                                                                ), Token.Empty, ASTNode.Empty)
                                                                                                                                |], [||])
                                                                                                )
                                                    |], [| |], Token.Newline(12, 14, [| |])), parser.ParseStmt())

    [<Fact>]
    let ``Import single name name test`` () =
        let lex = new MockTokenizer( [ ( Token.Import(0, 6, [| |]), 0 ); ( Token.Name(7, 11, "Test", [| |]), 7 ); ( Token.Dot(11, 12, [| |]), 11 ); ( Token.Name(12, 16, "Tell", [| |]), 12 ); ( Token.Newline(17, 19, [| |]), 17 ); ( Token.EOF([| |]), 20 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 20, [| 
                                                        ASTNode.Import(0, 17, Token.Import(0, 6, [| |]), 
                                                                                                    ASTNode.DottedAsNames(7, 17, [|
                                                                                                                                        ASTNode.DottedAsName(7, 17, ASTNode.DottedName(7, 17,   [|
                                                                                                                                                                                                    ASTNode.Name(7, 11, Token.Name(7, 11, "Test", [| |]));
                                                                                                                                                                                                    ASTNode.Name(12, 17, Token.Name(12, 16, "Tell", [| |]))
                                                                                                                                                                                                |], 
                                                                                                        
                                                                                                                                                                    [| Token.Dot(11, 12, [| |]) |]
                                                                                                                                                                ), Token.Empty, ASTNode.Empty)
                                                                                                                                |], [||])
                                                                                                )
                                                    |], [| |], Token.Newline(17, 19, [| |])), parser.ParseStmt())

    [<Fact>]
    let ``Import single name name as name test`` () =
        let lex = new MockTokenizer( [ ( Token.Import(0, 6, [| |]), 0 ); ( Token.Name(7, 11, "Test", [| |]), 7 ); ( Token.Dot(11, 12, [| |]), 11 ); ( Token.Name(12, 16, "Tell", [| |]), 12 ); ( Token.As(17, 19, [| |]), 17 ); ( Token.Name(20, 21, "a", [| |]), 20 ); ( Token.Newline(22, 24, [| |]), 22 ); ( Token.EOF([| |]), 25 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 25, [| 
                                                        ASTNode.Import(0, 22, Token.Import(0, 6, [| |]), 
                                                                                                    ASTNode.DottedAsNames(7, 22, [|
                                                                                                                                        ASTNode.DottedAsName(7, 22, ASTNode.DottedName(7, 17,   [|
                                                                                                                                                                                                    ASTNode.Name(7, 11, Token.Name(7, 11, "Test", [| |]));
                                                                                                                                                                                                    ASTNode.Name(12, 17, Token.Name(12, 16, "Tell", [| |]))
                                                                                                                                                                                                |], 
                                                                                                        
                                                                                                                                                                    [| Token.Dot(11, 12, [| |]) |]
                                                                                                                                                                ), Token.As(17, 19, [| |]), ASTNode.Name(20, 22, Token.Name(20, 21, "a", [| |])))
                                                                                                                                |], [| |])
                                                                                                )
                                                    |], [| |], Token.Newline(22, 24, [| |])), parser.ParseStmt())

    [<Fact>]
    let ``Import multilple name name as name test`` () =
        let lex = new MockTokenizer( [ ( Token.Import(0, 6, [| |]), 0 ); ( Token.Name(7, 11, "Test", [| |]), 7 ); ( Token.Dot(11, 12, [| |]), 11 ); ( Token.Name(12, 16, "Tell", [| |]), 12 ); ( Token.As(17, 19, [| |]), 17 ); ( Token.Name(20, 21, "a", [| |]), 20 ); 
                                        ( Token.Comma(22, 23, [| |]), 22 ); ( Token.Name(24, 28, "Test", [| |]), 24 ); ( Token.Dot(28, 29, [| |]), 28 ); ( Token.Name(29, 33, "Tell", [| |]), 29 ); ( Token.As(34, 36, [| |]), 34 ); ( Token.Name(36, 37, "a", [| |]), 36 );
                                        ( Token.Newline(38, 40, [| |]), 38 ); ( Token.EOF([| |]), 41 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 41, [| 
                                                        ASTNode.Import(0, 38, Token.Import(0, 6, [| |]), 
                                                                                                    ASTNode.DottedAsNames(7, 38, [|
                                                                                                                ASTNode.DottedAsName(7, 22, ASTNode.DottedName(7, 17,   [|
                                                                                                                                                                            ASTNode.Name(7, 11, Token.Name(7, 11, "Test", [| |]));
                                                                                                                                                                            ASTNode.Name(12, 17, Token.Name(12, 16, "Tell", [| |]))
                                                                                                                                                                        |], 
                                                                                                        
                                                                                                                                            [| Token.Dot(11, 12, [| |]) |]
                                                                                                                                        ), Token.As(17, 19, [| |]), ASTNode.Name(20, 22, Token.Name(20, 21, "a", [| |])));
                                                                                                                            
                                                                                                                ASTNode.DottedAsName(24, 38, ASTNode.DottedName(24, 34,   [|
                                                                                                                                                                            ASTNode.Name(24, 28, Token.Name(24, 28, "Test", [| |]));
                                                                                                                                                                            ASTNode.Name(29, 34, Token.Name(29, 33, "Tell", [| |]))
                                                                                                                                                                        |], 
                                                                                                        
                                                                                                                                            [| Token.Dot(28, 29, [| |]) |]
                                                                                                                                        ), Token.As(34, 36, [| |]), ASTNode.Name(36, 38, Token.Name(36, 37, "a", [| |])))
                                                                                                                            |], [| Token.Comma(22, 23, [| |]) |])
                                                                                                )
                                                    |], [| |], Token.Newline(38, 40, [| |])), parser.ParseStmt())

    [<Fact>]
    let ``import from first test`` () =
        let lex = new MockTokenizer( [ ( Token.From(0, 4, [| |]), 0 ); ( Token.Dot(5, 6, [| |]), 5 ); ( Token.Import(7, 13, [| |]), 7 ); ( Token.Mul(14, 15, [| |]), 14 ); ( Token.Newline(16, 18, [| |]), 16 ); ( Token.EOF([| |]), 19 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 19, [|
                                                          ASTNode.ImportFrom(0, 16, Token.From(0, 4, [||]), [| Token.Dot(5, 6, [||]) |], ASTNode.Empty, Token.Import(7, 13, [| |]), Token.Mul(14, 15, [||]), ASTNode.Empty, Token.Empty )  
                                                    |], 
                                                    [| |], Token.Newline(16, 18, [| |])), parser.ParseStmt())

    [<Fact>]
    let ``import from second test`` () =
        let lex = new MockTokenizer( [ ( Token.From(0, 4, [| |]), 0 ); ( Token.Dot(5, 6, [| |]), 5 ); ( Token.Elipsis(6, 9, [| |]), 6 ); ( Token.Import(10, 16, [| |]), 10 ); ( Token.Mul(17, 18, [| |]), 17 ); ( Token.Newline(19, 21, [| |]), 19 ); ( Token.EOF([| |]), 22 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 22, [|
                                                          ASTNode.ImportFrom(0, 19, Token.From(0, 4, [||]), [| Token.Dot(5, 6, [||]); Token.Elipsis(6, 9, [| |]) |], ASTNode.Empty, Token.Import(10, 16, [| |]), Token.Mul(17, 18, [||]), ASTNode.Empty, Token.Empty )  
                                                    |], 
                                                    [| |], Token.Newline(19, 21, [| |])), parser.ParseStmt())

    [<Fact>]
    let ``import from third test`` () =
        let lex = new MockTokenizer( [ ( Token.From(0, 4, [| |]), 0 ); ( Token.Dot(5, 6, [| |]), 5 ); ( Token.Elipsis(6, 9, [| |]), 6 ); ( Token.Name(9, 13, "test", [| |]), 9 ); ( Token.Import(14, 20, [| |]), 14 ); ( Token.Mul(21, 22, [| |]), 21 ); ( Token.Newline(22, 23, [| |]), 22 ); ( Token.EOF([| |]), 24 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 24, [|
                                                          ASTNode.ImportFrom(0, 22, Token.From(0, 4, [||]), [| Token.Dot(5, 6, [||]); Token.Elipsis(6, 9, [| |]) |], 
                                                                            ASTNode.DottedName(9, 14, [| ASTNode.Name(9, 14, Token.Name(9, 13, "test", [| |])) |], [| |]), 
                                                                            Token.Import(14, 20, [| |]), Token.Mul(21, 22, [||]), ASTNode.Empty, Token.Empty )  
                                                    |], 
                                                    [| |], Token.Newline(22, 23, [| |])), parser.ParseStmt())

    [<Fact>]
    let ``import from forth test`` () =
        let lex = new MockTokenizer( [ ( Token.From(0, 4, [| |]), 0 ); ( Token.Dot(5, 6, [| |]), 5 ); ( Token.Elipsis(6, 9, [| |]), 6 ); ( Token.Name(9, 13, "test", [| |]), 9 ); ( Token.Dot(13, 14, [| |]), 13 ); ( Token.Name(14, 18, "tell", [| |]), 14 ); ( Token.Import(19, 25, [| |]), 19 ); ( Token.Mul(26, 27, [| |]), 26 ); ( Token.Newline(27, 28, [| |]), 27 ); ( Token.EOF([| |]), 29 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 29, [|
                                                          ASTNode.ImportFrom(0, 27, Token.From(0, 4, [||]), [| Token.Dot(5, 6, [||]); Token.Elipsis(6, 9, [| |]) |], 
                                                                            ASTNode.DottedName(9, 19, [| 
                                                                                                            ASTNode.Name(9, 13, Token.Name(9, 13, "test", [| |])); 
                                                                                                            ASTNode.Name(14, 19, Token.Name(14, 18, "tell", [| |]))      
                                                                                                      |], [| Token.Dot(13, 14, [| |]) |]), 
                                                                            Token.Import(19, 25, [| |]), Token.Mul(26, 27, [||]), ASTNode.Empty, Token.Empty )  
                                                    |], 
                                                    [| |], Token.Newline(27, 28, [| |])), parser.ParseStmt())

    [<Fact>]
    let ``import from fifth test`` () =
        let lex = new MockTokenizer( [ ( Token.From(0, 4, [| |]), 0 ); ( Token.Dot(5, 6, [| |]), 5 ); ( Token.Elipsis(6, 9, [| |]), 6 ); ( Token.Name(9, 13, "test", [| |]), 9 ); ( Token.Dot(13, 14, [| |]), 13 ); ( Token.Name(14, 18, "tell", [| |]), 14 ); ( Token.Import(19, 25, [| |]), 19 ); ( Token.LeftParen(26, 27, [| |]), 26 ); ( Token.Name(28, 29, "a", [| |]), 28 ); ( Token.RightParen(30, 31, [| |]), 30 ); ( Token.Newline(31, 32, [| |]), 33 ); ( Token.EOF([| |]), 29 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 29, [|
                                                          ASTNode.ImportFrom(0, 33, Token.From(0, 4, [||]), [| Token.Dot(5, 6, [||]); Token.Elipsis(6, 9, [| |]) |], 
                                                                            ASTNode.DottedName(9, 19, [| 
                                                                                                            ASTNode.Name(9, 13, Token.Name(9, 13, "test", [| |])); 
                                                                                                            ASTNode.Name(14, 19, Token.Name(14, 18, "tell", [| |]))      
                                                                                                      |], [| Token.Dot(13, 14, [| |]) |]), 
                                                                            Token.Import(19, 25, [| |]), 
                                                                                Token.LeftParen(26, 27, [||]), 
                                                                                ASTNode.ImportAsNames(28, 30,   [| 
                                                                                                                    ASTNode.ImportAsName(28, 30, ASTNode.Name(28, 30, Token.Name(28, 29, "a", [| |])), Token.Empty, ASTNode.Empty)
                                                                                                                |], [| |]), 
                                                                                Token.RightParen(30, 31, [| |] )  )
                                                    |], 
                                                    [| |], Token.Newline(31, 32, [| |])), parser.ParseStmt())

    [<Fact>]
    let ``import from sixth test`` () =
        let lex = new MockTokenizer( [ ( Token.From(0, 4, [| |]), 0 ); ( Token.Dot(5, 6, [| |]), 5 ); ( Token.Elipsis(6, 9, [| |]), 6 ); ( Token.Name(9, 13, "test", [| |]), 9 ); ( Token.Dot(13, 14, [| |]), 13 ); ( Token.Name(14, 18, "tell", [| |]), 14 ); ( Token.Import(19, 25, [| |]), 19 ); ( Token.LeftParen(26, 27, [| |]), 26 ); ( Token.Name(28, 29, "a", [| |]), 28 ); ( Token.As(30, 32, [| |]), 30 ); ( Token.Name(33, 34, "b", [| |]), 33); ( Token.RightParen(35, 36, [| |]), 35 ); ( Token.Newline(37, 39, [| |]), 37 ); ( Token.EOF([| |]), 40 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 40, [|
                                                          ASTNode.ImportFrom(0, 37, Token.From(0, 4, [||]), [| Token.Dot(5, 6, [||]); Token.Elipsis(6, 9, [| |]) |], 
                                                                            ASTNode.DottedName(9, 19, [| 
                                                                                                            ASTNode.Name(9, 13, Token.Name(9, 13, "test", [| |])); 
                                                                                                            ASTNode.Name(14, 19, Token.Name(14, 18, "tell", [| |]))      
                                                                                                      |], [| Token.Dot(13, 14, [| |]) |]), 
                                                                            Token.Import(19, 25, [| |]), 
                                                                                Token.LeftParen(26, 27, [||]), 
                                                                                ASTNode.ImportAsNames(28, 35,   [| 
                                                                                                                    ASTNode.ImportAsName(28, 35, ASTNode.Name(28, 30, Token.Name(28, 29, "a", [| |])), Token.As(30, 32, [| |]), ASTNode.Name(33, 35, Token.Name(33, 34, "b", [||])))
                                                                                                                |], [| |]), 
                                                                                Token.RightParen(35, 36, [| |] )  )
                                                    |], 
                                                    [| |], Token.Newline(37, 39, [| |])), parser.ParseStmt())

    [<Fact>]
    let ``import from seventh test`` () =
        let lex = new MockTokenizer( [ ( Token.From(0, 4, [| |]), 0 ); ( Token.Dot(5, 6, [| |]), 5 ); ( Token.Elipsis(6, 9, [| |]), 6 ); ( Token.Name(9, 13, "test", [| |]), 9 ); ( Token.Dot(13, 14, [| |]), 13 ); ( Token.Name(14, 18, "tell", [| |]), 14 ); ( Token.Import(19, 25, [| |]), 19 ); ( Token.LeftParen(26, 27, [| |]), 26 ); ( Token.Name(28, 29, "a", [| |]), 28 ); ( Token.As(30, 32, [| |]), 30 ); ( Token.Name(33, 34, "b", [| |]), 33); 
                                        ( Token.Comma(34, 35, [| |]), 34 ); ( Token.Name(35, 36, "c", [| |]), 35); ( Token.RightParen(37, 38, [| |]), 37 ); ( Token.Newline(39, 41, [| |]), 39 ); ( Token.EOF([| |]), 42 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 42, [|
                                                          ASTNode.ImportFrom(0, 39, Token.From(0, 4, [||]), [| Token.Dot(5, 6, [||]); Token.Elipsis(6, 9, [| |]) |], 
                                                                            ASTNode.DottedName(9, 19, [| 
                                                                                                            ASTNode.Name(9, 13, Token.Name(9, 13, "test", [| |])); 
                                                                                                            ASTNode.Name(14, 19, Token.Name(14, 18, "tell", [| |]))      
                                                                                                      |], [| Token.Dot(13, 14, [| |]) |]), 
                                                                            Token.Import(19, 25, [| |]), 
                                                                                Token.LeftParen(26, 27, [||]), 
                                                                                ASTNode.ImportAsNames(28, 37,   [| 
                                                                                                                    ASTNode.ImportAsName(28, 34, ASTNode.Name(28, 30, Token.Name(28, 29, "a", [| |])), Token.As(30, 32, [| |]), ASTNode.Name(33, 34, Token.Name(33, 34, "b", [||])));
                                                                                                                    ASTNode.ImportAsName(35, 37, ASTNode.Name(35, 37, Token.Name(35, 36, "c", [| |])), Token.Empty, ASTNode.Empty)
                                                                                                                |], [| Token.Comma(34, 35, [| |]) |]), 
                                                                                Token.RightParen(37, 38, [| |] )  )
                                                    |], 
                                                    [| |], Token.Newline(39, 41, [| |])), parser.ParseStmt())

    [<Fact>]
    let ``import from eight test`` () =
        let lex = new MockTokenizer( [ ( Token.From(0, 4, [| |]), 0 ); ( Token.Dot(5, 6, [| |]), 5 ); ( Token.Elipsis(6, 9, [| |]), 6 ); ( Token.Name(9, 13, "test", [| |]), 9 ); ( Token.Dot(13, 14, [| |]), 13 ); ( Token.Name(14, 18, "tell", [| |]), 14 ); ( Token.Import(19, 25, [| |]), 19 ); ( Token.Name(28, 29, "a", [| |]), 28 ); ( Token.As(30, 32, [| |]), 30 ); ( Token.Name(33, 34, "b", [| |]), 33); 
                                        ( Token.Comma(34, 35, [| |]), 34 ); ( Token.Name(35, 36, "c", [| |]), 35); ( Token.Newline(39, 41, [| |]), 39 ); ( Token.EOF([| |]), 42 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 42, [|
                                                          ASTNode.ImportFrom(0, 39, Token.From(0, 4, [||]), [| Token.Dot(5, 6, [||]); Token.Elipsis(6, 9, [| |]) |], 
                                                                            ASTNode.DottedName(9, 19, [| 
                                                                                                            ASTNode.Name(9, 13, Token.Name(9, 13, "test", [| |])); 
                                                                                                            ASTNode.Name(14, 19, Token.Name(14, 18, "tell", [| |]))      
                                                                                                      |], [| Token.Dot(13, 14, [| |]) |]), 
                                                                            Token.Import(19, 25, [| |]), 
                                                                                Token.Empty, 
                                                                                ASTNode.ImportAsNames(28, 39,   [| 
                                                                                                                    ASTNode.ImportAsName(28, 34, ASTNode.Name(28, 30, Token.Name(28, 29, "a", [| |])), Token.As(30, 32, [| |]), ASTNode.Name(33, 34, Token.Name(33, 34, "b", [||])));
                                                                                                                    ASTNode.ImportAsName(35, 39, ASTNode.Name(35, 39, Token.Name(35, 36, "c", [| |])), Token.Empty, ASTNode.Empty)
                                                                                                                |], [| Token.Comma(34, 35, [| |]) |]), 
                                                                                Token.Empty )
                                                    |], 
                                                    [| |], Token.Newline(39, 41, [| |])), parser.ParseStmt())

    [<Fact>]
    let ``+= test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.PlusAssign(6, 8, [| |]), 6 ); ( Token.Number(9, 10, "1", [| |]), 9 ); ( Token.Newline(11, 13, [| |]), 11 ); ( Token.EOF([| |]), 14 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 14, [|
                                                        ASTNode.PlusAssign(0, 11,   ASTNode.TestList(0, 6, [| ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])) |], [| |]), 
                                                                                    Token.PlusAssign(6, 8, [| |]), 
                                                                                    ASTNode.TestList(9, 11, [| ASTNode.Number(9, 11, Token.Number(9, 10, "1", [| |]))   |], [| |])
                                                                            )
                                                    |], [| |], Token.Newline(11, 13, [| |])), parser.ParseStmt() )

    [<Fact>]
    let ``-= test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.MinusAssign(6, 8, [| |]), 6 ); ( Token.Number(9, 10, "1", [| |]), 9 ); ( Token.Newline(11, 13, [| |]), 11 ); ( Token.EOF([| |]), 14 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 14, [|
                                                        ASTNode.MinusAssign(0, 11,   ASTNode.TestList(0, 6, [| ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])) |], [| |]), 
                                                                                    Token.MinusAssign(6, 8, [| |]), 
                                                                                    ASTNode.TestList(9, 11, [| ASTNode.Number(9, 11, Token.Number(9, 10, "1", [| |]))   |], [| |])
                                                                            )
                                                    |], [| |], Token.Newline(11, 13, [| |])), parser.ParseStmt() )

    [<Fact>]
    let ``*= test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.MulAssign(6, 8, [| |]), 6 ); ( Token.Number(9, 10, "1", [| |]), 9 ); ( Token.Newline(11, 13, [| |]), 11 ); ( Token.EOF([| |]), 14 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 14, [|
                                                        ASTNode.MulAssign(0, 11,   ASTNode.TestList(0, 6, [| ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])) |], [| |]), 
                                                                                    Token.MulAssign(6, 8, [| |]), 
                                                                                    ASTNode.TestList(9, 11, [| ASTNode.Number(9, 11, Token.Number(9, 10, "1", [| |]))   |], [| |])
                                                                            )
                                                    |], [| |], Token.Newline(11, 13, [| |])), parser.ParseStmt() )

    [<Fact>]
    let ``/= test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.DivAssign(6, 8, [| |]), 6 ); ( Token.Number(9, 10, "1", [| |]), 9 ); ( Token.Newline(11, 13, [| |]), 11 ); ( Token.EOF([| |]), 14 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 14, [|
                                                        ASTNode.DivAssign(0, 11,   ASTNode.TestList(0, 6, [| ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])) |], [| |]), 
                                                                                    Token.DivAssign(6, 8, [| |]), 
                                                                                    ASTNode.TestList(9, 11, [| ASTNode.Number(9, 11, Token.Number(9, 10, "1", [| |]))   |], [| |])
                                                                            )
                                                    |], [| |], Token.Newline(11, 13, [| |])), parser.ParseStmt() )

    [<Fact>]
    let ``Matrice= test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.MatriceAssign(6, 8, [| |]), 6 ); ( Token.Number(9, 10, "1", [| |]), 9 ); ( Token.Newline(11, 13, [| |]), 11 ); ( Token.EOF([| |]), 14 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 14, [|
                                                        ASTNode.MatriceAssign(0, 11,   ASTNode.TestList(0, 6, [| ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])) |], [| |]), 
                                                                                    Token.MatriceAssign(6, 8, [| |]), 
                                                                                    ASTNode.TestList(9, 11, [| ASTNode.Number(9, 11, Token.Number(9, 10, "1", [| |]))   |], [| |])
                                                                            )
                                                    |], [| |], Token.Newline(11, 13, [| |])), parser.ParseStmt() )

    [<Fact>]
    let ``%= test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.ModuloAssign(6, 8, [| |]), 6 ); ( Token.Number(9, 10, "1", [| |]), 9 ); ( Token.Newline(11, 13, [| |]), 11 ); ( Token.EOF([| |]), 14 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 14, [|
                                                        ASTNode.ModuloAssign(0, 11,   ASTNode.TestList(0, 6, [| ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])) |], [| |]), 
                                                                                    Token.ModuloAssign(6, 8, [| |]), 
                                                                                    ASTNode.TestList(9, 11, [| ASTNode.Number(9, 11, Token.Number(9, 10, "1", [| |]))   |], [| |])
                                                                            )
                                                    |], [| |], Token.Newline(11, 13, [| |])), parser.ParseStmt() )

    [<Fact>]
    let ``&= test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.BitAndAssign(6, 8, [| |]), 6 ); ( Token.Number(9, 10, "1", [| |]), 9 ); ( Token.Newline(11, 13, [| |]), 11 ); ( Token.EOF([| |]), 14 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 14, [|
                                                        ASTNode.AndAssign(0, 11,   ASTNode.TestList(0, 6, [| ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])) |], [| |]), 
                                                                                    Token.BitAndAssign(6, 8, [| |]), 
                                                                                    ASTNode.TestList(9, 11, [| ASTNode.Number(9, 11, Token.Number(9, 10, "1", [| |]))   |], [| |])
                                                                            )
                                                    |], [| |], Token.Newline(11, 13, [| |])), parser.ParseStmt() )

    [<Fact>]
    let ``|= test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.BitOrAssign(6, 8, [| |]), 6 ); ( Token.Number(9, 10, "1", [| |]), 9 ); ( Token.Newline(11, 13, [| |]), 11 ); ( Token.EOF([| |]), 14 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 14, [|
                                                        ASTNode.OrAssign(0, 11,   ASTNode.TestList(0, 6, [| ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])) |], [| |]), 
                                                                                    Token.BitOrAssign(6, 8, [| |]), 
                                                                                    ASTNode.TestList(9, 11, [| ASTNode.Number(9, 11, Token.Number(9, 10, "1", [| |]))   |], [| |])
                                                                            )
                                                    |], [| |], Token.Newline(11, 13, [| |])), parser.ParseStmt() )

    [<Fact>]
    let ``^= test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.BitXorAssign(6, 8, [| |]), 6 ); ( Token.Number(9, 10, "1", [| |]), 9 ); ( Token.Newline(11, 13, [| |]), 11 ); ( Token.EOF([| |]), 14 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 14, [|
                                                        ASTNode.XorAssign(0, 11,   ASTNode.TestList(0, 6, [| ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])) |], [| |]), 
                                                                                    Token.BitXorAssign(6, 8, [| |]), 
                                                                                    ASTNode.TestList(9, 11, [| ASTNode.Number(9, 11, Token.Number(9, 10, "1", [| |]))   |], [| |])
                                                                            )
                                                    |], [| |], Token.Newline(11, 13, [| |])), parser.ParseStmt() )

    [<Fact>]
    let ``**= test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.PowerAssign(6, 8, [| |]), 6 ); ( Token.Number(9, 10, "1", [| |]), 9 ); ( Token.Newline(11, 13, [| |]), 11 ); ( Token.EOF([| |]), 14 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 14, [|
                                                        ASTNode.PowerAssign(0, 11,   ASTNode.TestList(0, 6, [| ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])) |], [| |]), 
                                                                                    Token.PowerAssign(6, 8, [| |]), 
                                                                                    ASTNode.TestList(9, 11, [| ASTNode.Number(9, 11, Token.Number(9, 10, "1", [| |]))   |], [| |])
                                                                            )
                                                    |], [| |], Token.Newline(11, 13, [| |])), parser.ParseStmt() )

    [<Fact>]
    let ``//= test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.FloorDivAssign(6, 8, [| |]), 6 ); ( Token.Number(9, 10, "1", [| |]), 9 ); ( Token.Newline(11, 13, [| |]), 11 ); ( Token.EOF([| |]), 14 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 14, [|
                                                        ASTNode.FloorDivAssign(0, 11,   ASTNode.TestList(0, 6, [| ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])) |], [| |]), 
                                                                                    Token.FloorDivAssign(6, 8, [| |]), 
                                                                                    ASTNode.TestList(9, 11, [| ASTNode.Number(9, 11, Token.Number(9, 10, "1", [| |]))   |], [| |])
                                                                            )
                                                    |], [| |], Token.Newline(11, 13, [| |])), parser.ParseStmt() )

    [<Fact>]
    let ``<<= test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.ShiftLeftAssign(6, 8, [| |]), 6 ); ( Token.Number(9, 10, "1", [| |]), 9 ); ( Token.Newline(11, 13, [| |]), 11 ); ( Token.EOF([| |]), 14 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 14, [|
                                                        ASTNode.ShiftLeftAssign(0, 11,   ASTNode.TestList(0, 6, [| ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])) |], [| |]), 
                                                                                    Token.ShiftLeftAssign(6, 8, [| |]), 
                                                                                    ASTNode.TestList(9, 11, [| ASTNode.Number(9, 11, Token.Number(9, 10, "1", [| |]))   |], [| |])
                                                                            )
                                                    |], [| |], Token.Newline(11, 13, [| |])), parser.ParseStmt() )

    [<Fact>]
    let ``>>= test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.ShiftRightAssign(6, 8, [| |]), 6 ); ( Token.Number(9, 10, "1", [| |]), 9 ); ( Token.Newline(11, 13, [| |]), 11 ); ( Token.EOF([| |]), 14 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 14, [|
                                                        ASTNode.ShiftRightAssign(0, 11,   ASTNode.TestList(0, 6, [| ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])) |], [| |]), 
                                                                                    Token.ShiftRightAssign(6, 8, [| |]), 
                                                                                    ASTNode.TestList(9, 11, [| ASTNode.Number(9, 11, Token.Number(9, 10, "1", [| |]))   |], [| |])
                                                                            )
                                                    |], [| |], Token.Newline(11, 13, [| |])), parser.ParseStmt() )

    [<Fact>]
    let ``>>= yield expr test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.ShiftRightAssign(6, 8, [| |]), 6 ); ( Token.Yield(9, 14, [| |]), 9 ); ( Token.Name(15, 16, "a", [| |]), 15 ); ( Token.Newline(16, 18, [| |]), 16 ); ( Token.EOF([| |]), 19 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 19, [|
                                                        ASTNode.ShiftRightAssign(0, 16,   ASTNode.TestList(0, 6, [| ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])) |], [| |]), 
                                                                                    Token.ShiftRightAssign(6, 8, [| |]), 
                                                                                    ASTNode.YieldExpr(9, 16, Token.Yield(9, 14, [| |]),
                                                                                        ASTNode.TestList(15, 16, [| ASTNode.Name(15, 16, Token.Name(15, 16, "a", [| |]))   |], [| |]) )
                                                                            )
                                                    |], [| |], Token.Newline(16, 18, [| |])), parser.ParseStmt() )

    [<Fact>]
    let ``a : b = yield expr test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Colon(6, 7, [| |]), 6 ); ( Token.Name(8, 9, "a", [| |]), 0 ); ( Token.Assign(10, 11, [| |]), 10 ); ( Token.Yield(12, 17, [| |]), 12 ); ( Token.Name(18, 19, "a", [| |]), 18 ); ( Token.Newline(19, 21, [| |]), 19 ); ( Token.EOF([| |]), 22 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 22, [|
                                                        ASTNode.AnnAssign(0, 19,   ASTNode.TestList(0, 6, [| ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])) |], [| |]), 
                                                                                    Token.Colon(6, 7, [| |]),
                                                                                    ASTNode.Name(0, 10, Token.Name(8, 9, "a", [| |])),
                                                                                    Token.Assign(10, 11, [| |]), 
                                                                                    ASTNode.YieldExpr(12, 19, Token.Yield(12, 17, [| |]),
                                                                                        ASTNode.TestList(18, 19, [| ASTNode.Name(18, 19, Token.Name(18, 19, "a", [| |]))   |], [| |]) )
                                                                            )
                                                    |], [| |], Token.Newline(19, 21, [| |])), parser.ParseStmt() )

    [<Fact>]
    let ``a : b = c test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Colon(6, 7, [| |]), 6 ); ( Token.Name(8, 9, "a", [| |]), 0 ); ( Token.Assign(10, 11, [| |]), 10 ); ( Token.Name(12, 13, "a", [| |]), 12 ); ( Token.Newline(13, 15, [| |]), 13 ); ( Token.EOF([| |]), 16 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 16, [|
                                                        ASTNode.AnnAssign(0, 13,   ASTNode.TestList(0, 6, [| ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])) |], [| |]), 
                                                                                    Token.Colon(6, 7, [| |]),
                                                                                    ASTNode.Name(0, 10, Token.Name(8, 9, "a", [| |])),
                                                                                    Token.Assign(10, 11, [| |]), 
                                                                                    ASTNode.TestList(12, 13, [| ASTNode.Name(12, 13, Token.Name(12, 13, "a", [| |]))   |], [| |]) )
                                                    |], [| |], Token.Newline(13, 15, [| |])), parser.ParseStmt() )

    [<Fact>]
    let ``a = yield expr test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Assign(10, 11, [| |]), 10 ); ( Token.Yield(12, 17, [| |]), 12 ); ( Token.Name(18, 19, "a", [| |]), 18 ); ( Token.Newline(19, 21, [| |]), 19 ); ( Token.EOF([| |]), 22 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 22, [|
                                                        ASTNode.Assign(0, 19,   ASTNode.TestList(0, 10, [| ASTNode.Name(0, 10, Token.Name(0, 5, "Test1", [| |])) |], [| |]), 
                                                                                    Token.Assign(10, 11, [| |]), 
                                                                                    Token.Empty,
                                                                                    ASTNode.YieldExpr(12, 19, Token.Yield(12, 17, [| |]),
                                                                                        ASTNode.TestList(18, 19, [| ASTNode.Name(18, 19, Token.Name(18, 19, "a", [| |]))   |], [| |]) )
                                                                            )
                                                    |], [| |], Token.Newline(19, 21, [| |])), parser.ParseStmt() )

    [<Fact>]
    let ``a = yield expr typecomment test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Assign(10, 11, [| |]), 10 ); ( Token.Yield(12, 17, [| |]), 12 ); ( Token.Name(18, 19, "a", [| |]), 18 ); ( Token.TypeComment(20, 30, "abcdefghik"), 20 ); ( Token.Newline(30, 32, [| |]), 30 ); ( Token.EOF([| |]), 33 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 33, [|
                                                        ASTNode.Assign(0, 30,   ASTNode.TestList(0, 10, [| ASTNode.Name(0, 10, Token.Name(0, 5, "Test1", [| |])) |], [| |]), 
                                                                                    Token.Assign(10, 11, [| |]), 
                                                                                    Token.TypeComment(20, 30, "abcdefghik"),
                                                                                    ASTNode.YieldExpr(12, 20, Token.Yield(12, 17, [| |]),
                                                                                        ASTNode.TestList(18, 20, [| ASTNode.Name(18, 20, Token.Name(18, 19, "a", [| |]))   |], [| |]) )
                                                                            )
                                                    |], [| |], Token.Newline(30, 32, [| |])), parser.ParseStmt() )

    [<Fact>]
    let ``a = b test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Assign(6, 7, [| |]), 6 ); ( Token.Name(8, 9, "a", [| |]), 8 ); ( Token.Newline(9, 11, [| |]), 9 ); ( Token.EOF([| |]), 12 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 12, [|
                                                        ASTNode.Assign(0, 9,    ASTNode.TestList(0, 6, [| ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])) |], [| |]), 
                                                                                Token.Assign(6, 7, [| |]), 
                                                                                Token.Empty,
                                                                                ASTNode.TestList(8, 9, [| ASTNode.Name(8, 9, Token.Name(8, 9, "a", [| |]))   |], [| |]) )
                                                    |], [| |], Token.Newline(9, 11, [| |])), parser.ParseStmt() )

    [<Fact>]
    let ``a = b = c typecomment test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 2, "a", [| |]), 0 ); ( Token.Assign(3, 4, [| |]), 3 ); ( Token.Name(5, 6, "b", [| |]), 5 ); ( Token.Assign(7, 8, [| |]), 7 ); ( Token.Name(8, 9, "c", [| |]), 8 ); ( Token.TypeComment(10, 12, "xa"), 10 ); ( Token.Newline(13, 15, [| |]), 13 ); ( Token.EOF([| |]), 16 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 16, [|
                                                        ASTNode.Assign(0, 13,
                                                                ASTNode.Assign(0, 7, 
                                                                    ASTNode.TestList(0, 3, [| ASTNode.Name(0, 3, Token.Name(0, 2, "a", [| |]))  |], [| |]), 
                                                                    Token.Assign(3, 4, [| |]), 
                                                                    Token.Empty, 
                                                                    ASTNode.TestList(5, 7, [| ASTNode.Name(5, 7, Token.Name(5, 6, "b", [| |])) |], [| |])),
                                                                Token.Assign(7, 8, [| |]),
                                                                Token.TypeComment(10, 12, "xa"),
                                                                ASTNode.TestList(8, 10, [| ASTNode.Name(8, 10, Token.Name(8, 9, "c", [| |])) |], [| |])
                                                                )
                                                    |], [| |], Token.Newline(13, 15, [| |])), parser.ParseStmt() )

    [<Fact>]
    let ``a = b = c test`` () =
        let lex = new MockTokenizer( [ ( Token.Name(0, 2, "a", [| |]), 0 ); ( Token.Assign(3, 4, [| |]), 3 ); ( Token.Name(5, 6, "b", [| |]), 5 ); ( Token.Assign(7, 8, [| |]), 7 ); ( Token.Name(8, 9, "c", [| |]), 8 ); ( Token.Newline(10, 12, [| |]), 10 ); ( Token.EOF([| |]), 13 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 13, [|
                                                        ASTNode.Assign(0, 10,
                                                                ASTNode.Assign(0, 7, 
                                                                    ASTNode.TestList(0, 3, [| ASTNode.Name(0, 3, Token.Name(0, 2, "a", [| |]))  |], [| |]), 
                                                                    Token.Assign(3, 4, [| |]), 
                                                                    Token.Empty, 
                                                                    ASTNode.TestList(5, 7, [| ASTNode.Name(5, 7, Token.Name(5, 6, "b", [| |])) |], [| |])),
                                                                Token.Assign(7, 8, [| |]),
                                                                Token.Empty,
                                                                ASTNode.TestList(8, 10, [| ASTNode.Name(8, 10, Token.Name(8, 9, "c", [| |])) |], [| |])
                                                                )
                                                    |], [| |], Token.Newline(10, 12, [| |])), parser.ParseStmt() )

    [<Fact>]
    let ``pass ; pass test`` () =
        let lex = new MockTokenizer( [ ( Token.Pass(0, 4, [| |]), 0 ); ( Token.SemiColon(4, 5, [| |]), 4 ); ( Token.Pass(6, 10, [| |]), 6 ); ( Token.Newline(12, 14, [| |]), 12 ); ( Token.EOF([| |]), 15 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.SimpleStmtList(0, 15, [|
                                                        ASTNode.Pass(0, 4, Token.Pass(0, 4, [| |]));
                                                        ASTNode.Pass(6, 12, Token.Pass(6, 10, [| |]))
                                                    |], [| Token.SemiColon(4, 5, [| |]) |], Token.Newline(12, 14, [| |])), parser.ParseStmt() )

    [<Fact>]
    let ``if single test`` () =
        let lex = new MockTokenizer( [ ( Token.If(0, 2, [| |]), 0 ); ( Token.True(3, 7, [| |]), 3 ); ( Token.Colon(7, 8, [| |]), 7 ); ( Token.Pass(9, 13, [| |]), 9 ); ( Token.Newline(14, 16, [| |]), 14 ); ( Token.EOF([| |]), 17 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.If(0, 17, Token.If(0, 2, [| |]), 
                                        ASTNode.True(3, 7, Token.True(3, 7, [| |])), 
                                        Token.Colon(7, 8, [| |]), 
                                        ASTNode.SimpleStmtList(9, 17, [| ASTNode.Pass(9, 14, Token.Pass(9, 13, [| |])) |], [| |], Token.Newline(14, 16, [| |])), 
                                        [| |], 
                                        ASTNode.Empty), parser.ParseStmt() )

    [<Fact>]
    let ``if single with namedexpr test`` () =
        let lex = new MockTokenizer( [ ( Token.If(0, 2, [| |]), 0 ); ( Token.Name(3, 8, "Test1", [| |]), 3 ); ( Token.ColonAssign(9, 11, [| |]), 9 ); ( Token.Name(12, 13, "b", [| |]), 12 ); ( Token.Colon(14, 15, [| |]), 14 ); ( Token.Pass(16, 20, [| |]), 16 ); ( Token.Newline(21, 23, [| |]), 21 ); ( Token.EOF([| |]), 24 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.If(0, 24, Token.If(0, 2, [| |]), 
                                        ASTNode.NamedExpr(3, 14, ASTNode.Name(3, 9, Token.Name(3, 8, "Test1", [| |])), Token.ColonAssign(9, 11, [| |]), ASTNode.Name(12, 14, Token.Name(12, 13, "b", [| |]))),
                                        Token.Colon(14, 15, [| |]), 
                                        ASTNode.SimpleStmtList(16, 24, [| ASTNode.Pass(16, 21, Token.Pass(16, 20, [| |])) |], [| |], Token.Newline(21, 23, [| |])), 
                                        [| |], 
                                        ASTNode.Empty), parser.ParseStmt() )

    [<Fact>]
    let ``if single with else test`` () =
        let lex = new MockTokenizer( [ ( Token.If(0, 2, [| |]), 0 ); ( Token.True(3, 7, [| |]), 3 ); ( Token.Colon(7, 8, [| |]), 7 ); ( Token.Pass(9, 13, [| |]), 9 ); ( Token.Newline(14, 16, [| |]), 14 ); 
                                       ( Token.Else(17, 21, [| |]), 17); ( Token.Colon(21, 22, [| |]), 21 ); ( Token.Pass(23, 27, [| |]), 23 ); ( Token.Newline(28, 30, [| |]), 28 ); ( Token.EOF([| |]), 31 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.If(0, 31, Token.If(0, 2, [| |]), 
                                        ASTNode.True(3, 7, Token.True(3, 7, [| |])), 
                                        Token.Colon(7, 8, [| |]), 
                                        ASTNode.SimpleStmtList(9, 17, [| ASTNode.Pass(9, 14, Token.Pass(9, 13, [| |])) |], [| |], Token.Newline(14, 16, [| |])), 
                                        [| |], 
                                        ASTNode.Else(17, 31, 
                                                        Token.Else(17, 21, [||]), 
                                                        Token.Colon(21, 22, [| |]), 
                                                        ASTNode.SimpleStmtList(23, 31, [| 
                                                                                            ASTNode.Pass(23, 28, Token.Pass(23, 27, [| |]))
                                                                                        |], [| |], Token.Newline(28, 30, [| |]))
                                                        )
                                        ), parser.ParseStmt() )

    [<Fact>]
    let ``if with single elif and with else test`` () =
        let lex = new MockTokenizer( [ ( Token.If(0, 2, [| |]), 0 ); ( Token.True(3, 7, [| |]), 3 ); ( Token.Colon(7, 8, [| |]), 7 ); ( Token.Pass(9, 13, [| |]), 9 ); ( Token.Newline(14, 16, [| |]), 14 ); 
                                       ( Token.Elif(17, 21, [| |]), 17 ); ( Token.True(22, 26, [| |]), 22 ); ( Token.Colon(27, 28, [| |]), 27 ); ( Token.Pass(28, 32, [| |]), 28 ); ( Token.Newline(32, 34, [| |]), 32 );
                                       ( Token.Else(35, 39, [| |]), 35); ( Token.Colon(40, 41, [| |]), 40 ); ( Token.Pass(42, 46, [| |]), 42 ); ( Token.Newline(47, 49, [| |]), 47 ); ( Token.EOF([| |]), 50 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.If(0, 50, Token.If(0, 2, [| |]), 
                                        ASTNode.True(3, 7, Token.True(3, 7, [| |])), 
                                        Token.Colon(7, 8, [| |]), 
                                        ASTNode.SimpleStmtList(9, 17, [| ASTNode.Pass(9, 14, Token.Pass(9, 13, [| |])) |], [| |], Token.Newline(14, 16, [| |])), 
                                        [| 
                                            ASTNode.Elif(17, 35, 
                                                                    Token.Elif(17, 21, [| |]),
                                                                    ASTNode.True(22, 27, Token.True(22, 26, [| |])),
                                                                    Token.Colon(27, 28, [| |]),
                                                                    ASTNode.SimpleStmtList(28, 35, [| 
                                                                                                        ASTNode.Pass(28, 32, Token.Pass(28, 32, [| |]))
                                                                                                    |], [| |], Token.Newline(32, 34, [| |]))
                                                                )
                                        |], 
                                        ASTNode.Else(35, 50, 
                                                        Token.Else(35, 39, [||]), 
                                                        Token.Colon(40, 41, [| |]), 
                                                        ASTNode.SimpleStmtList(42, 50, [| 
                                                                                            ASTNode.Pass(42, 47, Token.Pass(42, 46, [| |]))
                                                                                        |], [| |], Token.Newline(47, 49, [| |]))
                                                        )
                                        ), parser.ParseStmt() )

    [<Fact>]
    let ``if with double elif and with else test`` () =
        let lex = new MockTokenizer( [ ( Token.If(0, 2, [| |]), 0 ); ( Token.True(3, 7, [| |]), 3 ); ( Token.Colon(7, 8, [| |]), 7 ); ( Token.Pass(9, 13, [| |]), 9 ); ( Token.Newline(14, 16, [| |]), 14 ); 
                                       ( Token.Elif(17, 21, [| |]), 17 ); ( Token.True(22, 26, [| |]), 22 ); ( Token.Colon(27, 28, [| |]), 27 ); ( Token.Pass(28, 32, [| |]), 28 ); ( Token.Newline(32, 34, [| |]), 32 );
                                       ( Token.Elif(35, 39, [| |]), 35 ); ( Token.True(40, 44, [| |]), 40 ); ( Token.Colon(45, 46, [| |]), 45 ); ( Token.Pass(46, 50, [| |]), 46 ); ( Token.Newline(50, 52, [| |]), 50 );
                                       ( Token.Else(53, 57, [| |]), 53); ( Token.Colon(58, 59, [| |]), 58 ); ( Token.Pass(60, 64, [| |]), 60 ); ( Token.Newline(65, 67, [| |]), 65 ); ( Token.EOF([| |]), 68 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.If(0, 68, Token.If(0, 2, [| |]), 
                                        ASTNode.True(3, 7, Token.True(3, 7, [| |])), 
                                        Token.Colon(7, 8, [| |]), 
                                        ASTNode.SimpleStmtList(9, 17, [| ASTNode.Pass(9, 14, Token.Pass(9, 13, [| |])) |], [| |], Token.Newline(14, 16, [| |])), 
                                        [| 
                                            ASTNode.Elif(17, 35, 
                                                                    Token.Elif(17, 21, [| |]),
                                                                    ASTNode.True(22, 27, Token.True(22, 26, [| |])),
                                                                    Token.Colon(27, 28, [| |]),
                                                                    ASTNode.SimpleStmtList(28, 35, [| 
                                                                                                        ASTNode.Pass(28, 32, Token.Pass(28, 32, [| |]))
                                                                                                    |], [| |], Token.Newline(32, 34, [| |]))
                                                                );
                                            ASTNode.Elif(35, 53, 
                                                                    Token.Elif(35, 39, [| |]),
                                                                    ASTNode.True(40, 45, Token.True(40, 44, [| |])),
                                                                    Token.Colon(45, 46, [| |]),
                                                                    ASTNode.SimpleStmtList(46, 53, [| 
                                                                                                        ASTNode.Pass(46, 50, Token.Pass(46, 50, [| |]))
                                                                                                    |], [| |], Token.Newline(50, 52, [| |]))
                                                                )
                                        |], 
                                        ASTNode.Else(53, 68, 
                                                        Token.Else(53, 57, [||]), 
                                                        Token.Colon(58, 59, [| |]), 
                                                        ASTNode.SimpleStmtList(60, 68, [| 
                                                                                            ASTNode.Pass(60, 65, Token.Pass(60, 64, [| |]))
                                                                                        |], [| |], Token.Newline(65, 67, [| |]))
                                                        )
                                        ), parser.ParseStmt() )

    [<Fact>]
    let ``if with double elif test`` () =
        let lex = new MockTokenizer( [ ( Token.If(0, 2, [| |]), 0 ); ( Token.True(3, 7, [| |]), 3 ); ( Token.Colon(7, 8, [| |]), 7 ); ( Token.Pass(9, 13, [| |]), 9 ); ( Token.Newline(14, 16, [| |]), 14 ); 
                                       ( Token.Elif(17, 21, [| |]), 17 ); ( Token.True(22, 26, [| |]), 22 ); ( Token.Colon(27, 28, [| |]), 27 ); ( Token.Pass(28, 32, [| |]), 28 ); ( Token.Newline(32, 34, [| |]), 32 );
                                       ( Token.Elif(35, 39, [| |]), 35 ); ( Token.True(40, 44, [| |]), 40 ); ( Token.Colon(45, 46, [| |]), 45 ); ( Token.Pass(46, 50, [| |]), 46 ); ( Token.Newline(50, 52, [| |]), 50 );
                                       ( Token.EOF([| |]), 53 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.If(0, 53, Token.If(0, 2, [| |]), 
                                        ASTNode.True(3, 7, Token.True(3, 7, [| |])), 
                                        Token.Colon(7, 8, [| |]), 
                                        ASTNode.SimpleStmtList(9, 17, [| ASTNode.Pass(9, 14, Token.Pass(9, 13, [| |])) |], [| |], Token.Newline(14, 16, [| |])), 
                                        [| 
                                            ASTNode.Elif(17, 35, 
                                                                    Token.Elif(17, 21, [| |]),
                                                                    ASTNode.True(22, 27, Token.True(22, 26, [| |])),
                                                                    Token.Colon(27, 28, [| |]),
                                                                    ASTNode.SimpleStmtList(28, 35, [| 
                                                                                                        ASTNode.Pass(28, 32, Token.Pass(28, 32, [| |]))
                                                                                                    |], [| |], Token.Newline(32, 34, [| |]))
                                                                );
                                            ASTNode.Elif(35, 53, 
                                                                    Token.Elif(35, 39, [| |]),
                                                                    ASTNode.True(40, 45, Token.True(40, 44, [| |])),
                                                                    Token.Colon(45, 46, [| |]),
                                                                    ASTNode.SimpleStmtList(46, 53, [| 
                                                                                                        ASTNode.Pass(46, 50, Token.Pass(46, 50, [| |]))
                                                                                                    |], [| |], Token.Newline(50, 52, [| |]))
                                                                )
                                        |], 
                                        ASTNode.Empty
                                        ), parser.ParseStmt() )

    [<Fact>]
    let ``while statement test`` () =
           let lex = new MockTokenizer( [ ( Token.While(0, 5, [| |]), 0 ); ( Token.Name(6, 10, "Test", [| |]), 6 ); ( Token.ColonAssign(11, 13, [| |]), 11 ); ( Token.Name(14, 18, "Tell", [| |]), 14 ); ( Token.Colon(19, 20, [| |]), 19 ); ( Token.Pass(21, 25, [| |]), 21 ); ( Token.Newline(26, 28, [| |]), 26 ); ( Token.EOF([| |]), 29 ); ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.While(0, 29,   Token.While(0, 5, [| |]), 
                                                ASTNode.NamedExpr(6, 19, ASTNode.Name(6, 11, Token.Name(6, 10, "Test", [| |])), Token.ColonAssign(11, 13, [| |]), ASTNode.Name(14, 19, Token.Name(14, 18, "Tell", [| |]))), 
                                                Token.Colon(19, 20, [| |]), 
                                                ASTNode.SimpleStmtList(21, 29, [| ASTNode.Pass(21, 26, Token.Pass(21, 25, [| |])) |], [| |], Token.Newline(26, 28, [| |])), 
                                                ASTNode.Empty), parser.ParseStmt())

    [<Fact>]
    let ``while statement with else statement test`` () =
        let lex = new MockTokenizer( [ ( Token.While(0, 5, [| |]), 0 ); ( Token.Name(6, 10, "Test", [| |]), 6 ); ( Token.ColonAssign(11, 13, [| |]), 11 ); ( Token.Name(14, 18, "Tell", [| |]), 14 ); ( Token.Colon(19, 20, [| |]), 19 ); ( Token.Pass(21, 25, [| |]), 21 ); ( Token.Newline(26, 28, [| |]), 26 ); ( Token.Else(28, 32, [| |]), 28 ); ( Token.Colon(32, 33, [| |]), 32 ); ( Token.Pass(34, 38, [| |]), 34 ); ( Token.Newline(39, 41, [| |]), 39 ); ( Token.EOF([| |]), 42 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( ASTNode.While(0, 42,   Token.While(0, 5, [| |]), 
                                             ASTNode.NamedExpr(6, 19, ASTNode.Name(6, 11, Token.Name(6, 10, "Test", [| |])), Token.ColonAssign(11, 13, [| |]), ASTNode.Name(14, 19, Token.Name(14, 18, "Tell", [| |]))), 
                                             Token.Colon(19, 20, [| |]), 
                                             ASTNode.SimpleStmtList(21, 28, [| ASTNode.Pass(21, 26, Token.Pass(21, 25, [| |])) |], [| |], Token.Newline(26, 28, [| |])), 
                                             ASTNode.Else(28, 42, 
                                                                    Token.Else(28, 32, [| |]), 
                                                                    Token.Colon(32, 33, [| |]), 
                                                                    ASTNode.SimpleStmtList(34, 42, [| ASTNode.Pass(34, 39, Token.Pass(34, 38, [| |]))  |], [| |], Token.Newline(39, 41, [| |])))), parser.ParseStmt())

    [<Fact>]
    let ``for statement test`` () =
           let lex = new MockTokenizer( [ ( Token.For(0, 3, [| |]), 0 ); ( Token.Name(4, 8, "Test", [| |]), 4 ); ( Token.In(9, 11, [| |]), 9 ); ( Token.Name(12, 16, "Tell", [| |]), 12 ); ( Token.Colon(17, 18, [| |]), 17 ); ( Token.Pass(19, 23, [| |]), 19 ); ( Token.Newline(24, 26, [| |]), 24 ); ( Token.EOF([| |]), 27 ); ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.For(0, 27, Token.For(0, 3, [| |]),
                                            ASTNode.ExprList(4, 9, [| ASTNode.Name(4, 9, Token.Name(4, 8, "Test", [| |])) |], [| |]),
                                            Token.In(9, 11, [| |]),
                                            ASTNode.TestList(12, 17, [| ASTNode.Name(12, 17, Token.Name(12, 16, "Tell", [| |])) |], [| |]),
                                            Token.Colon(17, 18, [| |]),
                                            Token.Empty,
                                            ASTNode.SimpleStmtList(19, 27, [| ASTNode.Pass(19, 24, Token.Pass(19, 23, [| |])) |], [| |], Token.Newline(24, 26, [| |])),
                                            ASTNode.Empty
                                            ), parser.ParseStmt())

    [<Fact>]
    let ``for statement with typecomment test`` () =
           let lex = new MockTokenizer( [ ( Token.For(0, 3, [| |]), 0 ); ( Token.Name(4, 8, "Test", [| |]), 4 ); ( Token.In(9, 11, [| |]), 9 ); ( Token.Name(12, 16, "Tell", [| |]), 12 ); ( Token.Colon(17, 18, [| |]), 17 ); ( Token.TypeComment(19, 26, "comment"), 19 ); ( Token.Pass(27, 31, [| |]), 27 ); ( Token.Newline(32, 34, [| |]), 32 ); ( Token.EOF([| |]), 35 ); ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.For(0, 35, Token.For(0, 3, [| |]),
                                            ASTNode.ExprList(4, 9, [| ASTNode.Name(4, 9, Token.Name(4, 8, "Test", [| |])) |], [| |]),
                                            Token.In(9, 11, [| |]),
                                            ASTNode.TestList(12, 17, [| ASTNode.Name(12, 17, Token.Name(12, 16, "Tell", [| |])) |], [| |]),
                                            Token.Colon(17, 18, [| |]),
                                            Token.TypeComment(19, 26, "comment"),
                                            ASTNode.SimpleStmtList(27, 35, [| ASTNode.Pass(27, 32, Token.Pass(27, 31, [| |])) |], [| |], Token.Newline(32, 34, [| |])),
                                            ASTNode.Empty
                                            ), parser.ParseStmt())

    [<Fact>]
    let ``for statement with else part test`` () =
           let lex = new MockTokenizer( [ ( Token.For(0, 3, [| |]), 0 ); ( Token.Name(4, 8, "Test", [| |]), 4 ); ( Token.In(9, 11, [| |]), 9 ); ( Token.Name(12, 16, "Tell", [| |]), 12 ); ( Token.Colon(17, 18, [| |]), 17 ); ( Token.Pass(19, 23, [| |]), 19 ); ( Token.Newline(24, 26, [| |]), 24 ); 
                                            ( Token.Else(27, 31, [| |]), 27 ); ( Token.Colon(32, 33, [| |]), 32 ); ( Token.Pass(34, 38, [| |]), 34 ); ( Token.Newline(39, 41, [| |]), 39 ); ( Token.EOF([| |]), 42 ); ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.For(0, 42, Token.For(0, 3, [| |]),
                                            ASTNode.ExprList(4, 9, [| ASTNode.Name(4, 9, Token.Name(4, 8, "Test", [| |])) |], [| |]),
                                            Token.In(9, 11, [| |]),
                                            ASTNode.TestList(12, 17, [| ASTNode.Name(12, 17, Token.Name(12, 16, "Tell", [| |])) |], [| |]),
                                            Token.Colon(17, 18, [| |]),
                                            Token.Empty,
                                            ASTNode.SimpleStmtList(19, 27, [| ASTNode.Pass(19, 24, Token.Pass(19, 23, [| |])) |], [| |], Token.Newline(24, 26, [| |])),
                                            ASTNode.Else(27, 42, 
                                                            Token.Else(27, 31, [| |]),
                                                            Token.Colon(32, 33, [| |]),
                                                            ASTNode.SimpleStmtList(34, 42, [| ASTNode.Pass(34, 39, Token.Pass(34, 38, [| |])) |], [| |], Token.Newline(39, 41, [| |]))
                                            )
                                            ), parser.ParseStmt())

    [<Fact>]
    let ``for statement with else part and typecomment test`` () =
           let lex = new MockTokenizer( [ ( Token.For(0, 3, [| |]), 0 ); ( Token.Name(4, 8, "Test", [| |]), 4 ); ( Token.In(9, 11, [| |]), 9 ); ( Token.Name(12, 16, "Tell", [| |]), 12 ); ( Token.Colon(17, 18, [| |]), 17 ); ( Token.TypeComment(19, 26, "comment"), 19 ); ( Token.Pass(27, 31, [| |]), 27 ); ( Token.Newline(32, 34, [| |]), 32 ); 
                                            ( Token.Else(35, 39, [| |]), 35 ); ( Token.Colon(40, 41, [| |]), 40 ); ( Token.Pass(42, 46, [| |]), 42 ); ( Token.Newline(47, 49, [| |]), 47 ); ( Token.EOF([| |]), 50 ); ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.For(0, 50, Token.For(0, 3, [| |]),
                                            ASTNode.ExprList(4, 9, [| ASTNode.Name(4, 9, Token.Name(4, 8, "Test", [| |])) |], [| |]),
                                            Token.In(9, 11, [| |]),
                                            ASTNode.TestList(12, 17, [| ASTNode.Name(12, 17, Token.Name(12, 16, "Tell", [| |])) |], [| |]),
                                            Token.Colon(17, 18, [| |]),
                                            Token.TypeComment(19, 26, "comment"),
                                            ASTNode.SimpleStmtList(27, 35, [| ASTNode.Pass(27, 32, Token.Pass(27, 31, [| |])) |], [| |], Token.Newline(32, 34, [| |])),
                                            ASTNode.Else(35, 50, 
                                                            Token.Else(35, 39, [| |]),
                                                            Token.Colon(40, 41, [| |]),
                                                            ASTNode.SimpleStmtList(42, 50, [| ASTNode.Pass(42, 47, Token.Pass(42, 46, [| |])) |], [| |], Token.Newline(47, 49, [| |]))
                                            )
                                            ), parser.ParseStmt())

    [<Fact>]
    let ``try statement with finally part test`` () =
            let lex = new MockTokenizer( [ ( Token.Try(0, 3, [| |]), 0 ); ( Token.Colon(3, 4, [| |]), 3 ); ( Token.Pass(5, 9, [| |]), 5 ); ( Token.Newline(10, 12, [| |]), 10 );   
                                            ( Token.Finally(13, 20, [| |]), 13 ); ( Token.Colon(21, 22, [| |]), 21 ); ( Token.Pass(23, 27, [| |]), 23 ); ( Token.Newline(28, 30, [| |]), 28 ); ( Token.EOF([| |]), 31 ); ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.Try(0, 31, Token.Try(0, 3, [| |]), Token.Colon(3, 4, [| |]), 
                                                                        ASTNode.SimpleStmtList(5, 13, [| ASTNode.Pass(5, 10, Token.Pass(5, 9, [| |])) |], [| |], Token.Newline(10, 12, [| |])), 
                                                                        [| |], 
                                                                        ASTNode.Empty, 
                                                                        ASTNode.Finally(13, 31, Token.Finally(13, 20, [| |]), Token.Colon(21, 22, [| |]), ASTNode.SimpleStmtList(23, 31, [| ASTNode.Pass(23, 28, Token.Pass(23, 27, [| |])) |], [| |], Token.Newline(28, 30, [| |])))), parser.ParseStmt())

    [<Fact>]
    let ``try statement with one exception and finally part test`` () =
           let lex = new MockTokenizer( [ ( Token.Try(0, 3, [| |]), 0 ); ( Token.Colon(3, 4, [| |]), 3 ); ( Token.Pass(5, 9, [| |]), 5 ); ( Token.Newline(10, 12, [| |]), 10 );   
                                          ( Token.Except(13, 19, [| |]), 13 ); ( Token.Name(20, 21, "a", [| |]), 20 ); ( Token.As(22, 24, [| |]), 22); ( Token.Name(25, 26, "b", [| |]), 25 ); ( Token.Colon(27, 28, [| |]), 27 ); ( Token.Pass(29, 33, [| |]), 29 ); ( Token.Newline(34, 36, [||]), 34 );
                                          ( Token.Finally(37, 44, [| |]), 37 ); ( Token.Colon(45, 46, [| |]), 45 ); ( Token.Pass(47, 51, [| |]), 47 ); ( Token.Newline(52, 54, [| |]), 52 ); ( Token.EOF([| |]), 55 ); ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.Try(0, 55, Token.Try(0, 3, [| |]), Token.Colon(3, 4, [| |]), 
                                                                         ASTNode.SimpleStmtList(5, 13, [| ASTNode.Pass(5, 10, Token.Pass(5, 9, [| |])) |], [| |], Token.Newline(10, 12, [| |])), 
                                                                         [| 
                                                                                ASTNode.Except(13, 37, 
                                                                                                        Token.Except(13, 19, [| |]), 
                                                                                                        ASTNode.Name(20, 22, Token.Name(20, 21, "a", [| |])), 
                                                                                                        Token.As(22, 24, [| |]), 
                                                                                                        ASTNode.Name(25, 27, Token.Name(25, 26, "b", [| |])), 
                                                                                                        Token.Colon(27, 28, [| |]), 
                                                                                                        ASTNode.SimpleStmtList(29, 37, [| ASTNode.Pass(29, 34, Token.Pass(29, 33, [| |])) |], [| |], Token.Newline(34, 36, [| |])) )
                                                                         |], 
                                                                         ASTNode.Empty, 
                                                                         ASTNode.Finally(37, 55, Token.Finally(37, 44, [| |]), Token.Colon(45, 46, [| |]), ASTNode.SimpleStmtList(47, 55, [| ASTNode.Pass(47, 52, Token.Pass(47, 51, [| |])) |], [| |], Token.Newline(52, 54, [| |])))), parser.ParseStmt())

    [<Fact>]
    let ``try statement with two exception and finally part test`` () =
           let lex = new MockTokenizer( [ ( Token.Try(0, 3, [| |]), 0 ); ( Token.Colon(3, 4, [| |]), 3 ); ( Token.Pass(5, 9, [| |]), 5 ); ( Token.Newline(10, 12, [| |]), 10 );   
                                          ( Token.Except(13, 19, [| |]), 13 ); ( Token.Name(20, 21, "a", [| |]), 20 ); ( Token.As(22, 24, [| |]), 22); ( Token.Name(25, 26, "b", [| |]), 25 ); ( Token.Colon(27, 28, [| |]), 27 ); ( Token.Pass(29, 33, [| |]), 29 ); ( Token.Newline(34, 36, [||]), 34 );
                                          ( Token.Except(37, 43, [| |]), 37 ); ( Token.Name(44, 45, "a", [| |]), 44 ); ( Token.Colon(46, 47, [| |]), 46 ); ( Token.Pass(49, 53, [| |]), 49 ); ( Token.Newline(54, 56, [||]), 54 );
                                          ( Token.Finally(57, 64, [| |]), 57 ); ( Token.Colon(65, 66, [| |]), 65 ); ( Token.Pass(67, 71, [| |]), 67 ); ( Token.Newline(72, 74, [| |]), 72 ); ( Token.EOF([| |]), 75 ); ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.Try(0, 75, Token.Try(0, 3, [| |]), Token.Colon(3, 4, [| |]), 
                                                                         ASTNode.SimpleStmtList(5, 13, [| ASTNode.Pass(5, 10, Token.Pass(5, 9, [| |])) |], [| |], Token.Newline(10, 12, [| |])), 
                                                                         [| 
                                                                                ASTNode.Except(13, 37, 
                                                                                                        Token.Except(13, 19, [| |]), 
                                                                                                        ASTNode.Name(20, 22, Token.Name(20, 21, "a", [| |])), 
                                                                                                        Token.As(22, 24, [| |]), 
                                                                                                        ASTNode.Name(25, 27, Token.Name(25, 26, "b", [| |])), 
                                                                                                        Token.Colon(27, 28, [| |]), 
                                                                                                        ASTNode.SimpleStmtList(29, 37, [| ASTNode.Pass(29, 34, Token.Pass(29, 33, [| |])) |], [| |], Token.Newline(34, 36, [| |])) );
                                                                                ASTNode.Except(37, 57, 
                                                                                                        Token.Except(37, 43, [| |]), 
                                                                                                        ASTNode.Name(44, 46, Token.Name(44, 45, "a", [| |])), 
                                                                                                        Token.Empty, 
                                                                                                        ASTNode.Empty, 
                                                                                                        Token.Colon(46, 47, [| |]), 
                                                                                                        ASTNode.SimpleStmtList(49, 57, [| ASTNode.Pass(49, 54, Token.Pass(49, 53, [| |])) |], [| |], Token.Newline(54, 56, [| |])) )
                                                                         |], 
                                                                         ASTNode.Empty, 
                                                                         ASTNode.Finally(57, 75, Token.Finally(57, 64, [| |]), Token.Colon(65, 66, [| |]), ASTNode.SimpleStmtList(67, 75, [| ASTNode.Pass(67, 72, Token.Pass(67, 71, [| |])) |], [| |], Token.Newline(72, 74, [| |])))), parser.ParseStmt())

    [<Fact>]
    let ``try statement with three exception and finally part test`` () =
           let lex = new MockTokenizer( [ ( Token.Try(0, 3, [| |]), 0 ); ( Token.Colon(3, 4, [| |]), 3 ); ( Token.Pass(5, 9, [| |]), 5 ); ( Token.Newline(10, 12, [| |]), 10 );   
                                          ( Token.Except(13, 19, [| |]), 13 ); ( Token.Name(20, 21, "a", [| |]), 20 ); ( Token.As(22, 24, [| |]), 22); ( Token.Name(25, 26, "b", [| |]), 25 ); ( Token.Colon(27, 28, [| |]), 27 ); ( Token.Pass(29, 33, [| |]), 29 ); ( Token.Newline(34, 36, [||]), 34 );
                                          ( Token.Except(37, 43, [| |]), 37 ); ( Token.Name(44, 45, "a", [| |]), 44 ); ( Token.Colon(46, 47, [| |]), 46 ); ( Token.Pass(49, 53, [| |]), 49 ); ( Token.Newline(54, 56, [||]), 54 );
                                          ( Token.Except(57, 63, [| |]), 57 ); ( Token.Colon(66, 67, [| |]), 66 ); ( Token.Pass(69, 73, [| |]), 69 ); ( Token.Newline(74, 76, [||]), 74 );
                                          ( Token.Finally(75, 82, [| |]), 75 ); ( Token.Colon(83, 84, [| |]), 83 ); ( Token.Pass(86, 90, [| |]), 86 ); ( Token.Newline(92, 94, [| |]), 92 ); ( Token.EOF([| |]), 95 ); ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.Try(0, 95, Token.Try(0, 3, [| |]), Token.Colon(3, 4, [| |]), 
                                                                         ASTNode.SimpleStmtList(5, 13, [| ASTNode.Pass(5, 10, Token.Pass(5, 9, [| |])) |], [| |], Token.Newline(10, 12, [| |])), 
                                                                         [| 
                                                                                ASTNode.Except(13, 37, 
                                                                                                        Token.Except(13, 19, [| |]), 
                                                                                                        ASTNode.Name(20, 22, Token.Name(20, 21, "a", [| |])), 
                                                                                                        Token.As(22, 24, [| |]), 
                                                                                                        ASTNode.Name(25, 27, Token.Name(25, 26, "b", [| |])), 
                                                                                                        Token.Colon(27, 28, [| |]), 
                                                                                                        ASTNode.SimpleStmtList(29, 37, [| ASTNode.Pass(29, 34, Token.Pass(29, 33, [| |])) |], [| |], Token.Newline(34, 36, [| |])) );
                                                                                ASTNode.Except(37, 57, 
                                                                                                        Token.Except(37, 43, [| |]), 
                                                                                                        ASTNode.Name(44, 46, Token.Name(44, 45, "a", [| |])), 
                                                                                                        Token.Empty, 
                                                                                                        ASTNode.Empty, 
                                                                                                        Token.Colon(46, 47, [| |]), 
                                                                                                        ASTNode.SimpleStmtList(49, 57, [| ASTNode.Pass(49, 54, Token.Pass(49, 53, [| |])) |], [| |], Token.Newline(54, 56, [| |])) );
                                                                                ASTNode.Except(57, 75, 
                                                                                                        Token.Except(57, 63, [| |]), 
                                                                                                        ASTNode.Empty, 
                                                                                                        Token.Empty, 
                                                                                                        ASTNode.Empty, 
                                                                                                        Token.Colon(66, 67, [| |]), 
                                                                                                        ASTNode.SimpleStmtList(69, 75, [| ASTNode.Pass(69, 74, Token.Pass(69, 73, [| |])) |], [| |], Token.Newline(74, 76, [| |])) )
                                                                         |], 
                                                                         ASTNode.Empty, 
                                                                         ASTNode.Finally(75, 95, Token.Finally(75, 82, [| |]), Token.Colon(83, 84, [| |]), ASTNode.SimpleStmtList(86, 95, [| ASTNode.Pass(86, 92, Token.Pass(86, 90, [| |])) |], [| |], Token.Newline(92, 94, [| |])))), parser.ParseStmt())

    [<Fact>]
    let ``try statement with three exception and else and finally part test`` () =
           let lex = new MockTokenizer( [ ( Token.Try(0, 3, [| |]), 0 ); ( Token.Colon(3, 4, [| |]), 3 ); ( Token.Pass(5, 9, [| |]), 5 ); ( Token.Newline(10, 12, [| |]), 10 );   
                                          ( Token.Except(13, 19, [| |]), 13 ); ( Token.Name(20, 21, "a", [| |]), 20 ); ( Token.As(22, 24, [| |]), 22); ( Token.Name(25, 26, "b", [| |]), 25 ); ( Token.Colon(27, 28, [| |]), 27 ); ( Token.Pass(29, 33, [| |]), 29 ); ( Token.Newline(34, 36, [||]), 34 );
                                          ( Token.Except(37, 43, [| |]), 37 ); ( Token.Name(44, 45, "a", [| |]), 44 ); ( Token.Colon(46, 47, [| |]), 46 ); ( Token.Pass(49, 53, [| |]), 49 ); ( Token.Newline(54, 56, [||]), 54 );
                                          ( Token.Except(57, 63, [| |]), 57 ); ( Token.Colon(66, 67, [| |]), 66 ); ( Token.Pass(69, 73, [| |]), 69 ); ( Token.Newline(74, 76, [||]), 74 );
                                          ( Token.Else(75, 79, [| |]), 75 ); ( Token.Colon(79, 80, [| |]), 79); ( Token.Pass(81, 85, [| |]), 81 ); ( Token.Newline(86, 88, [| |]), 86 );
                                          ( Token.Finally(89, 96, [| |]), 89 ); ( Token.Colon(97, 98, [| |]), 97 ); ( Token.Pass(98, 102, [| |]), 98 ); ( Token.Newline(103, 105, [| |]), 103 ); ( Token.EOF([| |]), 106 ); ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.Try(0, 106, Token.Try(0, 3, [| |]), Token.Colon(3, 4, [| |]), 
                                                                         ASTNode.SimpleStmtList(5, 13, [| ASTNode.Pass(5, 10, Token.Pass(5, 9, [| |])) |], [| |], Token.Newline(10, 12, [| |])), 
                                                                         [| 
                                                                                ASTNode.Except(13, 37, 
                                                                                                        Token.Except(13, 19, [| |]), 
                                                                                                        ASTNode.Name(20, 22, Token.Name(20, 21, "a", [| |])), 
                                                                                                        Token.As(22, 24, [| |]), 
                                                                                                        ASTNode.Name(25, 27, Token.Name(25, 26, "b", [| |])), 
                                                                                                        Token.Colon(27, 28, [| |]), 
                                                                                                        ASTNode.SimpleStmtList(29, 37, [| ASTNode.Pass(29, 34, Token.Pass(29, 33, [| |])) |], [| |], Token.Newline(34, 36, [| |])) );
                                                                                ASTNode.Except(37, 57, 
                                                                                                        Token.Except(37, 43, [| |]), 
                                                                                                        ASTNode.Name(44, 46, Token.Name(44, 45, "a", [| |])), 
                                                                                                        Token.Empty, 
                                                                                                        ASTNode.Empty, 
                                                                                                        Token.Colon(46, 47, [| |]), 
                                                                                                        ASTNode.SimpleStmtList(49, 57, [| ASTNode.Pass(49, 54, Token.Pass(49, 53, [| |])) |], [| |], Token.Newline(54, 56, [| |])) );
                                                                                ASTNode.Except(57, 75, 
                                                                                                        Token.Except(57, 63, [| |]), 
                                                                                                        ASTNode.Empty, 
                                                                                                        Token.Empty, 
                                                                                                        ASTNode.Empty, 
                                                                                                        Token.Colon(66, 67, [| |]), 
                                                                                                        ASTNode.SimpleStmtList(69, 75, [| ASTNode.Pass(69, 74, Token.Pass(69, 73, [| |])) |], [| |], Token.Newline(74, 76, [| |])) )
                                                                         |], 
                                                                         ASTNode.Else(75, 89, 
                                                                                                Token.Else(75, 79, [| |]), 
                                                                                                Token.Colon(79, 80, [| |]), 
                                                                                                ASTNode.SimpleStmtList(81, 89, [| ASTNode.Pass(81, 86, Token.Pass(81, 85, [| |])) |] , [| |], Token.Newline(86, 88, [| |]) ) 
                                                                                                ), 
                                                                         ASTNode.Finally(89, 106, Token.Finally(89, 96, [| |]), Token.Colon(97, 98, [| |]), ASTNode.SimpleStmtList(98, 106, [| ASTNode.Pass(98, 103, Token.Pass(98, 102, [| |])) |], [| |], Token.Newline(103, 105, [| |])))), parser.ParseStmt())
    
    [<Fact>]
    let ``try statement with three exception and else part test`` () =
           let lex = new MockTokenizer( [ ( Token.Try(0, 3, [| |]), 0 ); ( Token.Colon(3, 4, [| |]), 3 ); ( Token.Pass(5, 9, [| |]), 5 ); ( Token.Newline(10, 12, [| |]), 10 );   
                                          ( Token.Except(13, 19, [| |]), 13 ); ( Token.Name(20, 21, "a", [| |]), 20 ); ( Token.As(22, 24, [| |]), 22); ( Token.Name(25, 26, "b", [| |]), 25 ); ( Token.Colon(27, 28, [| |]), 27 ); ( Token.Pass(29, 33, [| |]), 29 ); ( Token.Newline(34, 36, [||]), 34 );
                                          ( Token.Except(37, 43, [| |]), 37 ); ( Token.Name(44, 45, "a", [| |]), 44 ); ( Token.Colon(46, 47, [| |]), 46 ); ( Token.Pass(49, 53, [| |]), 49 ); ( Token.Newline(54, 56, [||]), 54 );
                                          ( Token.Except(57, 63, [| |]), 57 ); ( Token.Colon(66, 67, [| |]), 66 ); ( Token.Pass(69, 73, [| |]), 69 ); ( Token.Newline(74, 76, [||]), 74 );
                                          ( Token.Else(75, 79, [| |]), 75 ); ( Token.Colon(79, 80, [| |]), 79); ( Token.Pass(81, 85, [| |]), 81 ); ( Token.Newline(86, 88, [| |]), 86 );
                                          ( Token.EOF([| |]), 89 ); ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.Try(0, 89, Token.Try(0, 3, [| |]), Token.Colon(3, 4, [| |]), 
                                                                         ASTNode.SimpleStmtList(5, 13, [| ASTNode.Pass(5, 10, Token.Pass(5, 9, [| |])) |], [| |], Token.Newline(10, 12, [| |])), 
                                                                         [| 
                                                                                ASTNode.Except(13, 37, 
                                                                                                        Token.Except(13, 19, [| |]), 
                                                                                                        ASTNode.Name(20, 22, Token.Name(20, 21, "a", [| |])), 
                                                                                                        Token.As(22, 24, [| |]), 
                                                                                                        ASTNode.Name(25, 27, Token.Name(25, 26, "b", [| |])), 
                                                                                                        Token.Colon(27, 28, [| |]), 
                                                                                                        ASTNode.SimpleStmtList(29, 37, [| ASTNode.Pass(29, 34, Token.Pass(29, 33, [| |])) |], [| |], Token.Newline(34, 36, [| |])) );
                                                                                ASTNode.Except(37, 57, 
                                                                                                        Token.Except(37, 43, [| |]), 
                                                                                                        ASTNode.Name(44, 46, Token.Name(44, 45, "a", [| |])), 
                                                                                                        Token.Empty, 
                                                                                                        ASTNode.Empty, 
                                                                                                        Token.Colon(46, 47, [| |]), 
                                                                                                        ASTNode.SimpleStmtList(49, 57, [| ASTNode.Pass(49, 54, Token.Pass(49, 53, [| |])) |], [| |], Token.Newline(54, 56, [| |])) );
                                                                                ASTNode.Except(57, 75, 
                                                                                                        Token.Except(57, 63, [| |]), 
                                                                                                        ASTNode.Empty, 
                                                                                                        Token.Empty, 
                                                                                                        ASTNode.Empty, 
                                                                                                        Token.Colon(66, 67, [| |]), 
                                                                                                        ASTNode.SimpleStmtList(69, 75, [| ASTNode.Pass(69, 74, Token.Pass(69, 73, [| |])) |], [| |], Token.Newline(74, 76, [| |])) )
                                                                         |], 
                                                                         ASTNode.Else(75, 89, 
                                                                                                Token.Else(75, 79, [| |]), 
                                                                                                Token.Colon(79, 80, [| |]), 
                                                                                                ASTNode.SimpleStmtList(81, 89, [| ASTNode.Pass(81, 86, Token.Pass(81, 85, [| |])) |] , [| |], Token.Newline(86, 88, [| |]) ) 
                                                                                                ), 
                                                                         ASTNode.Empty), parser.ParseStmt())

    [<Fact>]
    let ``try statement with three exception test`` () =
           let lex = new MockTokenizer( [ ( Token.Try(0, 3, [| |]), 0 ); ( Token.Colon(3, 4, [| |]), 3 ); ( Token.Pass(5, 9, [| |]), 5 ); ( Token.Newline(10, 12, [| |]), 10 );   
                                          ( Token.Except(13, 19, [| |]), 13 ); ( Token.Name(20, 21, "a", [| |]), 20 ); ( Token.As(22, 24, [| |]), 22); ( Token.Name(25, 26, "b", [| |]), 25 ); ( Token.Colon(27, 28, [| |]), 27 ); ( Token.Pass(29, 33, [| |]), 29 ); ( Token.Newline(34, 36, [||]), 34 );
                                          ( Token.Except(37, 43, [| |]), 37 ); ( Token.Name(44, 45, "a", [| |]), 44 ); ( Token.Colon(46, 47, [| |]), 46 ); ( Token.Pass(49, 53, [| |]), 49 ); ( Token.Newline(54, 56, [||]), 54 );
                                          ( Token.Except(57, 63, [| |]), 57 ); ( Token.Colon(66, 67, [| |]), 66 ); ( Token.Pass(69, 73, [| |]), 69 ); ( Token.Newline(74, 76, [||]), 74 );
                                          ( Token.EOF([| |]), 77 ); ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.Try(0, 77, Token.Try(0, 3, [| |]), Token.Colon(3, 4, [| |]), 
                                                                         ASTNode.SimpleStmtList(5, 13, [| ASTNode.Pass(5, 10, Token.Pass(5, 9, [| |])) |], [| |], Token.Newline(10, 12, [| |])), 
                                                                         [| 
                                                                                ASTNode.Except(13, 37, 
                                                                                                        Token.Except(13, 19, [| |]), 
                                                                                                        ASTNode.Name(20, 22, Token.Name(20, 21, "a", [| |])), 
                                                                                                        Token.As(22, 24, [| |]), 
                                                                                                        ASTNode.Name(25, 27, Token.Name(25, 26, "b", [| |])), 
                                                                                                        Token.Colon(27, 28, [| |]), 
                                                                                                        ASTNode.SimpleStmtList(29, 37, [| ASTNode.Pass(29, 34, Token.Pass(29, 33, [| |])) |], [| |], Token.Newline(34, 36, [| |])) );
                                                                                ASTNode.Except(37, 57, 
                                                                                                        Token.Except(37, 43, [| |]), 
                                                                                                        ASTNode.Name(44, 46, Token.Name(44, 45, "a", [| |])), 
                                                                                                        Token.Empty, 
                                                                                                        ASTNode.Empty, 
                                                                                                        Token.Colon(46, 47, [| |]), 
                                                                                                        ASTNode.SimpleStmtList(49, 57, [| ASTNode.Pass(49, 54, Token.Pass(49, 53, [| |])) |], [| |], Token.Newline(54, 56, [| |])) );
                                                                                ASTNode.Except(57, 77, 
                                                                                                        Token.Except(57, 63, [| |]), 
                                                                                                        ASTNode.Empty, 
                                                                                                        Token.Empty, 
                                                                                                        ASTNode.Empty, 
                                                                                                        Token.Colon(66, 67, [| |]), 
                                                                                                        ASTNode.SimpleStmtList(69, 77, [| ASTNode.Pass(69, 74, Token.Pass(69, 73, [| |])) |], [| |], Token.Newline(74, 76, [| |])) )
                                                                         |], 
                                                                         ASTNode.Empty, 
                                                                         ASTNode.Empty), parser.ParseStmt())

    [<Fact>]
    let ``With statement with only one expression test`` () =
           let lex = new MockTokenizer( [ ( Token.With(0, 4, [| |]), 0 ); 
                                            ( Token.Name(5, 6, "a", [| |]), 5 );
                                            ( Token.Colon(7, 8, [| |]), 7 ); ( Token.Pass(10, 14, [| |]), 10 ); ( Token.Newline(15, 17, [| |]), 15 ); ( Token.EOF([| |]), 18 ); ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.With(0, 18, 
                                                Token.With(0, 4, [| |]), 
                                                [| ASTNode.WithItem(5, 7, ASTNode.Name(5, 7, Token.Name(5, 6, "a", [| |])), Token.Empty, ASTNode.Empty) |], 
                                                [| |], 
                                                Token.Colon(7, 8, [| |]), 
                                                Token.Empty, 
                                                ASTNode.SimpleStmtList(10, 18, [| ASTNode.Pass(10, 15, Token.Pass(10, 14, [| |])) |], [| |], Token.Newline(15, 17, [| |])) ), parser.ParseStmt()) 

    [<Fact>]
    let ``With statement with only one expression and type comment test`` () =
           let lex = new MockTokenizer( [ ( Token.With(0, 4, [| |]), 0 ); 
                                            ( Token.Name(5, 6, "a", [| |]), 5 );
                                            ( Token.Colon(7, 8, [| |]), 7 ); ( Token.TypeComment(10, 20, "type:   int"), 10 ); ( Token.Pass(20, 24, [| |]), 20 ); ( Token.Newline(25, 27, [| |]), 25 ); ( Token.EOF([| |]), 28 ); ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.With(0, 28, 
                                                Token.With(0, 4, [| |]), 
                                                [| ASTNode.WithItem(5, 7, ASTNode.Name(5, 7, Token.Name(5, 6, "a", [| |])), Token.Empty, ASTNode.Empty) |], 
                                                [| |], 
                                                Token.Colon(7, 8, [| |]), 
                                                Token.TypeComment(10, 20, "type:   int"), 
                                                ASTNode.SimpleStmtList(20, 28, [| ASTNode.Pass(20, 25, Token.Pass(20, 24, [| |])) |], [| |], Token.Newline(25, 27, [| |])) ), parser.ParseStmt()) 

    [<Fact>]
    let ``With statement with only one expression with as test`` () =
           let lex = new MockTokenizer( [ ( Token.With(0, 4, [| |]), 0 ); 
                                            ( Token.Name(5, 6, "a", [| |]), 5 ); ( Token.As(7, 9, [| |]), 7 ); ( Token.Name(10, 11, "b",  [| |]), 10 );
                                            ( Token.Colon(11, 12, [| |]), 11 ); ( Token.Pass(14, 18, [| |]), 14 ); ( Token.Newline(19, 21, [| |]), 19 ); ( Token.EOF([| |]), 22 ); ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.With(0, 22, 
                                                Token.With(0, 4, [| |]), 
                                                [| ASTNode.WithItem(5, 11, ASTNode.Name(5, 7, Token.Name(5, 6, "a", [| |])), Token.As(7, 9, [| |]), ASTNode.Name(10, 11, Token.Name(10, 11, "b", [| |]))) |], 
                                                [| |], 
                                                Token.Colon(11, 12, [| |]), 
                                                Token.Empty, 
                                                ASTNode.SimpleStmtList(14, 22, [| ASTNode.Pass(14, 19, Token.Pass(14, 18, [| |])) |], [| |], Token.Newline(19, 21, [| |])) ), parser.ParseStmt()) 

    [<Fact>]
    let ``With statement with two expression with as test`` () =
           let lex = new MockTokenizer( [ ( Token.With(0, 4, [| |]), 0 ); 
                                            ( Token.Name(5, 6, "a", [| |]), 5 ); ( Token.As(7, 9, [| |]), 7 ); ( Token.Name(10, 11, "b",  [| |]), 10 );
                                            ( Token.Comma(12, 13, [| |]), 12 ); ( Token.Name(14, 15, "c", [| |]), 14);
                                            ( Token.Colon(16, 17, [| |]), 16 ); ( Token.Pass(19, 23, [| |]), 19 ); ( Token.Newline(24, 26, [| |]), 24 ); ( Token.EOF([| |]), 27 ); ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.With(0, 27, 
                                                Token.With(0, 4, [| |]), 
                                                [| 
                                                    ASTNode.WithItem(5, 12, ASTNode.Name(5, 7, Token.Name(5, 6, "a", [| |])), Token.As(7, 9, [| |]), ASTNode.Name(10, 12, Token.Name(10, 11, "b", [| |])));
                                                    ASTNode.WithItem(14, 16, ASTNode.Name(14, 16, Token.Name(14, 15, "c", [| |])), Token.Empty, ASTNode.Empty)
                                                |], 
                                                [| Token.Comma(12, 13, [| |]) |], 
                                                Token.Colon(16, 17, [| |]), 
                                                Token.Empty, 
                                                ASTNode.SimpleStmtList(19, 27, [| ASTNode.Pass(19, 24, Token.Pass(19, 23, [| |])) |], [| |], Token.Newline(24, 26, [| |])) ), parser.ParseStmt())
                                                
    [<Fact>]
    let ``async With statement with two expression with as test`` () =
           let lex = new MockTokenizer( [   ( Token.Async(0, 5, [| |]), 0 ); ( Token.With(6, 10, [| |]), 6 ); 
                                            ( Token.Name(11, 12, "a", [| |]), 11 ); ( Token.As(13, 15, [| |]), 13 ); ( Token.Name(16, 17, "b",  [| |]), 16 );
                                            ( Token.Comma(18, 19, [| |]), 18 ); ( Token.Name(20, 21, "c", [| |]), 20);
                                            ( Token.Colon(22, 23, [| |]), 22 ); ( Token.Pass(25, 28, [| |]), 25 ); ( Token.Newline(30, 32, [| |]), 30 ); ( Token.EOF([| |]), 33 ); ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal(    ASTNode.AsyncStmt(0, 33, Token.Async(0, 5, [| |]),
                                                                    ASTNode.With(6, 33,   
                                                                                        Token.With(6, 10, [| |]), 
                                                                                        [| 
                                                                                            ASTNode.WithItem(11, 18, ASTNode.Name(11, 13, Token.Name(11, 12, "a", [| |])), Token.As(13, 15, [| |]), ASTNode.Name(16, 18, Token.Name(16, 17, "b", [| |])));
                                                                                            ASTNode.WithItem(20, 22, ASTNode.Name(20, 22, Token.Name(20, 21, "c", [| |])), Token.Empty, ASTNode.Empty)
                                                                                        |], 
                                                                                        [| Token.Comma(18, 19, [| |]) |], 
                                                                                        Token.Colon(22, 23, [| |]), 
                                                                                        Token.Empty, 
                                                                                        ASTNode.SimpleStmtList(25, 33, [| ASTNode.Pass(25, 30, Token.Pass(25, 28, [| |])) |], [| |], Token.Newline(30, 32, [| |])) ) ), parser.ParseStmt())

    [<Fact>]
    let ``async for statement test`` () =
           let lex = new MockTokenizer( [ ( Token.Async(0, 5, [| |]), 0 );  ( Token.For(6, 9, [| |]), 6 ); ( Token.Name(10, 14, "Test", [| |]), 10 ); ( Token.In(15, 16, [| |]), 15 ); ( Token.Name(18, 22, "Tell", [| |]), 18 ); ( Token.Colon(23, 24, [| |]), 23 ); ( Token.Pass(25, 29, [| |]), 25 ); ( Token.Newline(30, 32, [| |]), 30 ); ( Token.EOF([| |]), 33 ); ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.AsyncStmt(0, 33, Token.Async(0, 5, [| |]),
                                                        ASTNode.For(6, 33,    
                                                                Token.For(6, 9, [| |]),
                                                                ASTNode.ExprList(10, 15, [| ASTNode.Name(10, 15, Token.Name(10, 14, "Test", [| |])) |], [| |]),
                                                                Token.In(15, 16, [| |]),
                                                                ASTNode.TestList(18, 23, [| ASTNode.Name(18, 23, Token.Name(18, 22, "Tell", [| |])) |], [| |]),
                                                                Token.Colon(23, 24, [| |]),
                                                                Token.Empty,
                                                                ASTNode.SimpleStmtList(25, 33, [| ASTNode.Pass(25, 30, Token.Pass(25, 29, [| |])) |], [| |], Token.Newline(30, 32, [| |])),
                                                                ASTNode.Empty
                                                                ) ), parser.ParseStmt())

    [<Fact>]
    let ``suite with single entry test`` () =
           let lex = new MockTokenizer( [ ( Token.Newline(0, 2, [| |]), 0 );  ( Token.Indent([| |]), 3 ); ( Token.Pass(3, 7, [| |]), 3 ); ( Token.Newline(8, 10, [| |]), 8 ); ( Token.Dedent([| |]), 11 ); ( Token.EOF([| |]), 11 ); ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.Suite(0, 11, Token.Newline(0, 2, [| |]), Token.Indent([| |]), 
                                                [| 
                                                    ASTNode.SimpleStmtList(3, 11, 
                                                            [| 
                                                                ASTNode.Pass(3, 8, Token.Pass(3, 7, [| |]))
                                                            |], [| |], Token.Newline(8, 10, [| |]))
                                                |], Token.Dedent([| |])), parser.ParseSuite() )

    [<Fact>]
    let ``suite with double entry test`` () =
           let lex = new MockTokenizer( [ ( Token.Newline(0, 2, [| |]), 0 );  ( Token.Indent([| |]), 3 ); ( Token.Pass(3, 7, [| |]), 3 ); ( Token.SemiColon(8, 9, [| |]), 8 ); ( Token.Pass(10, 14, [| |]), 10 ); ( Token.Newline(15, 17, [| |]), 15 ); ( Token.Dedent([| |]), 18 ); ( Token.EOF([| |]), 18 ); ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.Suite(0, 18, Token.Newline(0, 2, [| |]), Token.Indent([| |]), 
                                                [| 
                                                    ASTNode.SimpleStmtList(3, 18, 
                                                            [| 
                                                                ASTNode.Pass(3, 8, Token.Pass(3, 7, [| |]));
                                                                ASTNode.Pass(10, 15, Token.Pass(10, 14, [| |]))
                                                            |], [| Token.SemiColon(8, 9, [| |]) |], Token.Newline(15, 17, [| |]))
                                                |], Token.Dedent([| |])), parser.ParseSuite() )

    [<Fact>]
    let ``suite with double entry and if stmt test`` () =
           let lex = new MockTokenizer( [   ( Token.Newline(0, 2, [| |]), 0 );  ( Token.Indent([| |]), 3 ); ( Token.Pass(3, 7, [| |]), 3 ); ( Token.SemiColon(8, 9, [| |]), 8 ); ( Token.Pass(10, 14, [| |]), 10 ); ( Token.Newline(15, 17, [| |]), 15 ); 
                                            ( Token.If(17, 19, [| |]), 17 ); (Token.True(20, 24, [| |]), 20); ( Token.Colon(25, 26, [| |]), 25 ); ( Token.Pass(27, 31, [| |]), 27 ); ( Token.Newline(32, 34, [| |]), 32 );
                                            ( Token.Dedent([| |]), 33 ); ( Token.EOF([| |]), 33 ); ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.Suite(0, 33, Token.Newline(0, 2, [| |]), Token.Indent([| |]), 
                                                [| 
                                                    ASTNode.SimpleStmtList(3, 17, 
                                                            [| 
                                                                ASTNode.Pass(3, 8, Token.Pass(3, 7, [| |]));
                                                                ASTNode.Pass(10, 15, Token.Pass(10, 14, [| |]))
                                                            |], [| Token.SemiColon(8, 9, [| |]) |], Token.Newline(15, 17, [| |]));
                                                    ASTNode.If(17, 33, Token.If(17, 19, [| |]),
                                                                        ASTNode.True(20, 25, Token.True(20, 24, [| |])),
                                                                        Token.Colon(25, 26, [| |]),
                                                                        ASTNode.SimpleStmtList(27, 33, [| ASTNode.Pass(27, 32, Token.Pass(27, 31, [| |])) |], [| |], Token.Newline(32, 34, [| |])),
                                                                        [| |],
                                                                        ASTNode.Empty
                                                                        )

                                                |], Token.Dedent([| |])), parser.ParseSuite() )

    [<Fact>]
    let ``class simple test`` () =
           let lex = new MockTokenizer( [   ( Token.Class(0, 5, [| |]), 0 );  ( Token.Name(6, 7, "a", [| |]), 6 ); ( Token.LeftParen(8, 9, [| |]), 8 ); ( Token.RightParen(9, 10, [| |]), 9 ); ( Token.Colon(10, 11, [| |]), 10 ); 
                                            ( Token.Pass(12, 16, [| |]), 12); ( Token.Newline(17, 19, [| |]), 19 ); ( Token.EOF([| |]), 20 ); ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.Class(0, 20, 
                                                Token.Class(0, 5, [| |]),
                                                ASTNode.Name(6, 8, Token.Name(6, 7, "a", [| |])),
                                                Token.LeftParen(8, 9, [| |]),
                                                ASTNode.Empty,
                                                Token.RightParen(9, 10, [| |]),
                                                Token.Colon(10, 11, [| |]),
                                                ASTNode.SimpleStmtList(12, 20, [| ASTNode.Pass(12, 19, Token.Pass(12, 16, [| |])) |], [| |], Token.Newline(17, 19, [| |]))
                                                ), parser.ParseStmt())

    [<Fact>]
    let ``class with parent test`` () =
           let lex = new MockTokenizer( [   ( Token.Class(0, 5, [| |]), 0 );  ( Token.Name(6, 7, "a", [| |]), 6 ); ( Token.LeftParen(8, 9, [| |]), 8 ); ( Token.Name(9, 10, "b", [| |]), 9 ); ( Token.RightParen(10, 11, [| |]), 10 ); ( Token.Colon(11, 12, [| |]), 11 ); 
                                            ( Token.Pass(12, 16, [| |]), 12); ( Token.Newline(17, 19, [| |]), 19 ); ( Token.EOF([| |]), 20 ); ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.Class(0, 20, 
                                                Token.Class(0, 5, [| |]),
                                                ASTNode.Name(6, 8, Token.Name(6, 7, "a", [| |])),
                                                Token.LeftParen(8, 9, [| |]),
                                                ASTNode.ArgumentList(9, 10, [| ASTNode.Argument(9, 10, ASTNode.Name(9, 10, Token.Name(9, 10, "b", [| |])), Token.Empty, ASTNode.Empty) |], [| |]),
                                                Token.RightParen(10, 11, [| |]),
                                                Token.Colon(11, 12, [| |]),
                                                ASTNode.SimpleStmtList(12, 20, [| ASTNode.Pass(12, 19, Token.Pass(12, 16, [| |])) |], [| |], Token.Newline(17, 19, [| |]))
                                                ), parser.ParseStmt())

    [<Fact>]
    let ``simple funct type input test`` () =
           let lex = new MockTokenizer( [   ( Token.LeftParen(0, 1, [| |]), 0); ( Token.RightParen(1, 2, [| |]), 1 ); ( Token.Ptr(2, 4, [| |]), 2 ); ( Token.Name(5, 6, "a", [| |]), 5 ); 
                                            ( Token.Newline(7, 9, [| |]), 7 ); ( Token.EOF([| |]), 10 ); ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.FuncTypeInput(0, 10, 
                                                        ASTNode.FuncType(0, 7, Token.LeftParen(0, 1, [| |]), ASTNode.Empty, Token.RightParen(1, 2, [| |]), Token.Ptr(2, 4, [| |]), ASTNode.Name(5, 7, Token.Name(5, 6, "a", [| |]))), 
                                                        [| Token.Newline(7, 9, [| |]) |], 
                                                        Token.EOF([| |]) ), parser.ParseFuncTypeInput())

    [<Fact>]
    let ``simple funct type input with mul argument test`` () =
           let lex = new MockTokenizer( [   ( Token.LeftParen(0, 1, [| |]), 0); ( Token.Mul(2, 3, [| |]), 2 ); ( Token.Name(3, 4, "b", [| |]), 3 ); ( Token.RightParen(5, 6, [| |]), 5 ); ( Token.Ptr(6, 8, [| |]), 6 ); ( Token.Name(9, 10, "a", [| |]), 9 ); 
                                            ( Token.Newline(11, 13, [| |]), 11 ); ( Token.EOF([| |]), 14 ); ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.FuncTypeInput(0, 14, 
                                                        ASTNode.FuncType(0, 11, 
                                                                Token.LeftParen(0, 1, [| |]), 
                                                                ASTNode.TypeList(2, 5, 
                                                                                        [| 
                                                                                            ASTNode.TypedMul(2, 5, Token.Mul(2, 3, [| |]), ASTNode.Name(3, 5, Token.Name(3, 4, "b", [| |])))
                                                                                        |], [| |]), 
                                                                Token.RightParen(5, 6, [| |]), 
                                                                Token.Ptr(6, 8, [| |]), 
                                                                ASTNode.Name(9, 11, Token.Name(9, 10, "a", [| |]))
                                                        ), 
                                                        [| Token.Newline(11, 13, [| |]) |], 
                                                        Token.EOF([| |]) ), parser.ParseFuncTypeInput())

    [<Fact>]
    let ``simple funct type input with power argument test`` () =
           let lex = new MockTokenizer( [   ( Token.LeftParen(0, 1, [| |]), 0); ( Token.Power(2, 4, [| |]), 2 ); ( Token.Name(4, 5, "b", [| |]), 4 ); ( Token.RightParen(6, 7, [| |]), 6 ); ( Token.Ptr(7, 9, [| |]), 7 ); ( Token.Name(10, 11, "a", [| |]), 10 ); 
                                            ( Token.Newline(12, 14, [| |]), 12 ); ( Token.EOF([| |]), 15 ); ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.FuncTypeInput(0, 15, 
                                                        ASTNode.FuncType(0, 12, 
                                                                Token.LeftParen(0, 1, [| |]), 
                                                                ASTNode.TypeList(2, 6, 
                                                                                        [| 
                                                                                            ASTNode.TypedPower(2, 6, Token.Power(2, 4, [| |]), ASTNode.Name(4, 6, Token.Name(4, 5, "b", [| |])))
                                                                                        |], [| |]), 
                                                                Token.RightParen(6, 7, [| |]), 
                                                                Token.Ptr(7, 9, [| |]), 
                                                                ASTNode.Name(10, 12, Token.Name(10, 11, "a", [| |]))
                                                        ), 
                                                        [| Token.Newline(12, 14, [| |]) |], 
                                                        Token.EOF([| |]) ), parser.ParseFuncTypeInput())

    [<Fact>]
    let ``simple funct type input with one argument test`` () =
           let lex = new MockTokenizer( [   ( Token.LeftParen(0, 1, [| |]), 0); ( Token.Name(4, 5, "b", [| |]), 4 ); ( Token.RightParen(6, 7, [| |]), 6 ); ( Token.Ptr(7, 9, [| |]), 7 ); ( Token.Name(10, 11, "a", [| |]), 10 ); 
                                            ( Token.Newline(12, 14, [| |]), 12 ); ( Token.EOF([| |]), 15 ); ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.FuncTypeInput(0, 15, 
                                                        ASTNode.FuncType(0, 12, 
                                                                Token.LeftParen(0, 1, [| |]), 
                                                                ASTNode.TypeList(4, 6, 
                                                                                        [| 
                                                                                            ASTNode.Name(4, 6, Token.Name(4, 5, "b", [| |]))
                                                                                        |], [| |]), 
                                                                Token.RightParen(6, 7, [| |]), 
                                                                Token.Ptr(7, 9, [| |]), 
                                                                ASTNode.Name(10, 12, Token.Name(10, 11, "a", [| |]))
                                                        ), 
                                                        [| Token.Newline(12, 14, [| |]) |], 
                                                        Token.EOF([| |]) ), parser.ParseFuncTypeInput())

    [<Fact>]
    let ``simple funct type input with two argument test`` () =
           let lex = new MockTokenizer( [   ( Token.LeftParen(0, 1, [| |]), 0); ( Token.Name(4, 5, "b", [| |]), 4 ); ( Token.Comma(5, 6, [| |]), 5 ); ( Token.Name(7, 8, "d", [| |]), 7 );    
                                            ( Token.RightParen(9, 10, [| |]), 9 ); ( Token.Ptr(10, 12, [| |]), 10 ); ( Token.Name(13, 14, "a", [| |]), 13 ); 
                                            ( Token.Newline(15, 17, [| |]), 15 ); ( Token.Newline(18, 21, [| |]), 18 ); ( Token.EOF([| |]), 22 ); ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.FuncTypeInput(0, 22, 
                                                        ASTNode.FuncType(0, 15, 
                                                                Token.LeftParen(0, 1, [| |]), 
                                                                ASTNode.TypeList(4, 9, 
                                                                                        [| 
                                                                                            ASTNode.Name(4, 5, Token.Name(4, 5, "b", [| |]));
                                                                                            ASTNode.Name(7, 9, Token.Name(7, 8, "d", [| |]))
                                                                                        |], [| Token.Comma(5, 6, [| |]) |]), 
                                                                Token.RightParen(9, 10, [| |]), 
                                                                Token.Ptr(10, 12, [| |]), 
                                                                ASTNode.Name(13, 15, Token.Name(13, 14, "a", [| |]))
                                                        ), 
                                                        [| Token.Newline(15, 17, [| |]); Token.Newline(18, 21, [| |]) |], 
                                                        Token.EOF([| |]) ), parser.ParseFuncTypeInput())

    [<Fact>]
    let ``simple funct type input with two argument and power test`` () =
           let lex = new MockTokenizer( [   ( Token.LeftParen(0, 1, [| |]), 0); ( Token.Name(4, 5, "b", [| |]), 4 ); ( Token.Comma(5, 6, [| |]), 5 ); ( Token.Name(7, 8, "d", [| |]), 7 );
                                            ( Token.Comma(8, 9, [| |]), 8 ); ( Token.Power(10, 12, [| |]), 10 ); ( Token.Name(12, 13, "e", [| |]), 12 );
                                            ( Token.RightParen(15, 16, [| |]), 15 ); ( Token.Ptr(16, 18, [| |]), 16 ); ( Token.Name(19, 20, "a", [| |]), 19 ); 
                                            ( Token.EOF([| |]), 21 ); ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.FuncTypeInput(0, 21, 
                                                        ASTNode.FuncType(0, 21, 
                                                                Token.LeftParen(0, 1, [| |]), 
                                                                ASTNode.TypeList(4, 15, 
                                                                                        [| 
                                                                                            ASTNode.Name(4, 5, Token.Name(4, 5, "b", [| |]));
                                                                                            ASTNode.Name(7, 8, Token.Name(7, 8, "d", [| |]));
                                                                                            ASTNode.TypedPower(10, 15, Token.Power(10, 12, [| |]), ASTNode.Name(12, 15, Token.Name(12, 13, "e", [| |])))
                                                                                        |], [| Token.Comma(5, 6, [| |]); Token.Comma(8, 9, [| |]) |]), 
                                                                Token.RightParen(15, 16, [| |]), 
                                                                Token.Ptr(16, 18, [| |]), 
                                                                ASTNode.Name(19, 21, Token.Name(19, 20, "a", [| |]))
                                                        ), 
                                                        [| |], 
                                                        Token.EOF([| |]) ), parser.ParseFuncTypeInput())

    [<Fact>]
    let ``simple funct type input with two argument and mul test`` () =
           let lex = new MockTokenizer( [   ( Token.LeftParen(0, 1, [| |]), 0); ( Token.Name(4, 5, "b", [| |]), 4 ); ( Token.Comma(5, 6, [| |]), 5 ); ( Token.Name(7, 8, "d", [| |]), 7 );
                                            ( Token.Comma(8, 9, [| |]), 8 ); ( Token.Mul(11, 12, [| |]), 11 ); ( Token.Name(12, 13, "e", [| |]), 12 );
                                            ( Token.RightParen(15, 16, [| |]), 15 ); ( Token.Ptr(16, 18, [| |]), 16 ); ( Token.Name(19, 20, "a", [| |]), 19 ); 
                                            ( Token.EOF([| |]), 21 ); ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.FuncTypeInput(0, 21, 
                                                        ASTNode.FuncType(0, 21, 
                                                                Token.LeftParen(0, 1, [| |]), 
                                                                ASTNode.TypeList(4, 15, 
                                                                                        [| 
                                                                                            ASTNode.Name(4, 5, Token.Name(4, 5, "b", [| |]));
                                                                                            ASTNode.Name(7, 8, Token.Name(7, 8, "d", [| |]));
                                                                                            ASTNode.TypedMul(11, 15, Token.Mul(11, 12, [| |]), ASTNode.Name(12, 15, Token.Name(12, 13, "e", [| |])))
                                                                                        |], [| Token.Comma(5, 6, [| |]); Token.Comma(8, 9, [| |]) |]), 
                                                                Token.RightParen(15, 16, [| |]), 
                                                                Token.Ptr(16, 18, [| |]), 
                                                                ASTNode.Name(19, 21, Token.Name(19, 20, "a", [| |]))
                                                        ), 
                                                        [| |], 
                                                        Token.EOF([| |]) ), parser.ParseFuncTypeInput())

    [<Fact>]
    let ``simple funct type input with two argument and mul and two more arguments test`` () =
           let lex = new MockTokenizer( [   ( Token.LeftParen(0, 1, [| |]), 0); ( Token.Name(4, 5, "b", [| |]), 4 ); ( Token.Comma(5, 6, [| |]), 5 ); ( Token.Name(7, 8, "d", [| |]), 7 );
                                            ( Token.Comma(8, 9, [| |]), 8 ); ( Token.Mul(11, 12, [| |]), 11 ); ( Token.Name(12, 13, "e", [| |]), 12 );
                                            ( Token.Comma(14, 15, [| |]), 14 ); ( Token.Name(18, 19, "f", [| |]), 18 );
                                            ( Token.Comma(20, 21, [| |]), 20 ); ( Token.Name(24, 25, "g", [| |]), 24 );
                                            ( Token.RightParen(25, 26, [| |]), 25 ); ( Token.Ptr(26, 28, [| |]), 26 ); ( Token.Name(29, 30, "a", [| |]), 29 ); 
                                            ( Token.EOF([| |]), 31 ); ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.FuncTypeInput(0, 31, 
                                                        ASTNode.FuncType(0, 31, 
                                                                Token.LeftParen(0, 1, [| |]), 
                                                                ASTNode.TypeList(4, 25, 
                                                                                        [| 
                                                                                            ASTNode.Name(4, 5, Token.Name(4, 5, "b", [| |]));
                                                                                            ASTNode.Name(7, 8, Token.Name(7, 8, "d", [| |]));
                                                                                            ASTNode.TypedMul(11, 14, Token.Mul(11, 12, [| |]), ASTNode.Name(12, 14, Token.Name(12, 13, "e", [| |])));
                                                                                            ASTNode.Name(18, 20, Token.Name(18, 19, "f", [| |]));
                                                                                            ASTNode.Name(24, 25, Token.Name(24, 25, "g", [| |]));
                                                                                        |], [| Token.Comma(5, 6, [| |]); Token.Comma(8, 9, [| |]); Token.Comma(14, 15, [| |]); Token.Comma(20, 21, [| |]) |]), 
                                                                Token.RightParen(25, 26, [| |]), 
                                                                Token.Ptr(26, 28, [| |]), 
                                                                ASTNode.Name(29, 31, Token.Name(29, 30, "a", [| |]))
                                                        ), 
                                                        [| |], 
                                                        Token.EOF([| |]) ), parser.ParseFuncTypeInput())

    [<Fact>]
    let ``simple funct type input with two argument and mul and two more arguments and power test`` () =
           let lex = new MockTokenizer( [   ( Token.LeftParen(0, 1, [| |]), 0); ( Token.Name(4, 5, "b", [| |]), 4 ); ( Token.Comma(5, 6, [| |]), 5 ); ( Token.Name(7, 8, "d", [| |]), 7 );
                                            ( Token.Comma(8, 9, [| |]), 8 ); ( Token.Mul(11, 12, [| |]), 11 ); ( Token.Name(12, 13, "e", [| |]), 12 );
                                            ( Token.Comma(14, 15, [| |]), 14 ); ( Token.Name(18, 19, "f", [| |]), 18 );
                                            ( Token.Comma(20, 21, [| |]), 20 ); ( Token.Name(24, 25, "g", [| |]), 24 );
                                            ( Token.Comma(26, 27, [| |]), 26 ); ( Token.Power(28, 30, [| |]), 28 ); ( Token.Name(30, 31, "e", [| |]), 30 );
                                            ( Token.RightParen(32, 33, [| |]), 32 ); ( Token.Ptr(33, 35, [| |]), 33 ); ( Token.Name(36, 37, "a", [| |]), 36 ); 
                                            ( Token.EOF([| |]), 38 ); ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.FuncTypeInput(0, 38, 
                                                        ASTNode.FuncType(0, 38, 
                                                                Token.LeftParen(0, 1, [| |]), 
                                                                ASTNode.TypeList(4, 32, 
                                                                                        [| 
                                                                                            ASTNode.Name(4, 5, Token.Name(4, 5, "b", [| |]));
                                                                                            ASTNode.Name(7, 8, Token.Name(7, 8, "d", [| |]));
                                                                                            ASTNode.TypedMul(11, 14, Token.Mul(11, 12, [| |]), ASTNode.Name(12, 14, Token.Name(12, 13, "e", [| |])));
                                                                                            ASTNode.Name(18, 20, Token.Name(18, 19, "f", [| |]));
                                                                                            ASTNode.Name(24, 26, Token.Name(24, 25, "g", [| |]));
                                                                                            ASTNode.TypedPower(28, 32, Token.Power(28, 30, [| |]), ASTNode.Name(30, 32, Token.Name(30, 31, "e", [| |])))
                                                                                        |], [| Token.Comma(5, 6, [| |]); Token.Comma(8, 9, [| |]); Token.Comma(14, 15, [| |]); Token.Comma(20, 21, [| |]); Token.Comma(26, 27, [| |]) |]), 
                                                                Token.RightParen(32, 33, [| |]), 
                                                                Token.Ptr(33, 35, [| |]), 
                                                                ASTNode.Name(36, 38, Token.Name(36, 37, "a", [| |]))
                                                        ), 
                                                        [| |], 
                                                        Token.EOF([| |]) ), parser.ParseFuncTypeInput())

    [<Fact>]
    let ``Func def suite with simple statement  test`` () =
           let lex = new MockTokenizer( [   ( Token.Pass(0, 4, [| |]), 0); ( Token.Newline(5, 7, [| |]), 5 ); ( Token.EOF([| |]), 7 ); ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.SimpleStmtList(0, 7, [| ASTNode.Pass(0, 5, Token.Pass(0, 4, [| |])) |], [| |], Token.Newline(5, 7, [| |])), parser.ParseFuncBodySuite())

    [<Fact>]
    let ``Func def suite with single statement test`` () =
           let lex = new MockTokenizer( [ ( Token.Newline(0, 2, [| |]), 0 ); ( Token.Indent([| |]), 2 ); ( Token.Pass(2, 6, [| |]), 2); ( Token.Newline(7, 9, [| |]), 7 ); ( Token.Dedent([| |]), 10 );  ( Token.EOF([| |]), 10 ); ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.FuncBodySuite(0, 10,   Token.Newline(0, 2, [| |]),
                                                        Token.Indent([| |]),
                                                        Token.Empty,
                                                        Token.Empty,
                                                        [|
                                                            ASTNode.SimpleStmtList(2, 10, [| ASTNode.Pass(2, 7, Token.Pass(2, 6, [| |])) |], [| |], Token.Newline(7, 9, [| |]))
                                                        |],
                                                        Token.Dedent([| |]) ), parser.ParseFuncBodySuite())

    [<Fact>]
    let ``Func def suite with double statement test`` () =
           let lex = new MockTokenizer( [   ( Token.Newline(0, 2, [| |]), 0 ); ( Token.Indent([| |]), 2 ); ( Token.Pass(2, 6, [| |]), 2); ( Token.Newline(7, 9, [| |]), 7 ); 
                                            ( Token.If(10, 12, [| |]), 10 ); ( Token.True(13, 17, [| |]), 13 ); ( Token.Colon(17, 18, [| |]), 17 ); ( Token.Pass(19, 23, [| |]), 19 ); ( Token.Newline(24, 26, [| |]), 24 );
                                            ( Token.Dedent([| |]), 27 );  ( Token.EOF([| |]), 27 ); ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.FuncBodySuite(0, 27,   Token.Newline(0, 2, [| |]),
                                                        Token.Indent([| |]),
                                                        Token.Empty,
                                                        Token.Empty,
                                                        [|
                                                            ASTNode.SimpleStmtList(2, 10, [| ASTNode.Pass(2, 7, Token.Pass(2, 6, [| |])) |], [| |], Token.Newline(7, 9, [| |]));
                                                            ASTNode.If(10, 27, Token.If(10, 12, [| |]), ASTNode.True(13, 17, Token.True(13, 17, [| |]) ), Token.Colon(17, 18, [| |]), ASTNode.SimpleStmtList(19, 27, [| ASTNode.Pass(19, 24, Token.Pass(19, 23, [| |])) |], [| |], Token.Newline(24, 26, [| |])), [| |], ASTNode.Empty )
                                                        |],
                                                        Token.Dedent([| |]) ), parser.ParseFuncBodySuite())

    [<Fact>]
    let ``Func def suite with double statement and type comment test`` () =
           let lex = new MockTokenizer( [   ( Token.Newline(0, 2, [| |]), 0 ); ( Token.Indent([| |]), 2 );
                                            ( Token.TypeComment(3, 14, "# type: int"), 3 ); ( Token.Newline(15, 17, [| |]), 15 );
                                            ( Token.Pass(18, 24, [| |]), 18); ( Token.Newline(23, 25, [| |]), 23 ); 
                                            ( Token.If(26, 28, [| |]), 26 ); ( Token.True(29, 31, [| |]), 29 ); ( Token.Colon(33, 34, [| |]), 34 ); ( Token.Pass(35, 39, [| |]), 35 ); ( Token.Newline(40, 42, [| |]), 40 );
                                            ( Token.Dedent([| |]), 41 );  ( Token.EOF([| |]), 41 ) ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.FuncBodySuite(0, 41,   Token.Newline(0, 2, [| |]),
                                                        Token.Indent([| |]),
                                                        Token.TypeComment(3, 14, "# type: int"),
                                                        Token.Newline(15, 17, [| |]),
                                                        [|
                                                            ASTNode.SimpleStmtList(18, 26, [| ASTNode.Pass(18, 23, Token.Pass(18, 24, [| |])) |], [| |], Token.Newline(23, 25, [| |]));
                                                            ASTNode.If(26, 41, Token.If(26, 28, [| |]), ASTNode.True(29, 34, Token.True(29, 31, [| |]) ), Token.Colon(33, 34, [| |]), ASTNode.SimpleStmtList(35, 41, [| ASTNode.Pass(35, 40, Token.Pass(35, 39, [| |])) |], [| |], Token.Newline(40, 42, [| |])), [| |], ASTNode.Empty )
                                                        |],
                                                        Token.Dedent([| |]) ), parser.ParseFuncBodySuite())

    [<Fact>]
    let ``def simple test`` () =
           let lex = new MockTokenizer( [ ( Token.Def(0, 3, [| |]), 0 ); ( Token.Name(4, 5, "a", [| |]), 4 ); ( Token.LeftParen(5, 6, [| |]), 5 ); ( Token.RightParen(6, 7, [| |]), 6 ); ( Token.Colon(8, 9, [| |]), 8 ); ( Token.Pass(10, 14, [| |]), 10 ); ( Token.Newline(15, 17, [| |]), 15 ); ( Token.EOF([| |]), 18 ) ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.FuncDef(0, 18, 
                                                Token.Def(0, 3, [| |]),  
                                                ASTNode.Name(4, 5, Token.Name(4, 5, "a", [| |])), 
                                                ASTNode.Parameters(5, 8, Token.LeftParen(5, 6, [| |]), ASTNode.Empty, Token.RightParen(6, 7, [| |])),
                                                Token.Empty,
                                                ASTNode.Empty,
                                                Token.Colon(8, 9, [| |]),
                                                Token.Empty,
                                                ASTNode.SimpleStmtList(10, 18, [| ASTNode.Pass(10, 15, Token.Pass(10, 14, [| |])) |], [| |], Token.Newline(15, 17, [| |]))
                                                ) , parser.ParseStmt())

    [<Fact>]
    let ``def simple with typecomment test`` () =
           let lex = new MockTokenizer( [   ( Token.Def(0, 3, [| |]), 0 ); ( Token.Name(4, 5, "a", [| |]), 4 ); ( Token.LeftParen(5, 6, [| |]), 5 ); ( Token.RightParen(6, 7, [| |]), 6 ); ( Token.Colon(8, 9, [| |]), 8 );
                                            ( Token.TypeComment(10, 20, "#type: int"), 10 )
                                            ( Token.Pass(20, 24, [| |]), 20 ); ( Token.Newline(25, 27, [| |]), 25 ); ( Token.EOF([| |]), 28 ) ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.FuncDef(0, 28, 
                                                Token.Def(0, 3, [| |]),  
                                                ASTNode.Name(4, 5, Token.Name(4, 5, "a", [| |])), 
                                                ASTNode.Parameters(5, 8, Token.LeftParen(5, 6, [| |]), ASTNode.Empty, Token.RightParen(6, 7, [| |])),
                                                Token.Empty,
                                                ASTNode.Empty,
                                                Token.Colon(8, 9, [| |]),
                                                Token.TypeComment(10, 20, "#type: int"),
                                                ASTNode.SimpleStmtList(20, 28, [| ASTNode.Pass(20, 25, Token.Pass(20, 24, [| |])) |], [| |], Token.Newline(25, 27, [| |]))
                                                ) , parser.ParseStmt())

    [<Fact>]
    let ``def simple with typecomment and return type test`` () =
           let lex = new MockTokenizer( [   ( Token.Def(0, 3, [| |]), 0 ); ( Token.Name(4, 5, "a", [| |]), 4 ); ( Token.LeftParen(5, 6, [| |]), 5 ); ( Token.RightParen(6, 7, [| |]), 6 ); 
                                            ( Token.Ptr(8, 10, [| |]), 8 ); ( Token.Name(11, 12, "b", [| |]), 11 ); ( Token.Colon(13, 14, [| |]), 13 ); ( Token.TypeComment(15, 25, "#type: int"), 15 )
                                            ( Token.Pass(25, 29, [| |]), 25 ); ( Token.Newline(30, 32, [| |]), 30 ); ( Token.EOF([| |]), 33 ) ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.FuncDef(0, 33, 
                                                Token.Def(0, 3, [| |]),  
                                                ASTNode.Name(4, 5, Token.Name(4, 5, "a", [| |])), 
                                                ASTNode.Parameters(5, 8, Token.LeftParen(5, 6, [| |]), ASTNode.Empty, Token.RightParen(6, 7, [| |])),
                                                Token.Ptr(8, 10, [| |]),
                                                ASTNode.Name(11, 13, Token.Name(11, 12, "b", [| |])),
                                                Token.Colon(13, 14, [| |]),
                                                Token.TypeComment(15, 25, "#type: int"),
                                                ASTNode.SimpleStmtList(25, 33, [| ASTNode.Pass(25, 30, Token.Pass(25, 29, [| |])) |], [| |], Token.Newline(30, 32, [| |]))
                                                ) , parser.ParseStmt())

    [<Fact>]
    let ``def simple with parameter test`` () =
           let lex = new MockTokenizer( [ ( Token.Def(0, 3, [| |]), 0 ); ( Token.Name(4, 5, "a", [| |]), 4 ); ( Token.LeftParen(5, 6, [| |]), 5 ); ( Token.Name(7, 8, "b", [| |]), 7 ); ( Token.RightParen(9, 10, [| |]), 9 ); ( Token.Colon(11, 12, [| |]), 11 ); ( Token.Pass(13, 17, [| |]), 13 ); ( Token.Newline(18, 20, [| |]), 18 ); ( Token.EOF([| |]), 21 ) ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.FuncDef(0, 21, 
                                                Token.Def(0, 3, [| |]),  
                                                ASTNode.Name(4, 5, Token.Name(4, 5, "a", [| |])), 
                                                ASTNode.Parameters(5, 11, Token.LeftParen(5, 6, [| |]), 
                                                            ASTNode.TypedArgsList(7, 9, [| 
                                                                                            ASTNode.Name(7, 9, Token.Name(7, 8, "b", [| |])) 
                                                                    |], [| |], [| |]), 
                                                            Token.RightParen(9, 10, [| |])),
                                                Token.Empty,
                                                ASTNode.Empty,
                                                Token.Colon(11, 12, [| |]),
                                                Token.Empty,
                                                ASTNode.SimpleStmtList(13, 21, [| ASTNode.Pass(13, 18, Token.Pass(13, 17, [| |])) |], [| |], Token.Newline(18, 20, [| |]))
                                                ) , parser.ParseStmt())

    [<Fact>]
    let ``async def simple test`` () =
           let lex = new MockTokenizer( [ ( Token.Async(0, 5, [| |]), 0 ); ( Token.Def(6, 9, [| |]), 6 ); ( Token.Name(10, 11, "a", [| |]), 10 ); ( Token.LeftParen(11, 12, [| |]), 11 ); ( Token.RightParen(12, 13, [| |]), 12 ); ( Token.Colon(14, 15, [| |]), 14 ); ( Token.Pass(16, 20, [| |]), 16 ); ( Token.Newline(21, 23, [| |]), 21 ); ( Token.EOF([| |]), 24 ) ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.AsyncFuncDef(0, 24, Token.Async(0, 5, [| |]),
                                        ASTNode.FuncDef(6, 24, 
                                                Token.Def(6, 9, [| |]),  
                                                ASTNode.Name(10, 11, Token.Name(10, 11, "a", [| |])), 
                                                ASTNode.Parameters(11, 14, Token.LeftParen(11, 12, [| |]), ASTNode.Empty, Token.RightParen(12, 13, [| |])),
                                                Token.Empty,
                                                ASTNode.Empty,
                                                Token.Colon(14, 15, [| |]),
                                                Token.Empty,
                                                ASTNode.SimpleStmtList(16, 24, [| ASTNode.Pass(16, 21, Token.Pass(16, 20, [| |])) |], [| |], Token.Newline(21, 23, [| |]))
                                                ) ) , parser.ParseStmt())

    [<Fact>]
    let ``decorator 1 test`` () =
           let lex = new MockTokenizer( [ ( Token.Matrice(0, 1, [| |]), 0 ); ( Token.Name(1, 2, "a", [| |]), 1 ); ( Token.Dot(2, 3, [| |]), 2 ); ( Token.Name(3, 4, "b", [| |]), 3); ( Token.Newline(5, 7, [| |]), 5 ); ( Token.EOF([| |]), 8 ) ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.Decorator(0, 8, Token.Matrice(0, 1, [| |]), ASTNode.DottedName(1, 5, [| ASTNode.Name(1, 2, Token.Name(1, 2, "a", [| |])); ASTNode.Name(3, 5, Token.Name(3, 4, "b", [| |])) |], [| Token.Dot(2, 3, [| |]) |]), Token.Empty, ASTNode.Empty, Token.Empty, Token.Newline(5, 7, [| |]) ), parser.ParseDecorator())

    [<Fact>]
    let ``decorator 2 test`` () =
           let lex = new MockTokenizer( [ ( Token.Matrice(0, 1, [| |]), 0 ); ( Token.Name(1, 2, "a", [| |]), 1 ); ( Token.Dot(2, 3, [| |]), 2 ); ( Token.Name(3, 4, "b", [| |]), 3); ( Token.LeftParen(4, 5, [| |]), 4 ); ( Token.RightParen(5, 6, [| |]), 5 ); ( Token.Newline(7, 9, [| |]), 7 ); ( Token.EOF([| |]), 10 ) ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.Decorator(0, 10, Token.Matrice(0, 1, [| |]), ASTNode.DottedName(1, 4, [| ASTNode.Name(1, 2, Token.Name(1, 2, "a", [| |])); ASTNode.Name(3, 4, Token.Name(3, 4, "b", [| |])) |], [| Token.Dot(2, 3, [| |]) |]), Token.LeftParen(4, 5, [| |]), ASTNode.Empty, Token.RightParen(5, 6, [| |]), Token.Newline(7, 9, [| |]) ), parser.ParseDecorator())

    [<Fact>]
    let ``decorator 3 test`` () =
           let lex = new MockTokenizer( [ ( Token.Matrice(0, 1, [| |]), 0 ); ( Token.Name(1, 2, "a", [| |]), 1 ); ( Token.Dot(2, 3, [| |]), 2 ); ( Token.Name(3, 4, "b", [| |]), 3); ( Token.LeftParen(4, 5, [| |]), 4 ); ( Token.Name(5, 6, "c", [| |]), 5 ); ( Token.RightParen(7, 8, [| |]), 7 ); ( Token.Newline(9, 11, [| |]), 9 ); ( Token.EOF([| |]), 12 ) ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.Decorator(0, 12, 
                                                    Token.Matrice(0, 1, [| |]), 
                                                    ASTNode.DottedName(1, 4, [| ASTNode.Name(1, 2, Token.Name(1, 2, "a", [| |])); ASTNode.Name(3, 4, Token.Name(3, 4, "b", [| |])) |], [| Token.Dot(2, 3, [| |]) |]), 
                                                    Token.LeftParen(4, 5, [| |]), 
                                                    ASTNode.ArgumentList(5, 7, [| ASTNode.Argument(5, 7, ASTNode.Name(5, 7, Token.Name(5, 6, "c", [| |])), Token.Empty, ASTNode.Empty)  |], [| |]), 
                                                    Token.RightParen(7, 8, [| |]), Token.Newline(9, 11, [| |]) ), parser.ParseDecorator())

    [<Fact>]
    let ``decorators test`` () =
           let lex = new MockTokenizer( [   ( Token.Matrice(0, 1, [| |]), 0 ); ( Token.Name(1, 2, "a", [| |]), 1 ); ( Token.Dot(2, 3, [| |]), 2 ); ( Token.Name(3, 4, "b", [| |]), 3); ( Token.Newline(5, 7, [| |]), 5 ); 
                                            ( Token.Matrice(8, 9, [| |]), 8 ); ( Token.Name(10, 11, "c", [| |]), 10 ); ( Token.Newline(12, 14, [| |]), 12 ); ( Token.EOF([| |]), 15 ) ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.Decorators(0, 15,
                                               [|
                                                    ASTNode.Decorator(0, 8, 
                                                                            Token.Matrice(0, 1, [| |]), 
                                                                            ASTNode.DottedName(1, 5, [| ASTNode.Name(1, 2, Token.Name(1, 2, "a", [| |])); ASTNode.Name(3, 5, Token.Name(3, 4, "b", [| |])) |], [| Token.Dot(2, 3, [| |]) |]), 
                                                                            Token.Empty, 
                                                                            ASTNode.Empty, 
                                                                            Token.Empty, 
                                                                            Token.Newline(5, 7, [| |]) );
                                                    ASTNode.Decorator(8, 15, 
                                                                            Token.Matrice(8, 9, [| |]), 
                                                                            ASTNode.DottedName(10, 12, [| ASTNode.Name(10, 12, Token.Name(10, 11, "c", [| |])); |], [| |]), 
                                                                            Token.Empty, 
                                                                            ASTNode.Empty, 
                                                                            Token.Empty, 
                                                                            Token.Newline(12, 14, [| |]) )
                                               |]), parser.ParseDecorators())

    [<Fact>]
    let ``decorated class test`` () =
           let lex = new MockTokenizer( [   ( Token.Matrice(0, 1, [| |]), 0 ); ( Token.Name(1, 2, "a", [| |]), 1 ); ( Token.Dot(2, 3, [| |]), 2 ); ( Token.Name(3, 4, "b", [| |]), 3); ( Token.Newline(5, 7, [| |]), 5 ); 
                                            ( Token.Class(8, 13, [| |]), 8 ); ( Token.Name(14, 15, "c", [| |]), 14 ); ( Token.Colon(15, 16, [| |]),15 ); ( Token.Pass(17, 21, [| |]), 17 ); ( Token.Newline(22, 24, [| |]), 22 ); ( Token.EOF([| |]), 25 ) ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.Decorated(0, 25, 
                            ASTNode.Decorators(0, 8, 
                                [|
                                    ASTNode.Decorator(0, 8, Token.Matrice(0, 1, [| |]), ASTNode.DottedName(1, 5, [| ASTNode.Name(1, 2, Token.Name(1, 2, "a", [| |])); ASTNode.Name(3, 5, Token.Name(3, 4, "b", [| |])) |], [| Token.Dot(2, 3, [| |]) |]), Token.Empty, ASTNode.Empty, Token.Empty, Token.Newline(5, 7, [| |]) )
                                |] ),
                            ASTNode.Class(8, 25, Token.Class(8, 13, [| |]), ASTNode.Name(14, 15, Token.Name(14, 15, "c", [| |] )), Token.Empty, ASTNode.Empty, Token.Empty, Token.Colon(15, 16, [| |]), ASTNode.SimpleStmtList(17, 25, [| ASTNode.Pass(17, 22, Token.Pass(17, 21, [| |])) |], [| |], Token.Newline(22, 24, [| |])) )
                            ), parser.ParseStmt())
    [<Fact>]
    let ``decorated def test`` () =
           let lex = new MockTokenizer( [   ( Token.Matrice(0, 1, [| |]), 0 ); ( Token.Name(1, 2, "a", [| |]), 1 ); ( Token.Dot(2, 3, [| |]), 2 ); ( Token.Name(3, 4, "b", [| |]), 3); ( Token.Newline(5, 7, [| |]), 5 ); 
                                            ( Token.Def(8, 11, [| |]), 8 ); ( Token.Name(12, 13, "c", [| |]), 12 ); ( Token.LeftParen(13, 14, [| |]), 13 ); ( Token.RightParen(14, 15, [| |]), 14 ); ( Token.Colon(16, 17, [| |]), 16 ); ( Token.Pass(18, 22, [| |]), 18 ); ( Token.Newline(23, 25, [| |]), 23 );
                                            ( Token.EOF([| |]), 26 ) ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.Decorated(0, 26, 
                           ASTNode.Decorators(0, 8, 
                               [|
                                   ASTNode.Decorator(0, 8, Token.Matrice(0, 1, [| |]), ASTNode.DottedName(1, 5, [| ASTNode.Name(1, 2, Token.Name(1, 2, "a", [| |])); ASTNode.Name(3, 5, Token.Name(3, 4, "b", [| |])) |], [| Token.Dot(2, 3, [| |]) |]), Token.Empty, ASTNode.Empty, Token.Empty, Token.Newline(5, 7, [| |]) )
                               |] ),
                           ASTNode.FuncDef(8, 26,
                                                Token.Def(8, 11, [| |]),
                                                ASTNode.Name(12, 13, Token.Name(12, 13, "c", [| |])),
                                                ASTNode.Parameters(13, 16, Token.LeftParen(13, 14, [| |]), ASTNode.Empty, Token.RightParen(14, 15, [| |])),
                                                Token.Empty,
                                                ASTNode.Empty,
                                                Token.Colon(16, 17, [| |]),
                                                Token.Empty,
                                                ASTNode.SimpleStmtList(18, 26, [| ASTNode.Pass(18, 23, Token.Pass(18, 22, [| |])) |], [| |], Token.Newline(23, 25, [| |])) )
                           ), parser.ParseStmt())

    [<Fact>]
    let ``decorated async def test`` () =
           let lex = new MockTokenizer( [   ( Token.Matrice(0, 1, [| |]), 0 ); ( Token.Name(1, 2, "a", [| |]), 1 ); ( Token.Dot(2, 3, [| |]), 2 ); ( Token.Name(3, 4, "b", [| |]), 3); ( Token.Newline(5, 7, [| |]), 5 ); 
                                            ( Token.Async(8, 13, [| |]), 8 ); ( Token.Def(14, 17, [| |]), 14 ); ( Token.Name(18, 19, "c", [| |]), 18 ); ( Token.LeftParen(19, 20, [| |]), 19 ); ( Token.RightParen(20, 21, [| |]), 20 ); ( Token.Colon(22, 23, [| |]), 22 ); ( Token.Pass(24, 28, [| |]), 24 ); ( Token.Newline(29, 31, [| |]), 29 );
                                            ( Token.EOF([| |]), 32 ) ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.Decorated(0, 32, 
                               ASTNode.Decorators(0, 8, 
                                   [|
                                       ASTNode.Decorator(0, 8, Token.Matrice(0, 1, [| |]), ASTNode.DottedName(1, 5, [| ASTNode.Name(1, 2, Token.Name(1, 2, "a", [| |])); ASTNode.Name(3, 5, Token.Name(3, 4, "b", [| |])) |], [| Token.Dot(2, 3, [| |]) |]), Token.Empty, ASTNode.Empty, Token.Empty, Token.Newline(5, 7, [| |]) )
                                   |] ),
                               ASTNode.AsyncFuncDef(8, 32, Token.Async(8, 13, [| |]),
                                            ASTNode.FuncDef(14, 32,
                                                                Token.Def(14, 17, [| |]),
                                                                ASTNode.Name(18, 19, Token.Name(18, 19, "c", [| |])),
                                                                ASTNode.Parameters(19, 22, Token.LeftParen(19, 20, [| |]), ASTNode.Empty, Token.RightParen(20, 21, [| |])),
                                                                Token.Empty,
                                                                ASTNode.Empty,
                                                                Token.Colon(22, 23, [| |]),
                                                                Token.Empty,
                                                                ASTNode.SimpleStmtList(24, 32, [| ASTNode.Pass(24, 29, Token.Pass(24, 28, [| |])) |], [| |], Token.Newline(29, 31, [| |])) )
                                    )
                               ), parser.ParseStmt())

    [<Fact>]
    let ``eval input 1 test`` () =
            let lex = new MockTokenizer( [ ( Token.Name(0, 1, "a", [| |]), 0 ); ( Token.EOF([| |]), 2 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.EvalInput(0, 2, ASTNode.TestList(0, 2, [| ASTNode.Name(0, 2, Token.Name(0, 1, "a", [| |])) |], [| |]), [| |], Token.EOF([| |])), parser.ParseEvalInput())

    [<Fact>]
    let ``eval input 2 test`` () =
           let lex = new MockTokenizer( [ ( Token.Name(0, 1, "a", [| |]), 0 ); ( Token.Newline(2, 4, [| |]), 2 ); ( Token.EOF([| |]), 5 ) ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.EvalInput(0, 5, ASTNode.TestList(0, 2, [| ASTNode.Name(0, 2, Token.Name(0, 1, "a", [| |])) |], [| |]), [| Token.Newline(2, 4, [| |]) |], Token.EOF([| |])), parser.ParseEvalInput())

    [<Fact>]
    let ``eval input 3 test`` () =
           let lex = new MockTokenizer( [ ( Token.Name(0, 1, "a", [| |]), 0 ); ( Token.Newline(2, 4, [| |]), 2 ); ( Token.Newline(5, 7, [| |]), 5 ); ( Token.EOF([| |]), 5 ) ] )
           lex.Next()
           let parser = new Parser(lex)
           Assert.Equal( ASTNode.EvalInput(0, 5, ASTNode.TestList(0, 2, [| ASTNode.Name(0, 2, Token.Name(0, 1, "a", [| |])) |], [| |]), [| Token.Newline(2, 4, [| |]); Token.Newline(5, 7, [| |]) |], Token.EOF([| |])), parser.ParseEvalInput())

    [<Fact>]
    let ``file input 1 test`` () =
            let lex = new MockTokenizer( [ ( Token.Newline(0, 2, [| |]), 0 ); ( Token.EOF([| |]), 3 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.FileInput(0, 3, [| |], [| Token.Newline(0, 2, [| |]) |], Token.EOF([| |])), parser.ParseFileInput())

    [<Fact>]
    let ``file input 2 test`` () =
            let lex = new MockTokenizer( [ ( Token.Newline(0, 2, [| |]), 0 ); ( Token.Pass(3, 7, [| |]), 3 ); ( Token.Newline(8, 10, [| |]), 8 ); ( Token.EOF([| |]), 11 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.FileInput(0, 11, [| 
                                                        ASTNode.SimpleStmtList(3, 11, [| ASTNode.Pass(3, 8, Token.Pass(3, 7, [| |])) |], [| |], Token.Newline(8, 10, [| |])) 
                                                    |], [| Token.Newline(0, 2, [| |]) |], Token.EOF([| |])), parser.ParseFileInput())

    [<Fact>]
    let ``file input 3 test`` () =
            let lex = new MockTokenizer( [ ( Token.Newline(0, 2, [| |]), 0 ); ( Token.Pass(3, 7, [| |]), 3 ); ( Token.Newline(8, 10, [| |]), 8 ); ( Token.Newline(11, 13, [| |]), 11 ); ( Token.EOF([| |]), 14 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.FileInput(0, 14, [| 
                                                        ASTNode.SimpleStmtList(3, 11, [| ASTNode.Pass(3, 8, Token.Pass(3, 7, [| |])) |], [| |], Token.Newline(8, 10, [| |])) 
                                                    |], [| Token.Newline(0, 2, [| |]); Token.Newline(11, 13, [| |]) |], Token.EOF([| |])), parser.ParseFileInput())

    [<Fact>]
    let ``file input 4 test`` () =
            let lex = new MockTokenizer( [ ( Token.Newline(0, 2, [| |]), 0 ); ( Token.Pass(3, 7, [| |]), 3 ); ( Token.Newline(8, 10, [| |]), 8 ); ( Token.Newline(11, 13, [| |]), 11 ); ( Token.Pass(14, 18, [| |]), 14 ); ( Token.Newline(19, 21, [| |]), 19 );  ( Token.EOF([| |]), 21 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.FileInput(0, 21, [| 
                                                        ASTNode.SimpleStmtList(3, 11, [| ASTNode.Pass(3, 8, Token.Pass(3, 7, [| |])) |], [| |], Token.Newline(8, 10, [| |]));
                                                        ASTNode.SimpleStmtList(14, 21, [| ASTNode.Pass(14, 19, Token.Pass(14, 18, [| |])) |], [| |], Token.Newline(19, 21, [| |])) 
                                                    |], [| Token.Newline(0, 2, [| |]); Token.Newline(11, 13, [| |]) |], Token.EOF([| |])), parser.ParseFileInput())

    [<Fact>]
    let ``Single input 1 test`` () =
            let lex = new MockTokenizer( [ ( Token.Newline(0, 2, [| |]), 0 ); ( Token.EOF([| |]), 3 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.SingleInput(0, 3, ASTNode.Empty, Token.Newline(0, 2, [| |]) ), parser.ParseSingleInput())

    [<Fact>]
    let ``Single input 2 test`` () =
            let lex = new MockTokenizer( [ ( Token.Pass(0, 4, [| |]), 0 ); ( Token.Newline(5, 7, [| |]), 5 ); ( Token.EOF([| |]), 8 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.SingleInput(0, 8, ASTNode.SimpleStmtList(0, 8, [| ASTNode.Pass(0, 5, Token.Pass(0, 4, [| |])) |], [| |], Token.Newline(5, 7, [| |])), Token.Empty ), parser.ParseSingleInput())

    [<Fact>]
    let ``Single input 3 test`` () =
            let lex = new MockTokenizer( [ ( Token.Pass(0, 4, [| |]), 0 ); ( Token.SemiColon(5, 6, [| |]), 5 ); ( Token.Pass( 7, 11, [| |]), 7 ); ( Token.Newline(12, 14, [| |]), 12 ); ( Token.EOF([| |]), 15 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.SingleInput(0, 15, ASTNode.SimpleStmtList(0, 15, 
                                                                                    [| 
                                                                                        ASTNode.Pass(0, 5, Token.Pass(0, 4, [| |]));
                                                                                        ASTNode.Pass(7, 12, Token.Pass(7, 11, [| |]))
                                                                                    |], [| Token.SemiColon(5, 6, [| |]) |], Token.Newline(12, 14, [| |])), Token.Empty ), parser.ParseSingleInput())

    [<Fact>]
    let ``Single input 4 test`` () =
            let lex = new MockTokenizer( [ ( Token.If(0, 2, [| |]), 0 ); ( Token.Name(3, 4, "a", [| |]), 3 ); ( Token.Colon(4, 5, [| |]), 4 ); ( Token.Pass(6, 10, [| |]), 6 ); ( Token.Newline(11, 13, [| |]), 11 ); ( Token.Newline(14, 16, [| |]), 14 ); ( Token.EOF([| |]), 17 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.SingleInput(0, 17, 
                                                    ASTNode.If(0, 14, 
                                                                    Token.If(0, 2, [| |]),
                                                                    ASTNode.Name(3, 4, Token.Name(3, 4, "a", [| |])),
                                                                    Token.Colon(4, 5, [| |]),
                                                                    ASTNode.SimpleStmtList(6, 14, [| ASTNode.Pass(6, 11, Token.Pass(6, 10, [| |])) |], [| |], Token.Newline(11, 13, [| |])),
                                                                    [| |],
                                                                    ASTNode.Empty
                                                    ), 
                                                    Token.Newline(14, 16, [| |]) ), parser.ParseSingleInput())

    [<Fact>]
    let ``varargslist 1 test`` () =
            let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.EOF([| |]), 6 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.VarArgsList(0, 6, [| ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |]))  |], [| |], [| |]), parser.ParseVarArgsList())

    [<Fact>]
    let ``varargslist 2 test`` () =
            let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Assign(6, 7, [| |]), 6 ); ( Token.Name(8, 9, "b", [| |]), 8 ); ( Token.EOF([| |]), 10 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.VarArgsList(0, 10, [| 
                                                        ASTNode.VarAssign(0, 10,
                                                                    ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])),
                                                                    Token.Assign(6, 7, [| |]),
                                                                    ASTNode.Name(8, 10, Token.Name(8, 9, "b", [| |]))
                                                                    )
                                                     |], [| |], [| |]), parser.ParseVarArgsList())

    [<Fact>]
    let ``varargslist 3 test`` () =
            let lex = new MockTokenizer( [  ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Assign(6, 7, [| |]), 6 ); ( Token.Name(8, 9, "b", [| |]), 8 ); 
                                            ( Token.Comma(10, 11, [| |]), 10 ); ( Token.Name(12, 13, "a", [| |]), 12 );
                                            ( Token.EOF([| |]), 14 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.VarArgsList(0, 14, [| 
                                                        ASTNode.VarAssign(0, 10,
                                                                    ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])),
                                                                    Token.Assign(6, 7, [| |]),
                                                                    ASTNode.Name(8, 10, Token.Name(8, 9, "b", [| |]))
                                                                    );
                                                        ASTNode.Name(12, 14, Token.Name(12, 13, "a", [| |]))
                                                     |], [| Token.Comma(10, 11, [| |]) |], [| |]), parser.ParseVarArgsList())

    [<Fact>]
    let ``varargslist 4 test`` () =
            let lex = new MockTokenizer( [  ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Assign(6, 7, [| |]), 6 ); ( Token.Name(8, 9, "b", [| |]), 8 ); 
                                            ( Token.Comma(10, 11, [| |]), 10 ); ( Token.Name(12, 13, "a", [| |]), 12 );
                                            ( Token.Comma(13, 14, [| |]), 13 ); ( Token.Div(15, 16, [| |]), 15 );
                                            ( Token.EOF([| |]), 17 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.VarArgsList(0, 17, [| 
                                                        ASTNode.VarAssign(0, 10,
                                                                    ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])),
                                                                    Token.Assign(6, 7, [| |]),
                                                                    ASTNode.Name(8, 10, Token.Name(8, 9, "b", [| |]))
                                                                    );
                                                        ASTNode.Name(12, 13, Token.Name(12, 13, "a", [| |]));
                                                        ASTNode.ArgDiv(15, 17, Token.Div(15, 16, [| |]))
                                                     |], [| 
                                                            Token.Comma(10, 11, [| |]);
                                                            Token.Comma(13, 14, [| |])
                                                        |], [| |]), parser.ParseVarArgsList())

    [<Fact>]
    let ``varargslist 5 test`` () =
            let lex = new MockTokenizer( [  ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Assign(6, 7, [| |]), 6 ); ( Token.Name(8, 9, "b", [| |]), 8 ); 
                                            ( Token.Comma(10, 11, [| |]), 10 ); ( Token.Name(12, 13, "a", [| |]), 12 );
                                            ( Token.Comma(13, 14, [| |]), 13 ); ( Token.Div(15, 16, [| |]), 15 );
                                            ( Token.Comma(17, 18, [| |]), 17 ); ( Token.Name(19, 20, "c", [| |]), 19 );
                                            ( Token.EOF([| |]), 21 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.VarArgsList(0, 21, [| 
                                                        ASTNode.VarAssign(0, 10,
                                                                    ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])),
                                                                    Token.Assign(6, 7, [| |]),
                                                                    ASTNode.Name(8, 10, Token.Name(8, 9, "b", [| |]))
                                                                    );
                                                        ASTNode.Name(12, 13, Token.Name(12, 13, "a", [| |]));
                                                        ASTNode.ArgDiv(15, 17, Token.Div(15, 16, [| |]));
                                                        ASTNode.Name(19, 21, Token.Name(19, 20, "c", [| |]))
                                                     |], [| 
                                                            Token.Comma(10, 11, [| |]);
                                                            Token.Comma(13, 14, [| |]);
                                                            Token.Comma(17, 18, [| |])
                                                        |], [| |]), parser.ParseVarArgsList())

    [<Fact>]
    let ``varargslist 6 test`` () =
            let lex = new MockTokenizer( [  ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Assign(6, 7, [| |]), 6 ); ( Token.Name(8, 9, "b", [| |]), 8 ); 
                                            ( Token.Comma(10, 11, [| |]), 10 ); ( Token.Name(12, 13, "a", [| |]), 12 );
                                            ( Token.Comma(13, 14, [| |]), 13 ); ( Token.Div(15, 16, [| |]), 15 );
                                            ( Token.Comma(17, 18, [| |]), 17 ); ( Token.Name(19, 20, "c", [| |]), 19 );
                                            ( Token.Comma(21, 22, [| |]), 21 ); ( Token.Name(22, 23, "e", [| |]), 22 ); ( Token.Assign(23, 24, [| |]), 23 ); ( Token.Name(25, 26, "f", [| |]), 25 );
                                            ( Token.EOF([| |]), 27 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.VarArgsList(0, 27, [| 
                                                        ASTNode.VarAssign(0, 10,
                                                                    ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])),
                                                                    Token.Assign(6, 7, [| |]),
                                                                    ASTNode.Name(8, 10, Token.Name(8, 9, "b", [| |]))
                                                                    );
                                                        ASTNode.Name(12, 13, Token.Name(12, 13, "a", [| |]));
                                                        ASTNode.ArgDiv(15, 17, Token.Div(15, 16, [| |]));
                                                        ASTNode.Name(19, 21, Token.Name(19, 20, "c", [| |]));
                                                        ASTNode.VarAssign(22, 27,
                                                                    ASTNode.Name(22, 23, Token.Name(22, 23, "e", [| |])),
                                                                    Token.Assign(23, 24, [| |]),
                                                                    ASTNode.Name(25, 27, Token.Name(25, 26, "f", [| |]))
                                                                    )
                                                     |], [| 
                                                            Token.Comma(10, 11, [| |]);
                                                            Token.Comma(13, 14, [| |]);
                                                            Token.Comma(17, 18, [| |]);
                                                            Token.Comma(21, 22, [| |])
                                                        |], [| |]), parser.ParseVarArgsList())

    [<Fact>]
    let ``varargslist 7 test`` () =
            let lex = new MockTokenizer( [  ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Assign(6, 7, [| |]), 6 ); ( Token.Name(8, 9, "b", [| |]), 8 ); 
                                            ( Token.Comma(10, 11, [| |]), 10 ); ( Token.Name(12, 13, "a", [| |]), 12 );
                                            ( Token.Comma(13, 14, [| |]), 13 ); ( Token.Div(15, 16, [| |]), 15 );
                                            ( Token.Comma(17, 18, [| |]), 17 ); ( Token.Name(19, 20, "c", [| |]), 19 );
                                            ( Token.Comma(21, 22, [| |]), 21 ); ( Token.Name(22, 23, "e", [| |]), 22 ); ( Token.Assign(23, 24, [| |]), 23 ); ( Token.Name(25, 26, "f", [| |]), 25 );
                                            ( Token.Comma(27, 28, [| |]), 27 ); ( Token.Mul(29, 30, [| |]), 29 ); ( Token.Name(31, 32, "g", [| |]), 31 );
                                            ( Token.EOF([| |]), 33 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.VarArgsList(0, 33, [| 
                                                        ASTNode.VarAssign(0, 10,
                                                                    ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])),
                                                                    Token.Assign(6, 7, [| |]),
                                                                    ASTNode.Name(8, 10, Token.Name(8, 9, "b", [| |]))
                                                                    );
                                                        ASTNode.Name(12, 13, Token.Name(12, 13, "a", [| |]));
                                                        ASTNode.ArgDiv(15, 17, Token.Div(15, 16, [| |]));
                                                        ASTNode.Name(19, 21, Token.Name(19, 20, "c", [| |]));
                                                        ASTNode.VarAssign(22, 27,
                                                                    ASTNode.Name(22, 23, Token.Name(22, 23, "e", [| |])),
                                                                    Token.Assign(23, 24, [| |]),
                                                                    ASTNode.Name(25, 27, Token.Name(25, 26, "f", [| |]))
                                                                    );
                                                        ASTNode.VarMul(29, 33, Token.Mul(29, 30, [| |]), ASTNode.Name(31, 33, Token.Name(31, 32, "g", [| |])))
                                                     |], [| 
                                                            Token.Comma(10, 11, [| |]);
                                                            Token.Comma(13, 14, [| |]);
                                                            Token.Comma(17, 18, [| |]);
                                                            Token.Comma(21, 22, [| |]);
                                                            Token.Comma(27, 28, [| |])
                                                        |], [| |]), parser.ParseVarArgsList())

    [<Fact>]
    let ``varargslist 8 test`` () =
            let lex = new MockTokenizer( [  ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Assign(6, 7, [| |]), 6 ); ( Token.Name(8, 9, "b", [| |]), 8 ); 
                                            ( Token.Comma(10, 11, [| |]), 10 ); ( Token.Name(12, 13, "a", [| |]), 12 );
                                            ( Token.Comma(13, 14, [| |]), 13 ); ( Token.Div(15, 16, [| |]), 15 );
                                            ( Token.Comma(17, 18, [| |]), 17 ); ( Token.Name(19, 20, "c", [| |]), 19 );
                                            ( Token.Comma(21, 22, [| |]), 21 ); ( Token.Name(22, 23, "e", [| |]), 22 ); ( Token.Assign(23, 24, [| |]), 23 ); ( Token.Name(25, 26, "f", [| |]), 25 );
                                            ( Token.Comma(27, 28, [| |]), 27 ); ( Token.Mul(29, 30, [| |]), 29 ); ( Token.Name(31, 32, "g", [| |]), 31 );
                                            ( Token.Comma(33, 34, [| |]), 33 ); ( Token.Name(35, 36, "h", [| |]), 35 );
                                            ( Token.EOF([| |]), 37 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.VarArgsList(0, 37, [| 
                                                        ASTNode.VarAssign(0, 10,
                                                                    ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])),
                                                                    Token.Assign(6, 7, [| |]),
                                                                    ASTNode.Name(8, 10, Token.Name(8, 9, "b", [| |]))
                                                                    );
                                                        ASTNode.Name(12, 13, Token.Name(12, 13, "a", [| |]));
                                                        ASTNode.ArgDiv(15, 17, Token.Div(15, 16, [| |]));
                                                        ASTNode.Name(19, 21, Token.Name(19, 20, "c", [| |]));
                                                        ASTNode.VarAssign(22, 27,
                                                                    ASTNode.Name(22, 23, Token.Name(22, 23, "e", [| |])),
                                                                    Token.Assign(23, 24, [| |]),
                                                                    ASTNode.Name(25, 27, Token.Name(25, 26, "f", [| |]))
                                                                    );
                                                        ASTNode.VarMul(29, 33, Token.Mul(29, 30, [| |]), ASTNode.Name(31, 33, Token.Name(31, 32, "g", [| |])));
                                                        ASTNode.Name(35, 37, Token.Name(35, 36, "h", [| |]))
                                                        
                                                     |], [| 
                                                            Token.Comma(10, 11, [| |]);
                                                            Token.Comma(13, 14, [| |]);
                                                            Token.Comma(17, 18, [| |]);
                                                            Token.Comma(21, 22, [| |]);
                                                            Token.Comma(27, 28, [| |]);
                                                            Token.Comma(33, 34, [| |])
                                                        |], [| |]), parser.ParseVarArgsList())

    [<Fact>]
    let ``varargslist 9 test`` () =
            let lex = new MockTokenizer( [  ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Assign(6, 7, [| |]), 6 ); ( Token.Name(8, 9, "b", [| |]), 8 ); 
                                            ( Token.Comma(10, 11, [| |]), 10 ); ( Token.Name(12, 13, "a", [| |]), 12 );
                                            ( Token.Comma(13, 14, [| |]), 13 ); ( Token.Div(15, 16, [| |]), 15 );
                                            ( Token.Comma(17, 18, [| |]), 17 ); ( Token.Name(19, 20, "c", [| |]), 19 );
                                            ( Token.Comma(21, 22, [| |]), 21 ); ( Token.Name(22, 23, "e", [| |]), 22 ); ( Token.Assign(23, 24, [| |]), 23 ); ( Token.Name(25, 26, "f", [| |]), 25 );
                                            ( Token.Comma(27, 28, [| |]), 27 ); ( Token.Mul(29, 30, [| |]), 29 ); ( Token.Name(31, 32, "g", [| |]), 31 );
                                            ( Token.Comma(33, 34, [| |]), 33 ); ( Token.Name(35, 36, "h", [| |]), 35 );
                                            ( Token.Comma(37, 38, [| |]), 37 ); ( Token.Name(39, 40, "g", [| |]), 39 ); ( Token.Assign(41, 42, [| |]), 41 ); ( Token.Name(43, 44, "x", [| |]), 43 );
                                            ( Token.EOF([| |]), 45 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.VarArgsList(0, 45, [| 
                                                        ASTNode.VarAssign(0, 10,
                                                                    ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])),
                                                                    Token.Assign(6, 7, [| |]),
                                                                    ASTNode.Name(8, 10, Token.Name(8, 9, "b", [| |]))
                                                                    );
                                                        ASTNode.Name(12, 13, Token.Name(12, 13, "a", [| |]));
                                                        ASTNode.ArgDiv(15, 17, Token.Div(15, 16, [| |]));
                                                        ASTNode.Name(19, 21, Token.Name(19, 20, "c", [| |]));
                                                        ASTNode.VarAssign(22, 27,
                                                                    ASTNode.Name(22, 23, Token.Name(22, 23, "e", [| |])),
                                                                    Token.Assign(23, 24, [| |]),
                                                                    ASTNode.Name(25, 27, Token.Name(25, 26, "f", [| |]))
                                                                    );
                                                        ASTNode.VarMul(29, 33, Token.Mul(29, 30, [| |]), ASTNode.Name(31, 33, Token.Name(31, 32, "g", [| |])));
                                                        ASTNode.Name(35, 37, Token.Name(35, 36, "h", [| |]));
                                                        ASTNode.VarAssign(39, 45,
                                                                    ASTNode.Name(39, 41, Token.Name(39, 40, "g", [| |])),
                                                                    Token.Assign(41, 42, [| |]),
                                                                    ASTNode.Name(43, 45, Token.Name(43, 44, "x", [| |]))
                                                                    )
                                                     |], [| 
                                                            Token.Comma(10, 11, [| |]);
                                                            Token.Comma(13, 14, [| |]);
                                                            Token.Comma(17, 18, [| |]);
                                                            Token.Comma(21, 22, [| |]);
                                                            Token.Comma(27, 28, [| |]);
                                                            Token.Comma(33, 34, [| |]);
                                                            Token.Comma(37, 38, [| |])
                                                        |], [| |]), parser.ParseVarArgsList())

    [<Fact>]
    let ``varargslist 10 test`` () =
            let lex = new MockTokenizer( [  ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Assign(6, 7, [| |]), 6 ); ( Token.Name(8, 9, "b", [| |]), 8 ); 
                                            ( Token.Comma(10, 11, [| |]), 10 ); ( Token.Name(12, 13, "a", [| |]), 12 );
                                            ( Token.Comma(13, 14, [| |]), 13 ); ( Token.Div(15, 16, [| |]), 15 );
                                            ( Token.Comma(17, 18, [| |]), 17 ); ( Token.Name(19, 20, "c", [| |]), 19 );
                                            ( Token.Comma(21, 22, [| |]), 21 ); ( Token.Name(22, 23, "e", [| |]), 22 ); ( Token.Assign(23, 24, [| |]), 23 ); ( Token.Name(25, 26, "f", [| |]), 25 );
                                            ( Token.Comma(27, 28, [| |]), 27 ); ( Token.Mul(29, 30, [| |]), 29 ); ( Token.Name(31, 32, "g", [| |]), 31 );
                                            ( Token.Comma(33, 34, [| |]), 33 ); ( Token.Name(35, 36, "h", [| |]), 35 );
                                            ( Token.Comma(37, 38, [| |]), 37 ); ( Token.Name(39, 40, "g", [| |]), 39 ); ( Token.Assign(41, 42, [| |]), 41 ); ( Token.Name(43, 44, "x", [| |]), 43 );
                                            ( Token.Comma(45, 46, [| |]), 45 ); ( Token.Power(47, 49, [| |]), 47 ); ( Token.Name(50, 51, "y", [| |]), 50 ); 
                                            ( Token.EOF([| |]), 52 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.VarArgsList(0, 52, [| 
                                                        ASTNode.VarAssign(0, 10,
                                                                    ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])),
                                                                    Token.Assign(6, 7, [| |]),
                                                                    ASTNode.Name(8, 10, Token.Name(8, 9, "b", [| |]))
                                                                    );
                                                        ASTNode.Name(12, 13, Token.Name(12, 13, "a", [| |]));
                                                        ASTNode.ArgDiv(15, 17, Token.Div(15, 16, [| |]));
                                                        ASTNode.Name(19, 21, Token.Name(19, 20, "c", [| |]));
                                                        ASTNode.VarAssign(22, 27,
                                                                    ASTNode.Name(22, 23, Token.Name(22, 23, "e", [| |])),
                                                                    Token.Assign(23, 24, [| |]),
                                                                    ASTNode.Name(25, 27, Token.Name(25, 26, "f", [| |]))
                                                                    );
                                                        ASTNode.VarMul(29, 33, Token.Mul(29, 30, [| |]), ASTNode.Name(31, 33, Token.Name(31, 32, "g", [| |])));
                                                        ASTNode.Name(35, 37, Token.Name(35, 36, "h", [| |]));
                                                        ASTNode.VarAssign(39, 45,
                                                                    ASTNode.Name(39, 41, Token.Name(39, 40, "g", [| |])),
                                                                    Token.Assign(41, 42, [| |]),
                                                                    ASTNode.Name(43, 45, Token.Name(43, 44, "x", [| |]))
                                                                    );
                                                        ASTNode.VarPower(47, 52, Token.Power(47, 49, [| |]), ASTNode.Name(50, 52, Token.Name(50, 51, "y", [| |])))
                                                     |], [| 
                                                            Token.Comma(10, 11, [| |]);
                                                            Token.Comma(13, 14, [| |]);
                                                            Token.Comma(17, 18, [| |]);
                                                            Token.Comma(21, 22, [| |]);
                                                            Token.Comma(27, 28, [| |]);
                                                            Token.Comma(33, 34, [| |]);
                                                            Token.Comma(37, 38, [| |]);
                                                            Token.Comma(45, 46, [| |])
                                                        |], [| |]), parser.ParseVarArgsList())

    [<Fact>]
    let ``varargslist 11 test`` () =
            let lex = new MockTokenizer( [  ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Assign(6, 7, [| |]), 6 ); ( Token.Name(8, 9, "b", [| |]), 8 ); 
                                            ( Token.Comma(10, 11, [| |]), 10 ); ( Token.Name(12, 13, "a", [| |]), 12 );
                                            ( Token.Comma(13, 14, [| |]), 13 ); ( Token.Div(15, 16, [| |]), 15 );
                                            ( Token.Comma(17, 18, [| |]), 17 ); ( Token.Name(19, 20, "c", [| |]), 19 );
                                            ( Token.Comma(21, 22, [| |]), 21 ); ( Token.Name(22, 23, "e", [| |]), 22 ); ( Token.Assign(23, 24, [| |]), 23 ); ( Token.Name(25, 26, "f", [| |]), 25 );
                                            ( Token.Comma(27, 28, [| |]), 27 ); ( Token.Mul(29, 30, [| |]), 29 ); ( Token.Name(31, 32, "g", [| |]), 31 );
                                            ( Token.Comma(33, 34, [| |]), 33 ); ( Token.Name(35, 36, "h", [| |]), 35 );
                                            ( Token.Comma(37, 38, [| |]), 37 ); ( Token.Name(39, 40, "g", [| |]), 39 ); ( Token.Assign(41, 42, [| |]), 41 ); ( Token.Name(43, 44, "x", [| |]), 43 );
                                            ( Token.Comma(45, 46, [| |]), 45 ); ( Token.Power(47, 49, [| |]), 47 ); ( Token.Name(50, 51, "y", [| |]), 50 ); 
                                            ( Token.Comma(52, 53, [| |]), 52 ); ( Token.EOF([| |]), 54 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.VarArgsList(0, 54, [| 
                                                        ASTNode.VarAssign(0, 10,
                                                                    ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])),
                                                                    Token.Assign(6, 7, [| |]),
                                                                    ASTNode.Name(8, 10, Token.Name(8, 9, "b", [| |]))
                                                                    );
                                                        ASTNode.Name(12, 13, Token.Name(12, 13, "a", [| |]));
                                                        ASTNode.ArgDiv(15, 17, Token.Div(15, 16, [| |]));
                                                        ASTNode.Name(19, 21, Token.Name(19, 20, "c", [| |]));
                                                        ASTNode.VarAssign(22, 27,
                                                                    ASTNode.Name(22, 23, Token.Name(22, 23, "e", [| |])),
                                                                    Token.Assign(23, 24, [| |]),
                                                                    ASTNode.Name(25, 27, Token.Name(25, 26, "f", [| |]))
                                                                    );
                                                        ASTNode.VarMul(29, 33, Token.Mul(29, 30, [| |]), ASTNode.Name(31, 33, Token.Name(31, 32, "g", [| |])));
                                                        ASTNode.Name(35, 37, Token.Name(35, 36, "h", [| |]));
                                                        ASTNode.VarAssign(39, 45,
                                                                    ASTNode.Name(39, 41, Token.Name(39, 40, "g", [| |])),
                                                                    Token.Assign(41, 42, [| |]),
                                                                    ASTNode.Name(43, 45, Token.Name(43, 44, "x", [| |]))
                                                                    );
                                                        ASTNode.VarPower(47, 52, Token.Power(47, 49, [| |]), ASTNode.Name(50, 52, Token.Name(50, 51, "y", [| |])))
                                                     |], [| 
                                                            Token.Comma(10, 11, [| |]);
                                                            Token.Comma(13, 14, [| |]);
                                                            Token.Comma(17, 18, [| |]);
                                                            Token.Comma(21, 22, [| |]);
                                                            Token.Comma(27, 28, [| |]);
                                                            Token.Comma(33, 34, [| |]);
                                                            Token.Comma(37, 38, [| |]);
                                                            Token.Comma(45, 46, [| |]);
                                                            Token.Comma(52, 53, [| |])
                                                        |], [| |]), parser.ParseVarArgsList())

    [<Fact>]
    let ``varargslist 12 test`` () =
            let lex = new MockTokenizer( [  ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Assign(6, 7, [| |]), 6 ); ( Token.Name(8, 9, "b", [| |]), 8 ); 
                                            ( Token.Comma(10, 11, [| |]), 10 ); ( Token.Name(12, 13, "a", [| |]), 12 );
                                            ( Token.Comma(13, 14, [| |]), 13 ); ( Token.Div(15, 16, [| |]), 15 );
                                            ( Token.Comma(17, 18, [| |]), 17 ); ( Token.Name(19, 20, "c", [| |]), 19 );
                                            ( Token.Comma(21, 22, [| |]), 21 ); ( Token.Name(22, 23, "e", [| |]), 22 ); ( Token.Assign(23, 24, [| |]), 23 ); ( Token.Name(25, 26, "f", [| |]), 25 );
                                            ( Token.Comma(27, 28, [| |]), 27 ); ( Token.Mul(29, 30, [| |]), 29 ); ( Token.Name(31, 32, "g", [| |]), 31 );
                                            ( Token.Comma(33, 34, [| |]), 33 ); ( Token.Name(35, 36, "h", [| |]), 35 );
                                            ( Token.Comma(37, 38, [| |]), 37 ); ( Token.Colon(39, 40, [| |]), 39 ); ( Token.EOF([| |]), 41 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.VarArgsList(0, 39, [| 
                                                        ASTNode.VarAssign(0, 10,
                                                                    ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])),
                                                                    Token.Assign(6, 7, [| |]),
                                                                    ASTNode.Name(8, 10, Token.Name(8, 9, "b", [| |]))
                                                                    );
                                                        ASTNode.Name(12, 13, Token.Name(12, 13, "a", [| |]));
                                                        ASTNode.ArgDiv(15, 17, Token.Div(15, 16, [| |]));
                                                        ASTNode.Name(19, 21, Token.Name(19, 20, "c", [| |]));
                                                        ASTNode.VarAssign(22, 27,
                                                                    ASTNode.Name(22, 23, Token.Name(22, 23, "e", [| |])),
                                                                    Token.Assign(23, 24, [| |]),
                                                                    ASTNode.Name(25, 27, Token.Name(25, 26, "f", [| |]))
                                                                    );
                                                        ASTNode.VarMul(29, 33, Token.Mul(29, 30, [| |]), ASTNode.Name(31, 33, Token.Name(31, 32, "g", [| |])));
                                                        ASTNode.Name(35, 37, Token.Name(35, 36, "h", [| |]));
                                                     |], [| 
                                                            Token.Comma(10, 11, [| |]);
                                                            Token.Comma(13, 14, [| |]);
                                                            Token.Comma(17, 18, [| |]);
                                                            Token.Comma(21, 22, [| |]);
                                                            Token.Comma(27, 28, [| |]);
                                                            Token.Comma(33, 34, [| |]);
                                                            Token.Comma(37, 38, [| |])
                                                        |], [| |]), parser.ParseVarArgsList())

    [<Fact>]
    let ``varargslist 13 test`` () =
            let lex = new MockTokenizer( [  ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Assign(6, 7, [| |]), 6 ); ( Token.Name(8, 9, "b", [| |]), 8 ); 
                                            ( Token.Comma(10, 11, [| |]), 10 ); ( Token.Name(12, 13, "a", [| |]), 12 );
                                            ( Token.Comma(13, 14, [| |]), 13 ); ( Token.Div(15, 16, [| |]), 15 );
                                            ( Token.Comma(17, 18, [| |]), 17 ); ( Token.Name(19, 20, "c", [| |]), 19 );
                                            ( Token.Comma(21, 22, [| |]), 21 ); ( Token.Name(22, 23, "e", [| |]), 22 ); ( Token.Assign(23, 24, [| |]), 23 ); ( Token.Name(25, 26, "f", [| |]), 25 );
                                            ( Token.Comma(27, 28, [| |]), 27 ); ( Token.Mul(29, 30, [| |]), 29 ); ( Token.Name(31, 32, "g", [| |]), 31 );
                                            ( Token.Comma(37, 38, [| |]), 37 ); ( Token.Colon(39, 40, [| |]), 39 ); ( Token.EOF([| |]), 41 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.VarArgsList(0, 39, [| 
                                                        ASTNode.VarAssign(0, 10,
                                                                    ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])),
                                                                    Token.Assign(6, 7, [| |]),
                                                                    ASTNode.Name(8, 10, Token.Name(8, 9, "b", [| |]))
                                                                    );
                                                        ASTNode.Name(12, 13, Token.Name(12, 13, "a", [| |]));
                                                        ASTNode.ArgDiv(15, 17, Token.Div(15, 16, [| |]));
                                                        ASTNode.Name(19, 21, Token.Name(19, 20, "c", [| |]));
                                                        ASTNode.VarAssign(22, 27,
                                                                    ASTNode.Name(22, 23, Token.Name(22, 23, "e", [| |])),
                                                                    Token.Assign(23, 24, [| |]),
                                                                    ASTNode.Name(25, 27, Token.Name(25, 26, "f", [| |]))
                                                                    );
                                                        ASTNode.VarMul(29, 37, Token.Mul(29, 30, [| |]), ASTNode.Name(31, 37, Token.Name(31, 32, "g", [| |])));
                                                        
                                                     |], [| 
                                                            Token.Comma(10, 11, [| |]);
                                                            Token.Comma(13, 14, [| |]);
                                                            Token.Comma(17, 18, [| |]);
                                                            Token.Comma(21, 22, [| |]);
                                                            Token.Comma(27, 28, [| |]);
                                                            Token.Comma(37, 38, [| |])
                                                        |], [| |]), parser.ParseVarArgsList())

    [<Fact>]
    let ``varargslist 14 test`` () =
            let lex = new MockTokenizer( [  ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Assign(6, 7, [| |]), 6 ); ( Token.Name(8, 9, "b", [| |]), 8 ); 
                                            ( Token.Comma(10, 11, [| |]), 10 ); ( Token.Name(12, 13, "a", [| |]), 12 );
                                            ( Token.Comma(13, 14, [| |]), 13 ); ( Token.Div(15, 16, [| |]), 15 );
                                            ( Token.Comma(17, 18, [| |]), 17 ); ( Token.Colon(19, 20, [| |]), 19 ); ( Token.EOF([| |]), 21 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.VarArgsList(0, 19, [| 
                                                        ASTNode.VarAssign(0, 10,
                                                                    ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])),
                                                                    Token.Assign(6, 7, [| |]),
                                                                    ASTNode.Name(8, 10, Token.Name(8, 9, "b", [| |]))
                                                                    );
                                                        ASTNode.Name(12, 13, Token.Name(12, 13, "a", [| |]));
                                                        ASTNode.ArgDiv(15, 17, Token.Div(15, 16, [| |]));
                                                        
                                                     |], [| 
                                                            Token.Comma(10, 11, [| |]);
                                                            Token.Comma(13, 14, [| |]);
                                                            Token.Comma(17, 18, [| |])
                                                        |], [| |]), parser.ParseVarArgsList())

    [<Fact>]
    let ``varargslist 15 test`` () =
            let lex = new MockTokenizer( [  ( Token.Mul(0, 1, [| |]), 0 ); ( Token.Name(2, 3, "g", [| |]), 2 );
                                            ( Token.Comma(4, 5, [| |]), 4 ); ( Token.Name(6, 7, "h", [| |]), 6 );
                                            ( Token.Comma(8, 9, [| |]), 8 ); ( Token.Name(10, 11, "g", [| |]), 10 ); ( Token.Assign(12, 13, [| |]), 12 ); ( Token.Name(14, 15, "x", [| |]), 14 );
                                            ( Token.Comma(16, 17, [| |]), 16 ); ( Token.Power(18, 20, [| |]), 18 ); ( Token.Name(21, 22, "y", [| |]), 21 ); 
                                            ( Token.Comma(23, 24, [| |]), 23 ); ( Token.EOF([| |]), 25 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.VarArgsList(0, 25, [| 
                                                        ASTNode.VarMul(0, 4, Token.Mul(0, 1, [| |]), ASTNode.Name(2, 4, Token.Name(2, 3, "g", [| |])));
                                                        ASTNode.Name(6, 8, Token.Name(6, 7, "h", [| |]));
                                                        ASTNode.VarAssign(10, 16,
                                                                    ASTNode.Name(10, 12, Token.Name(10, 11, "g", [| |])),
                                                                    Token.Assign(12, 13, [| |]),
                                                                    ASTNode.Name(14, 16, Token.Name(14, 15, "x", [| |]))
                                                                    );
                                                        ASTNode.VarPower(18, 23, Token.Power(18, 20, [| |]), ASTNode.Name(21, 23, Token.Name(21, 22, "y", [| |])))
                                                     |], [| 
                                                            Token.Comma(4, 5, [| |]);
                                                            Token.Comma(8, 9, [| |]);
                                                            Token.Comma(16, 17, [| |]);
                                                            Token.Comma(23, 24, [| |])
                                                        |], [| |]), parser.ParseVarArgsList())

    [<Fact>]
    let ``varargslist 16 test`` () =
            let lex = new MockTokenizer( [  ( Token.Mul(0, 1, [| |]), 0 ); ( Token.Name(2, 3, "g", [| |]), 2 );
                                            ( Token.Comma(4, 5, [| |]), 4 ); ( Token.Name(6, 7, "h", [| |]), 6 );
                                            ( Token.Comma(8, 9, [| |]), 8 ); ( Token.Name(10, 11, "g", [| |]), 10 ); ( Token.Assign(12, 13, [| |]), 12 ); ( Token.Name(14, 15, "x", [| |]), 14 );
                                            ( Token.Comma(16, 17, [| |]), 16 ); ( Token.Colon(18, 19, [| |]), 18 ); ( Token.EOF([| |]), 20 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.VarArgsList(0, 18, [| 
                                                        ASTNode.VarMul(0, 4, Token.Mul(0, 1, [| |]), ASTNode.Name(2, 4, Token.Name(2, 3, "g", [| |])));
                                                        ASTNode.Name(6, 8, Token.Name(6, 7, "h", [| |]));
                                                        ASTNode.VarAssign(10, 16,
                                                                    ASTNode.Name(10, 12, Token.Name(10, 11, "g", [| |])),
                                                                    Token.Assign(12, 13, [| |]),
                                                                    ASTNode.Name(14, 16, Token.Name(14, 15, "x", [| |]))
                                                                    );
                                                        
                                                     |], [| 
                                                            Token.Comma(4, 5, [| |]);
                                                            Token.Comma(8, 9, [| |]);
                                                            Token.Comma(16, 17, [| |]);
                                                           
                                                        |], [| |]), parser.ParseVarArgsList())

    [<Fact>]
    let ``varargslist 17 test`` () =
            let lex = new MockTokenizer( [  ( Token.Mul(0, 1, [| |]), 0 ); ( Token.Name(2, 3, "g", [| |]), 2 );
                                            ( Token.Comma(4, 5, [| |]), 4 ); ( Token.Name(6, 7, "h", [| |]), 6 );
                                            ( Token.Comma(8, 9, [| |]), 8 ); ( Token.Name(10, 11, "g", [| |]), 10 ); ( Token.Assign(12, 13, [| |]), 12 ); ( Token.Name(14, 15, "x", [| |]), 14 );
                                            ( Token.EOF([| |]), 16 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.VarArgsList(0, 16, [| 
                                                        ASTNode.VarMul(0, 4, Token.Mul(0, 1, [| |]), ASTNode.Name(2, 4, Token.Name(2, 3, "g", [| |])));
                                                        ASTNode.Name(6, 8, Token.Name(6, 7, "h", [| |]));
                                                        ASTNode.VarAssign(10, 16,
                                                                    ASTNode.Name(10, 12, Token.Name(10, 11, "g", [| |])),
                                                                    Token.Assign(12, 13, [| |]),
                                                                    ASTNode.Name(14, 16, Token.Name(14, 15, "x", [| |]))
                                                                    );
                                                     |], [| 
                                                            Token.Comma(4, 5, [| |]);
                                                            Token.Comma(8, 9, [| |]);
                                                        |], [| |]), parser.ParseVarArgsList())

    [<Fact>]
    let ``varargslist 18 test`` () =
            let lex = new MockTokenizer( [  ( Token.Mul(0, 1, [| |]), 0 ); ( Token.Name(2, 3, "g", [| |]), 2 );
                                            ( Token.Comma(4, 5, [| |]), 4 ); ( Token.Colon(6, 7, [| |]), 6 );
                                            ( Token.EOF([| |]), 8 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.VarArgsList(0, 6, [| 
                                                        ASTNode.VarMul(0, 4, Token.Mul(0, 1, [| |]), ASTNode.Name(2, 4, Token.Name(2, 3, "g", [| |])));
                                                     |], [| 
                                                            Token.Comma(4, 5, [| |])
                                                        |], [| |]), parser.ParseVarArgsList())

    [<Fact>]
    let ``varargslist 19 test`` () =
            let lex = new MockTokenizer( [  ( Token.Mul(0, 1, [| |]), 0 ); ( Token.Name(2, 3, "g", [| |]), 2 );
                                            ( Token.EOF([| |]), 4 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.VarArgsList(0, 4, [| 
                                                        ASTNode.VarMul(0, 4, Token.Mul(0, 1, [| |]), ASTNode.Name(2, 4, Token.Name(2, 3, "g", [| |])));
                                                     |], [| 
                                                            
                                                        |], [| |]), parser.ParseVarArgsList())

    [<Fact>]
    let ``varargslist 20 test`` () =
            let lex = new MockTokenizer( [  ( Token.Power(0, 2, [| |]), 0 ); ( Token.Name(2, 3, "g", [| |]), 2 );
                                            ( Token.Comma(4, 5, [| |]), 4 ); ( Token.Colon(6, 7, [| |]), 6 );
                                            ( Token.EOF([| |]), 8 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.VarArgsList(0, 6, [| 
                                                        ASTNode.VarPower(0, 4, Token.Power(0, 2, [| |]), ASTNode.Name(2, 4, Token.Name(2, 3, "g", [| |])));
                                                     |], [| 
                                                            Token.Comma(4, 5, [| |])
                                                        |], [| |]), parser.ParseVarArgsList())

    [<Fact>]
    let ``varargslist 21 test`` () =
            let lex = new MockTokenizer( [  ( Token.Power(0, 2, [| |]), 0 ); ( Token.Name(2, 3, "g", [| |]), 2 );
                                            ( Token.EOF([| |]), 4 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.VarArgsList(0, 4, [| 
                                                        ASTNode.VarPower(0, 4, Token.Power(0, 2, [| |]), ASTNode.Name(2, 4, Token.Name(2, 3, "g", [| |])));
                                                     |], [| 
                                                            
                                                        |], [| |]), parser.ParseVarArgsList())

    [<Fact>]
    let ``typeargslist 1 test`` () =
            let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.EOF([| |]), 6 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.TypedArgsList(0, 6, [| ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |]))  |], [| |], [| |]), parser.ParseTypedArgsList())

    [<Fact>]
    let ``typeargslist 2 test`` () =
            let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Assign(6, 7, [| |]), 6 ); ( Token.Name(8, 9, "b", [| |]), 8 ); ( Token.EOF([| |]), 10 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.TypedArgsList(0, 10, [|   ASTNode.TypedAssign(0, 10,
                                                                                    ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])),
                                                                                    Token.Assign(6, 7, [| |]),
                                                                                    ASTNode.Name(8, 10, Token.Name(8, 9, "b", [| |])) )
                                                        |], [| |], [| |]), parser.ParseTypedArgsList())

    [<Fact>]
    let ``typeargslist 3 test`` () =
            let lex = new MockTokenizer( [ ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Colon(6, 7, [| |]), 6 ); ( Token.Name(8, 9, "c", [| |]), 8 ); ( Token.Assign(10, 11, [| |]), 10 ); ( Token.Name(12, 13, "b", [| |]), 12 ); ( Token.EOF([| |]), 14 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.TypedArgsList(0, 14, [|   ASTNode.TypedAssign(0, 14,
                                                                                    ASTNode.TFPDef(0, 10,
                                                                                        ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])),
                                                                                        Token.Colon(6, 7, [| |]),
                                                                                        ASTNode.Name(8, 10, Token.Name(8, 9, "c", [| |]))),
                                                                                    Token.Assign(10, 11, [| |]),
                                                                                    ASTNode.Name(12, 14, Token.Name(12, 13, "b", [| |])) )
                                                        |], [| |], [| |]), parser.ParseTypedArgsList())

    [<Fact>]
    let ``typeargslist 4 test`` () =
            let lex = new MockTokenizer( [  ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Colon(6, 7, [| |]), 6 ); ( Token.Name(8, 9, "c", [| |]), 8 ); ( Token.Assign(10, 11, [| |]), 10 ); ( Token.Name(12, 13, "b", [| |]), 12 ); 
                                            ( Token.Comma(14, 15, [| |]), 14 ); ( Token.TypeComment(16, 26, "#type: int"), 16 ); ( Token.Name(27, 28, "d", [| |]), 27 );
                                            ( Token.EOF([| |]), 29 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.TypedArgsList(0, 29, [|   ASTNode.TypedAssign(0, 14,
                                                                                    ASTNode.TFPDef(0, 10,
                                                                                        ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])),
                                                                                        Token.Colon(6, 7, [| |]),
                                                                                        ASTNode.Name(8, 10, Token.Name(8, 9, "c", [| |]))),
                                                                                    Token.Assign(10, 11, [| |]),
                                                                                    ASTNode.Name(12, 14, Token.Name(12, 13, "b", [| |])) );
                                                            ASTNode.Name(27, 29, Token.Name(27, 28, "d", [| |]))
                                                        |], [| Token.Comma(14, 15, [| |]) |], [| Token.TypeComment(16, 26, "#type: int") |]), parser.ParseTypedArgsList())

    [<Fact>]
    let ``typeargslist 5 test`` () =
            let lex = new MockTokenizer( [  ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Colon(6, 7, [| |]), 6 ); ( Token.Name(8, 9, "c", [| |]), 8 ); ( Token.Assign(10, 11, [| |]), 10 ); ( Token.Name(12, 13, "b", [| |]), 12 ); 
                                            ( Token.Comma(14, 15, [| |]), 14 ); ( Token.Name(27, 28, "d", [| |]), 27 );
                                            ( Token.EOF([| |]), 29 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.TypedArgsList(0, 29, [|   ASTNode.TypedAssign(0, 14,
                                                                                    ASTNode.TFPDef(0, 10,
                                                                                        ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])),
                                                                                        Token.Colon(6, 7, [| |]),
                                                                                        ASTNode.Name(8, 10, Token.Name(8, 9, "c", [| |]))),
                                                                                    Token.Assign(10, 11, [| |]),
                                                                                    ASTNode.Name(12, 14, Token.Name(12, 13, "b", [| |])) );
                                                            ASTNode.Name(27, 29, Token.Name(27, 28, "d", [| |]))
                                                        |], [| Token.Comma(14, 15, [| |]) |], [|  |]), parser.ParseTypedArgsList())

    [<Fact>]
    let ``typeargslist 6 test`` () =
            let lex = new MockTokenizer( [  ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Colon(6, 7, [| |]), 6 ); ( Token.Name(8, 9, "c", [| |]), 8 ); ( Token.Assign(10, 11, [| |]), 10 ); ( Token.Name(12, 13, "b", [| |]), 12 ); 
                                            ( Token.Comma(14, 15, [| |]), 14 ); ( Token.TypeComment(16, 26, "#type: int"), 16 ); ( Token.Name(27, 28, "d", [| |]), 27 );
                                            ( Token.Comma(29, 30, [| |]), 29 ); ( Token.Div(31, 32, [| |]), 31 );
                                            ( Token.EOF([| |]), 33 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.TypedArgsList(0, 33, [|   ASTNode.TypedAssign(0, 14,
                                                                                    ASTNode.TFPDef(0, 10,
                                                                                        ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])),
                                                                                        Token.Colon(6, 7, [| |]),
                                                                                        ASTNode.Name(8, 10, Token.Name(8, 9, "c", [| |]))),
                                                                                    Token.Assign(10, 11, [| |]),
                                                                                    ASTNode.Name(12, 14, Token.Name(12, 13, "b", [| |])) );
                                                            ASTNode.Name(27, 29, Token.Name(27, 28, "d", [| |]));
                                                            ASTNode.ArgDiv(31, 33, Token.Div(31, 32, [| |]))
                                                        |], 
                                                        [| 
                                                            Token.Comma(14, 15, [| |]);
                                                            Token.Comma(29, 30, [| |])
                                                        |], 
                                                        [| 
                                                            Token.TypeComment(16, 26, "#type: int") 
                                                        |]), parser.ParseTypedArgsList())

    [<Fact>]
    let ``typeargslist 7 test`` () =
            let lex = new MockTokenizer( [  ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Colon(6, 7, [| |]), 6 ); ( Token.Name(8, 9, "c", [| |]), 8 ); ( Token.Assign(10, 11, [| |]), 10 ); ( Token.Name(12, 13, "b", [| |]), 12 ); 
                                            ( Token.Comma(14, 15, [| |]), 14 ); ( Token.TypeComment(16, 26, "#type: int"), 16 ); ( Token.Name(27, 28, "d", [| |]), 27 );
                                            ( Token.Comma(29, 30, [| |]), 29 ); ( Token.TypeComment(31, 41, "#type: int"), 31 ); ( Token.Div(42, 43, [| |]), 42 );
                                            ( Token.EOF([| |]), 43 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.TypedArgsList(0, 43, [|   ASTNode.TypedAssign(0, 14,
                                                                                    ASTNode.TFPDef(0, 10,
                                                                                        ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])),
                                                                                        Token.Colon(6, 7, [| |]),
                                                                                        ASTNode.Name(8, 10, Token.Name(8, 9, "c", [| |]))),
                                                                                    Token.Assign(10, 11, [| |]),
                                                                                    ASTNode.Name(12, 14, Token.Name(12, 13, "b", [| |])) );
                                                            ASTNode.Name(27, 29, Token.Name(27, 28, "d", [| |]));
                                                            ASTNode.ArgDiv(42, 43, Token.Div(42, 43, [| |]))
                                                        |], 
                                                        [| 
                                                            Token.Comma(14, 15, [| |]);
                                                            Token.Comma(29, 30, [| |])
                                                        |], 
                                                        [| 
                                                            Token.TypeComment(16, 26, "#type: int");
                                                            Token.TypeComment(31, 41, "#type: int")
                                                        |]), parser.ParseTypedArgsList())

    [<Fact>]
    let ``typeargslist 8 test`` () =
            let lex = new MockTokenizer( [  ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Colon(6, 7, [| |]), 6 ); ( Token.Name(8, 9, "c", [| |]), 8 ); ( Token.Assign(10, 11, [| |]), 10 ); ( Token.Name(12, 13, "b", [| |]), 12 ); 
                                            ( Token.Comma(14, 15, [| |]), 14 ); ( Token.TypeComment(16, 26, "#type: int"), 16 ); ( Token.Name(27, 28, "d", [| |]), 27 );
                                            ( Token.Comma(29, 30, [| |]), 29 ); ( Token.TypeComment(31, 41, "#type: int"), 31 ); ( Token.Div(42, 43, [| |]), 42 );
                                            ( Token.Comma(43, 44, [| |]), 43 ); ( Token.Name(45, 46, "e", [| |]), 45 ); 
                                            ( Token.Comma(47, 48, [| |]), 47  ); ( Token.RightParen(49, 50, [| |]), 49 );
                                            ( Token.EOF([| |]), 51 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.TypedArgsList(0, 49, [|   ASTNode.TypedAssign(0, 14,
                                                                                    ASTNode.TFPDef(0, 10,
                                                                                        ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])),
                                                                                        Token.Colon(6, 7, [| |]),
                                                                                        ASTNode.Name(8, 10, Token.Name(8, 9, "c", [| |]))),
                                                                                    Token.Assign(10, 11, [| |]),
                                                                                    ASTNode.Name(12, 14, Token.Name(12, 13, "b", [| |])) );
                                                            ASTNode.Name(27, 29, Token.Name(27, 28, "d", [| |]));
                                                            ASTNode.ArgDiv(42, 43, Token.Div(42, 43, [| |]));
                                                            ASTNode.Name(45, 47, Token.Name(45, 46, "e", [| |]))
                                                        |], 
                                                        [| 
                                                            Token.Comma(14, 15, [| |]);
                                                            Token.Comma(29, 30, [| |]);
                                                            Token.Comma(43, 44, [| |]);
                                                            Token.Comma(47, 48, [| |])
                                                        |], 
                                                        [| 
                                                            Token.TypeComment(16, 26, "#type: int");
                                                            Token.TypeComment(31, 41, "#type: int")
                                                        |]), parser.ParseTypedArgsList())

    [<Fact>]
    let ``typeargslist 9 test`` () =
            let lex = new MockTokenizer( [  ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Colon(6, 7, [| |]), 6 ); ( Token.Name(8, 9, "c", [| |]), 8 ); ( Token.Assign(10, 11, [| |]), 10 ); ( Token.Name(12, 13, "b", [| |]), 12 ); 
                                            ( Token.Comma(14, 15, [| |]), 14 ); ( Token.TypeComment(16, 26, "#type: int"), 16 ); ( Token.Name(27, 28, "d", [| |]), 27 );
                                            ( Token.Comma(29, 30, [| |]), 29 ); ( Token.TypeComment(31, 41, "#type: int"), 31 ); ( Token.Div(42, 43, [| |]), 42 );
                                            ( Token.Comma(43, 44, [| |]), 43 ); ( Token.Name(45, 46, "e", [| |]), 45 );
                                            ( Token.Comma(47, 48, [| |]), 47 ); ( Token.TypeComment(49, 59, "#type: int"), 49 ); ( Token.Name(60, 61, "g", [| |]), 60 ); ( Token.Colon(62, 63, [| |]), 62 ); ( Token.Name(64, 65, "h", [| |]), 64 ); ( Token.Assign(66, 67, [| |]), 66 ); ( Token.Name(68, 69, "i", [| |]), 68 );
                                            ( Token.Comma(70, 71, [| |]), 70  ); ( Token.RightParen(72, 73, [| |]), 72 );
                                            ( Token.EOF([| |]), 74 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.TypedArgsList(0, 72, [|   ASTNode.TypedAssign(0, 14,
                                                                                    ASTNode.TFPDef(0, 10,
                                                                                        ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])),
                                                                                        Token.Colon(6, 7, [| |]),
                                                                                        ASTNode.Name(8, 10, Token.Name(8, 9, "c", [| |]))),
                                                                                    Token.Assign(10, 11, [| |]),
                                                                                    ASTNode.Name(12, 14, Token.Name(12, 13, "b", [| |])) );
                                                            ASTNode.Name(27, 29, Token.Name(27, 28, "d", [| |]));
                                                            ASTNode.ArgDiv(42, 43, Token.Div(42, 43, [| |]));
                                                            ASTNode.Name(45, 47, Token.Name(45, 46, "e", [| |]));
                                                            ASTNode.TypedAssign(60, 70,
                                                                                    ASTNode.TFPDef(60, 66,
                                                                                        ASTNode.Name(60, 62, Token.Name(60, 61, "g", [| |])),
                                                                                        Token.Colon(62, 63, [| |]),
                                                                                        ASTNode.Name(64, 66, Token.Name(64, 65, "h", [| |]))),
                                                                                    Token.Assign(66, 67, [| |]),
                                                                                    ASTNode.Name(68, 70, Token.Name(68, 69, "i", [| |])) );
                                                        |], 
                                                        [| 
                                                            Token.Comma(14, 15, [| |]);
                                                            Token.Comma(29, 30, [| |]);
                                                            Token.Comma(43, 44, [| |]);
                                                            Token.Comma(47, 48, [| |]);
                                                            Token.Comma(70, 71, [| |])
                                                        |], 
                                                        [| 
                                                            Token.TypeComment(16, 26, "#type: int");
                                                            Token.TypeComment(31, 41, "#type: int");
                                                            Token.TypeComment(49, 59, "#type: int")
                                                        |]), parser.ParseTypedArgsList())

    [<Fact>]
    let ``typeargslist 10 test`` () =
            let lex = new MockTokenizer( [  ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Colon(6, 7, [| |]), 6 ); ( Token.Name(8, 9, "c", [| |]), 8 ); ( Token.Assign(10, 11, [| |]), 10 ); ( Token.Name(12, 13, "b", [| |]), 12 ); 
                                            ( Token.Comma(14, 15, [| |]), 14 ); ( Token.TypeComment(16, 26, "#type: int"), 16 ); ( Token.Name(27, 28, "d", [| |]), 27 );
                                            ( Token.Comma(29, 30, [| |]), 29 ); ( Token.TypeComment(31, 41, "#type: int"), 31 ); ( Token.Div(42, 43, [| |]), 42 );
                                            ( Token.Comma(43, 44, [| |]), 43 ); ( Token.Name(45, 46, "e", [| |]), 45 );
                                            ( Token.Comma(47, 48, [| |]), 47 ); ( Token.TypeComment(49, 59, "#type: int"), 49 ); ( Token.Name(60, 61, "g", [| |]), 60 ); ( Token.Colon(62, 63, [| |]), 62 ); ( Token.Name(64, 65, "h", [| |]), 64 ); ( Token.Assign(66, 67, [| |]), 66 ); ( Token.Name(68, 69, "i", [| |]), 68 );
                                            ( Token.Comma(70, 71, [| |]), 70  ); ( Token.Mul(72, 73, [| |]), 72 ); ( Token.Name(73, 74, "a", [| |]), 73 ); ( Token.Colon(75, 76, [| |]), 75 ); ( Token.Name(77, 78, "b", [| |]), 77 );
                                            ( Token.RightParen(79, 80, [| |]), 79 ); ( Token.EOF([| |]), 81 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.TypedArgsList(0, 79, [|   ASTNode.TypedAssign(0, 14,
                                                                                    ASTNode.TFPDef(0, 10,
                                                                                        ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])),
                                                                                        Token.Colon(6, 7, [| |]),
                                                                                        ASTNode.Name(8, 10, Token.Name(8, 9, "c", [| |]))),
                                                                                    Token.Assign(10, 11, [| |]),
                                                                                    ASTNode.Name(12, 14, Token.Name(12, 13, "b", [| |])) );
                                                            ASTNode.Name(27, 29, Token.Name(27, 28, "d", [| |]));
                                                            ASTNode.ArgDiv(42, 43, Token.Div(42, 43, [| |]));
                                                            ASTNode.Name(45, 47, Token.Name(45, 46, "e", [| |]));
                                                            ASTNode.TypedAssign(60, 70,
                                                                                    ASTNode.TFPDef(60, 66,
                                                                                        ASTNode.Name(60, 62, Token.Name(60, 61, "g", [| |])),
                                                                                        Token.Colon(62, 63, [| |]),
                                                                                        ASTNode.Name(64, 66, Token.Name(64, 65, "h", [| |]))),
                                                                                    Token.Assign(66, 67, [| |]),
                                                                                    ASTNode.Name(68, 70, Token.Name(68, 69, "i", [| |])) );
                                                            ASTNode.TypedMul(72, 79, 
                                                                                    Token.Mul(72, 73, [| |]),
                                                                                    ASTNode.TFPDef(73, 79,
                                                                                        ASTNode.Name(73, 75, Token.Name(73, 74, "a", [| |])),
                                                                                        Token.Colon(75, 76, [| |]),
                                                                                        ASTNode.Name(77, 79, Token.Name(77, 78, "b", [| |]))
                                                            ));
                                                        |], 
                                                        [| 
                                                            Token.Comma(14, 15, [| |]);
                                                            Token.Comma(29, 30, [| |]);
                                                            Token.Comma(43, 44, [| |]);
                                                            Token.Comma(47, 48, [| |]);
                                                            Token.Comma(70, 71, [| |])
                                                        |], 
                                                        [| 
                                                            Token.TypeComment(16, 26, "#type: int");
                                                            Token.TypeComment(31, 41, "#type: int");
                                                            Token.TypeComment(49, 59, "#type: int")
                                                        |]), parser.ParseTypedArgsList())

    [<Fact>]
    let ``typeargslist 11 test`` () =
            let lex = new MockTokenizer( [  ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Colon(6, 7, [| |]), 6 ); ( Token.Name(8, 9, "c", [| |]), 8 ); ( Token.Assign(10, 11, [| |]), 10 ); ( Token.Name(12, 13, "b", [| |]), 12 ); 
                                            ( Token.Comma(14, 15, [| |]), 14 ); ( Token.TypeComment(16, 26, "#type: int"), 16 ); ( Token.Name(27, 28, "d", [| |]), 27 );
                                            ( Token.Comma(29, 30, [| |]), 29 ); ( Token.TypeComment(31, 41, "#type: int"), 31 ); ( Token.Div(42, 43, [| |]), 42 );
                                            ( Token.Comma(43, 44, [| |]), 43 ); ( Token.Name(45, 46, "e", [| |]), 45 );
                                            ( Token.Comma(47, 48, [| |]), 47 ); ( Token.TypeComment(49, 59, "#type: int"), 49 ); ( Token.Name(60, 61, "g", [| |]), 60 ); ( Token.Colon(62, 63, [| |]), 62 ); ( Token.Name(64, 65, "h", [| |]), 64 ); ( Token.Assign(66, 67, [| |]), 66 ); ( Token.Name(68, 69, "i", [| |]), 68 );
                                            ( Token.Comma(70, 71, [| |]), 70  ); ( Token.Mul(72, 73, [| |]), 72 ); ( Token.Name(73, 74, "a", [| |]), 73 ); ( Token.Colon(75, 76, [| |]), 75 ); ( Token.Name(77, 78, "b", [| |]), 77 );
                                            ( Token.Comma(79, 80, [| |]), 79 ); ( Token.RightParen(81, 82, [| |]), 81 ); ( Token.EOF([| |]), 83 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.TypedArgsList(0, 81, [|   ASTNode.TypedAssign(0, 14,
                                                                                    ASTNode.TFPDef(0, 10,
                                                                                        ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])),
                                                                                        Token.Colon(6, 7, [| |]),
                                                                                        ASTNode.Name(8, 10, Token.Name(8, 9, "c", [| |]))),
                                                                                    Token.Assign(10, 11, [| |]),
                                                                                    ASTNode.Name(12, 14, Token.Name(12, 13, "b", [| |])) );
                                                            ASTNode.Name(27, 29, Token.Name(27, 28, "d", [| |]));
                                                            ASTNode.ArgDiv(42, 43, Token.Div(42, 43, [| |]));
                                                            ASTNode.Name(45, 47, Token.Name(45, 46, "e", [| |]));
                                                            ASTNode.TypedAssign(60, 70,
                                                                                    ASTNode.TFPDef(60, 66,
                                                                                        ASTNode.Name(60, 62, Token.Name(60, 61, "g", [| |])),
                                                                                        Token.Colon(62, 63, [| |]),
                                                                                        ASTNode.Name(64, 66, Token.Name(64, 65, "h", [| |]))),
                                                                                    Token.Assign(66, 67, [| |]),
                                                                                    ASTNode.Name(68, 70, Token.Name(68, 69, "i", [| |])) );
                                                            ASTNode.TypedMul(72, 79, 
                                                                                    Token.Mul(72, 73, [| |]),
                                                                                    ASTNode.TFPDef(73, 79,
                                                                                        ASTNode.Name(73, 75, Token.Name(73, 74, "a", [| |])),
                                                                                        Token.Colon(75, 76, [| |]),
                                                                                        ASTNode.Name(77, 79, Token.Name(77, 78, "b", [| |]))
                                                            ));
                                                        |], 
                                                        [| 
                                                            Token.Comma(14, 15, [| |]);
                                                            Token.Comma(29, 30, [| |]);
                                                            Token.Comma(43, 44, [| |]);
                                                            Token.Comma(47, 48, [| |]);
                                                            Token.Comma(70, 71, [| |]);
                                                            Token.Comma(79, 80, [| |])
                                                        |], 
                                                        [| 
                                                            Token.TypeComment(16, 26, "#type: int");
                                                            Token.TypeComment(31, 41, "#type: int");
                                                            Token.TypeComment(49, 59, "#type: int")
                                                        |]), parser.ParseTypedArgsList())

    [<Fact>]
    let ``typeargslist 12 test`` () =
            let lex = new MockTokenizer( [  ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Colon(6, 7, [| |]), 6 ); ( Token.Name(8, 9, "c", [| |]), 8 ); ( Token.Assign(10, 11, [| |]), 10 ); ( Token.Name(12, 13, "b", [| |]), 12 ); 
                                            ( Token.Comma(14, 15, [| |]), 14 ); ( Token.TypeComment(16, 26, "#type: int"), 16 ); ( Token.Name(27, 28, "d", [| |]), 27 );
                                            ( Token.Comma(29, 30, [| |]), 29 ); ( Token.TypeComment(31, 41, "#type: int"), 31 ); ( Token.Div(42, 43, [| |]), 42 );
                                            ( Token.Comma(43, 44, [| |]), 43 ); ( Token.Name(45, 46, "e", [| |]), 45 );
                                            ( Token.Comma(47, 48, [| |]), 47 ); ( Token.TypeComment(49, 59, "#type: int"), 49 ); ( Token.Name(60, 61, "g", [| |]), 60 ); ( Token.Colon(62, 63, [| |]), 62 ); ( Token.Name(64, 65, "h", [| |]), 64 ); ( Token.Assign(66, 67, [| |]), 66 ); ( Token.Name(68, 69, "i", [| |]), 68 );
                                            ( Token.Comma(70, 71, [| |]), 70  ); ( Token.Mul(72, 73, [| |]), 72 ); ( Token.Name(73, 74, "a", [| |]), 73 ); ( Token.Colon(75, 76, [| |]), 75 ); ( Token.Name(77, 78, "b", [| |]), 77 );
                                            ( Token.Comma(79, 80, [| |]), 79 ); ( Token.Name(81, 82, "a", [| |]), 81 ); ( Token.Colon(83, 84, [| |]), 83 ); ( Token.Name(85, 86, "b", [| |]), 85 ); 
                                            ( Token.RightParen(87, 88, [| |]), 87 ); ( Token.EOF([| |]), 89 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.TypedArgsList(0, 87, [|   ASTNode.TypedAssign(0, 14,
                                                                                    ASTNode.TFPDef(0, 10,
                                                                                        ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])),
                                                                                        Token.Colon(6, 7, [| |]),
                                                                                        ASTNode.Name(8, 10, Token.Name(8, 9, "c", [| |]))),
                                                                                    Token.Assign(10, 11, [| |]),
                                                                                    ASTNode.Name(12, 14, Token.Name(12, 13, "b", [| |])) );
                                                            ASTNode.Name(27, 29, Token.Name(27, 28, "d", [| |]));
                                                            ASTNode.ArgDiv(42, 43, Token.Div(42, 43, [| |]));
                                                            ASTNode.Name(45, 47, Token.Name(45, 46, "e", [| |]));
                                                            ASTNode.TypedAssign(60, 70,
                                                                                    ASTNode.TFPDef(60, 66,
                                                                                        ASTNode.Name(60, 62, Token.Name(60, 61, "g", [| |])),
                                                                                        Token.Colon(62, 63, [| |]),
                                                                                        ASTNode.Name(64, 66, Token.Name(64, 65, "h", [| |]))),
                                                                                    Token.Assign(66, 67, [| |]),
                                                                                    ASTNode.Name(68, 70, Token.Name(68, 69, "i", [| |])) );
                                                            ASTNode.TypedMul(72, 79, 
                                                                                    Token.Mul(72, 73, [| |]),
                                                                                    ASTNode.TFPDef(73, 79,
                                                                                        ASTNode.Name(73, 75, Token.Name(73, 74, "a", [| |])),
                                                                                        Token.Colon(75, 76, [| |]),
                                                                                        ASTNode.Name(77, 79, Token.Name(77, 78, "b", [| |]))
                                                            ));
                                                            ASTNode.TFPDef(81, 87,  
                                                                                ASTNode.Name(81, 83, Token.Name(81, 82, "a", [| |])),
                                                                                Token.Colon(83, 84, [| |]),
                                                                                ASTNode.Name(85, 87, Token.Name(85, 86, "b", [| |])) )
                                                        |], 
                                                        [| 
                                                            Token.Comma(14, 15, [| |]);
                                                            Token.Comma(29, 30, [| |]);
                                                            Token.Comma(43, 44, [| |]);
                                                            Token.Comma(47, 48, [| |]);
                                                            Token.Comma(70, 71, [| |]);
                                                            Token.Comma(79, 80, [| |])
                                                        |], 
                                                        [| 
                                                            Token.TypeComment(16, 26, "#type: int");
                                                            Token.TypeComment(31, 41, "#type: int");
                                                            Token.TypeComment(49, 59, "#type: int")
                                                        |]), parser.ParseTypedArgsList())

    [<Fact>]
    let ``typeargslist 13 test`` () =
            let lex = new MockTokenizer( [  ( Token.Name(0, 5, "Test1", [| |]), 0 ); ( Token.Colon(6, 7, [| |]), 6 ); ( Token.Name(8, 9, "c", [| |]), 8 ); ( Token.Assign(10, 11, [| |]), 10 ); ( Token.Name(12, 13, "b", [| |]), 12 ); 
                                            ( Token.Comma(14, 15, [| |]), 14 ); ( Token.TypeComment(16, 26, "#type: int"), 16 ); ( Token.Name(27, 28, "d", [| |]), 27 );
                                            ( Token.Comma(29, 30, [| |]), 29 ); ( Token.TypeComment(31, 41, "#type: int"), 31 ); ( Token.Div(42, 43, [| |]), 42 );
                                            ( Token.Comma(43, 44, [| |]), 43 ); ( Token.Name(45, 46, "e", [| |]), 45 );
                                            ( Token.Comma(47, 48, [| |]), 47 ); ( Token.TypeComment(49, 59, "#type: int"), 49 ); ( Token.Name(60, 61, "g", [| |]), 60 ); ( Token.Colon(62, 63, [| |]), 62 ); ( Token.Name(64, 65, "h", [| |]), 64 ); ( Token.Assign(66, 67, [| |]), 66 ); ( Token.Name(68, 69, "i", [| |]), 68 );
                                            ( Token.Comma(70, 71, [| |]), 70  ); ( Token.Mul(72, 73, [| |]), 72 ); ( Token.Name(73, 74, "a", [| |]), 73 ); ( Token.Colon(75, 76, [| |]), 75 ); ( Token.Name(77, 78, "b", [| |]), 77 );
                                            ( Token.Comma(79, 80, [| |]), 79 ); ( Token.Name(81, 82, "a", [| |]), 81 ); ( Token.Colon(83, 84, [| |]), 83 ); ( Token.Name(85, 86, "b", [| |]), 85 ); 
                                            ( Token.Comma(87, 88, [| |]), 87 ); ( Token.Power(89, 91, [| |]), 89 ); ( Token.Name(91, 92, "c", [| |]), 91 );
                                            ( Token.RightParen(93, 94, [| |]), 93 ); ( Token.EOF([| |]), 95 ) ] )
            lex.Next()
            let parser = new Parser(lex)
            Assert.Equal( ASTNode.TypedArgsList(0, 93, [|   ASTNode.TypedAssign(0, 14,
                                                                                    ASTNode.TFPDef(0, 10,
                                                                                        ASTNode.Name(0, 6, Token.Name(0, 5, "Test1", [| |])),
                                                                                        Token.Colon(6, 7, [| |]),
                                                                                        ASTNode.Name(8, 10, Token.Name(8, 9, "c", [| |]))),
                                                                                    Token.Assign(10, 11, [| |]),
                                                                                    ASTNode.Name(12, 14, Token.Name(12, 13, "b", [| |])) );
                                                            ASTNode.Name(27, 29, Token.Name(27, 28, "d", [| |]));
                                                            ASTNode.ArgDiv(42, 43, Token.Div(42, 43, [| |]));
                                                            ASTNode.Name(45, 47, Token.Name(45, 46, "e", [| |]));
                                                            ASTNode.TypedAssign(60, 70,
                                                                                    ASTNode.TFPDef(60, 66,
                                                                                        ASTNode.Name(60, 62, Token.Name(60, 61, "g", [| |])),
                                                                                        Token.Colon(62, 63, [| |]),
                                                                                        ASTNode.Name(64, 66, Token.Name(64, 65, "h", [| |]))),
                                                                                    Token.Assign(66, 67, [| |]),
                                                                                    ASTNode.Name(68, 70, Token.Name(68, 69, "i", [| |])) );
                                                            ASTNode.TypedMul(72, 79, 
                                                                                    Token.Mul(72, 73, [| |]),
                                                                                    ASTNode.TFPDef(73, 79,
                                                                                        ASTNode.Name(73, 75, Token.Name(73, 74, "a", [| |])),
                                                                                        Token.Colon(75, 76, [| |]),
                                                                                        ASTNode.Name(77, 79, Token.Name(77, 78, "b", [| |]))
                                                            ));
                                                            ASTNode.TFPDef(81, 87,  
                                                                                ASTNode.Name(81, 83, Token.Name(81, 82, "a", [| |])),
                                                                                Token.Colon(83, 84, [| |]),
                                                                                ASTNode.Name(85, 87, Token.Name(85, 86, "b", [| |])) );
                                                            ASTNode.TypedPower(89, 93, Token.Power(89, 91, [| |]), ASTNode.Name(91, 93, Token.Name(91, 92, "c", [| |])))
                                                        |], 
                                                        [| 
                                                            Token.Comma(14, 15, [| |]);
                                                            Token.Comma(29, 30, [| |]);
                                                            Token.Comma(43, 44, [| |]);
                                                            Token.Comma(47, 48, [| |]);
                                                            Token.Comma(70, 71, [| |]);
                                                            Token.Comma(79, 80, [| |]);
                                                            Token.Comma(87, 88, [| |])
                                                        |], 
                                                        [| 
                                                            Token.TypeComment(16, 26, "#type: int");
                                                            Token.TypeComment(31, 41, "#type: int");
                                                            Token.TypeComment(49, 59, "#type: int")
                                                        |]), parser.ParseTypedArgsList())


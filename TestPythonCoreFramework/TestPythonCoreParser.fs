
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

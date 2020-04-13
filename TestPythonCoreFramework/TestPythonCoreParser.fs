module TestsPythonCoreParser

open Xunit
open PythonCoreFramework



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

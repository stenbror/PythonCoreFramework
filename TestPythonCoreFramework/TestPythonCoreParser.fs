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

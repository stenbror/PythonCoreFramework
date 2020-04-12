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
let ``Name literal number`` () =
    let lex = new MockTokenizer( [ ( Token.Number(0, 5, "1234", [| |]), 0 ); ( Token.EOF([| |]), 5 ); ] )
    lex.Next()
    let parser = new Parser(lex)
    Assert.Equal( ASTNode.Number(0, 5, Token.Number(0, 5, "1234", [| |])), parser.ParseAtom())

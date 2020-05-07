
// PythonCoreFramework.Net - Mock Tokenizer for pure parse rule UnitTests.
// Written by Richard Magnor Stenbro. 

namespace PythonCoreFramework.UnitTests

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

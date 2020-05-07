
// PythonCoreFramework.Net - UnitTests for correct handling of SyntaxError during parsing of Python Grammar.
// Written by Richard Magnor Stenbro. 

namespace PythonCoreFramework.UnitTests

open Xunit
open PythonCoreFramework


module TestPythonCoreParserSyntaxErrorHandling =

    [<Fact>]
    let ``Template for SyntaxError UnitTest`` () =
        let lex = new MockTokenizer( [ ( Token.EOF([| |]), 0 ); ] )
        lex.Next()
        let parser = new Parser(lex)
        Assert.Equal( true, true )


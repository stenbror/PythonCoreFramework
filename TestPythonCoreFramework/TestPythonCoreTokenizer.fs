
namespace PythonCoreFramework.UnitTests

open Xunit
open PythonCoreFramework

module TestPythonCoreTokenizer =

    [<Fact>]
    let ``Reserved keyword 'False'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("False") with | Option.Some( x ) -> true | Option.None -> false)

    [<Fact>]
    let ``Reserved keyword 'None'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("None") with | Option.Some( x ) -> true | Option.None -> false)

    [<Fact>]
    let ``Reserved keyword 'True'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("True") with | Option.Some( x ) -> true | Option.None -> false)
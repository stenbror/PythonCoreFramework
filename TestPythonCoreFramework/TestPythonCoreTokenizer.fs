
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

    [<Fact>]
    let ``Reserved keyword 'and'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("and") with | Option.Some( x ) -> true | Option.None -> false)

    [<Fact>]
    let ``Reserved keyword 'as'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("as") with | Option.Some( x ) -> true | Option.None -> false)

    [<Fact>]
    let ``Reserved keyword 'assert'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("assert") with | Option.Some( x ) -> true | Option.None -> false)

    [<Fact>]
    let ``Reserved keyword 'async'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("async") with | Option.Some( x ) -> true | Option.None -> false)

    [<Fact>]
    let ``Reserved keyword 'await'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("await") with | Option.Some( x ) -> true | Option.None -> false)

    [<Fact>]
    let ``Reserved keyword 'break'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("break") with | Option.Some( x ) -> true | Option.None -> false)

    [<Fact>]
    let ``Reserved keyword 'class'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("class") with | Option.Some( x ) -> true | Option.None -> false)

    [<Fact>]
    let ``Reserved keyword 'continue'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("continue") with | Option.Some( x ) -> true | Option.None -> false)

    [<Fact>]
    let ``Reserved keyword 'def'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("def") with | Option.Some( x ) -> true | Option.None -> false)

    [<Fact>]
    let ``Reserved keyword 'del'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("del") with | Option.Some( x ) -> true | Option.None -> false)

    [<Fact>]
    let ``Reserved keyword 'elif'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("elif") with | Option.Some( x ) -> true | Option.None -> false)

    [<Fact>]
    let ``Reserved keyword 'else'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("else") with | Option.Some( x ) -> true | Option.None -> false)

    [<Fact>]
    let ``Reserved keyword 'except'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("except") with | Option.Some( x ) -> true | Option.None -> false)

    [<Fact>]
    let ``Reserved keyword 'finally'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("finally") with | Option.Some( x ) -> true | Option.None -> false)

    [<Fact>]
    let ``Reserved keyword 'for'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("for") with | Option.Some( x ) -> true | Option.None -> false)

    [<Fact>]
    let ``Reserved keyword 'from'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("from") with | Option.Some( x ) -> true | Option.None -> false)

    [<Fact>]
    let ``Reserved keyword 'global'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("global") with | Option.Some( x ) -> true | Option.None -> false)

    [<Fact>]
    let ``Reserved keyword 'if'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("if") with | Option.Some( x ) -> true | Option.None -> false)

    [<Fact>]
    let ``Reserved keyword 'import'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("import") with | Option.Some( x ) -> true | Option.None -> false)

    [<Fact>]
    let ``Reserved keyword 'in'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("in") with | Option.Some( x ) -> true | Option.None -> false)

    [<Fact>]
    let ``Reserved keyword 'is'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("is") with | Option.Some( x ) -> true | Option.None -> false)

    [<Fact>]
    let ``Reserved keyword 'lambda'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("lambda") with | Option.Some( x ) -> true | Option.None -> false)

    [<Fact>]
    let ``Reserved keyword 'nonlocal'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("nonlocal") with | Option.Some( x ) -> true | Option.None -> false)

    [<Fact>]
    let ``Reserved keyword 'not'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("not") with | Option.Some( x ) -> true | Option.None -> false)

    [<Fact>]
    let ``Reserved keyword 'or'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("or") with | Option.Some( x ) -> true | Option.None -> false)

    [<Fact>]
    let ``Reserved keyword 'pass'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("pass") with | Option.Some( x ) -> true | Option.None -> false)

    [<Fact>]
    let ``Reserved keyword 'raise'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("raise") with | Option.Some( x ) -> true | Option.None -> false)

    [<Fact>]
    let ``Reserved keyword 'return'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("return") with | Option.Some( x ) -> true | Option.None -> false)

    [<Fact>]
    let ``Reserved keyword 'try'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("try") with | Option.Some( x ) -> true | Option.None -> false)

    [<Fact>]
    let ``Reserved keyword 'while'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("while") with | Option.Some( x ) -> true | Option.None -> false)

    [<Fact>]
    let ``Reserved keyword 'with'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("with") with | Option.Some( x ) -> true | Option.None -> false)

    [<Fact>]
    let ``Reserved keyword 'yield'`` () =
        let lex = new Tokenizer()
        Assert.True(match lex.IsReservedKeywordOrLiteralName("yield") with | Option.Some( x ) -> true | Option.None -> false)

    [<Fact>]
    let ``Operator or delimiter '+'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('+', ' ', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Plus( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '-'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('-', ' ', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Minus( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )






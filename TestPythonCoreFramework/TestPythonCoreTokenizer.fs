
namespace PythonCoreFramework.UnitTests

open Xunit
open PythonCoreFramework

module TestPythonCoreTokenizer =

    [<Fact>]
    let ``Reserved keyword 'False'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("False") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.False( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Reserved keyword 'None'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("None") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.None( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Reserved keyword 'True'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("True") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.True( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Reserved keyword 'and'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("and") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.And( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Reserved keyword 'as'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("as") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.As( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Reserved keyword 'assert'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("assert") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Assert( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Reserved keyword 'async'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("async") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Async( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Reserved keyword 'await'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("await") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Await( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Reserved keyword 'break'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("break") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Break( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Reserved keyword 'class'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("class") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Class( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Reserved keyword 'continue'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("continue") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Continue( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Reserved keyword 'def'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("def") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Def( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Reserved keyword 'del'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("del") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Del( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Reserved keyword 'elif'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("elif") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Elif( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Reserved keyword 'else'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("else") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Else( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Reserved keyword 'except'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("except") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Except( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Reserved keyword 'finally'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("finally") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Finally( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Reserved keyword 'for'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("for") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.For( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Reserved keyword 'from'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("from") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.From( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Reserved keyword 'global'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("global") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Global( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Reserved keyword 'if'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("if") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.If( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Reserved keyword 'import'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("import") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Import( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Reserved keyword 'in'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("in") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.In( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Reserved keyword 'is'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("is") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Is( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Reserved keyword 'lambda'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("lambda") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Lambda( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Reserved keyword 'nonlocal'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("nonlocal") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Nonlocal( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Reserved keyword 'not'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("not") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Not( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Reserved keyword 'or'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("or") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Or( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Reserved keyword 'pass'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("pass") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Pass( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Reserved keyword 'raise'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("raise") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Raise( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Reserved keyword 'return'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("return") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Return( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Reserved keyword 'try'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("try") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Try( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Reserved keyword 'while'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("while") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.While( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Reserved keyword 'with'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("with") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.With( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Reserved keyword 'yield'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsReservedKeywordOrLiteralName("yield") with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Yield( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

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

    [<Fact>]
    let ``Operator or delimiter '*'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('*', ' ', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Mul( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '**'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('*', '*', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Power( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '/'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('/', ' ', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Div( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '//'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('/', '/', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.FloorDiv( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '%'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('%', ' ', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Modulo( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter Matrice`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('@', ' ', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Matrice( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '<<'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('<', '<', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.ShiftLeft( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '>>'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('>', '>', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.ShiftRight( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '&'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('&', ' ', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.BitAnd( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '|'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('|', ' ', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.BitOr( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '^'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('^', ' ', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.BitXor( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '~'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('~', ' ', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.BitInvert( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '<'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('<', ' ', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Less( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '>'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('>', ' ', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Greater( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '<='`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('<', '=', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.LessEqual( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '>='`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('>', '=', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.GreaterEqual( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '=='`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('=', '=', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Equal( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '<>'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('<', '>', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.NotEqual( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '!='`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('!', '=', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.NotEqual( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '('`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('(', ' ', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.LeftParen( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter ')'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter(')', ' ', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.RightParen( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '['`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('[', ' ', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.LeftBracket( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter ']'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter(']', ' ', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.RightBracket( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '{'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('{', ' ', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.LeftCurly( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '}'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('}', ' ', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.RightCurly( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter ','`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter(',', ' ', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Comma( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter ':'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter(':', ' ', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Colon( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter ':='`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter(':', '=', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.ColonAssign( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter dot`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('.', ' ', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Dot( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '...'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('.', '.', '.') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Elipsis( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter ';'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter(';', ' ', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.SemiColon( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '='`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('=', ' ', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Assign( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '->'`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('-', '>', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.Ptr( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '+='`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('+', '=', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.PlusAssign( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '-='`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('-', '=', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.MinusAssign( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '*='`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('*', '=', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.MulAssign( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '/='`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('/', '=', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.DivAssign( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '//='`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('/', '/', '=') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.FloorDivAssign( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '%='`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('%', '=', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.ModuloAssign( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter Matrice Assign`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('@', '=', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.MatriceAssign( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '&='`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('&', '=', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.BitAndAssign( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '|='`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('|', '=', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.BitOrAssign( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '^='`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('^', '=', ' ') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.BitXorAssign( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '<<='`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('<', '<', '=') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.ShiftLeftAssign( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '>>='`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('>', '>', '=') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.ShiftRightAssign( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

    [<Fact>]
    let ``Operator or delimiter '**='`` () =
        let lex = new Tokenizer()
        Assert.True(
            match lex.IsOperatorOrDelimiter('*', '*', '=') with
            |   Option.Some(x) ->
                    match x(0, 0, [||]) with
                    |   Token.PowerAssign( _ , _ , [| |]) ->
                            true
                    |   _ ->
                            false
            |   Option.None ->
                    false 
            )

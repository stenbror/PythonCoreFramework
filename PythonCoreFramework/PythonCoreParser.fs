
// PythonCore - Tokenizer, Parser, AST tree, Exceptions for 3.9 grammar of Python
// Written by Richard Magnor Stenbro. 

namespace PythonCoreFramework

open System

// Whitespace in sourcecode ///////////////////////////////////////////////////////////////////////

type Trivia =
    |   Whitespace of char array
    |   Newline of char array
    |   Comment of char array
    |   LineContinuation of char

// Tokenizer produces these symbols for parser ////////////////////////////////////////////////////

type Token =
    |   False of int * int * Trivia array
    |   None of int * int * Trivia array
    |   True of int * int * Trivia array
    |   And of int * int * Trivia array
    |   As of int * int * Trivia array
    |   Assert of int * int * Trivia array
    |   Async of int * int * Trivia array
    |   Await of int * int * Trivia array
    |   Break of int * int * Trivia array
    |   Class of int * int * Trivia array
    |   Continue of int * int * Trivia array
    |   Def of int * int * Trivia array
    |   Del of int * int * Trivia array
    |   Elif of int * int * Trivia array
    |   Else of int * int * Trivia array
    |   Except of int * int * Trivia array
    |   Finally of int * int * Trivia array
    |   For of int * int * Trivia array
    |   From of int * int * Trivia array
    |   Global of int * int * Trivia array
    |   If of int * int * Trivia array
    |   Import of int * int * Trivia array
    |   In of int * int * Trivia array
    |   Is of int * int * Trivia array
    |   Lambda of int * int * Trivia array
    |   Nonlocal of int * int * Trivia array
    |   Not of int * int * Trivia array
    |   Or of int * int * Trivia array
    |   Pass of int * int * Trivia array
    |   Raise of int * int * Trivia array
    |   Return of int * int * Trivia array
    |   Try of int * int * Trivia array
    |   While of int * int * Trivia array
    |   With of int * int * Trivia array
    |   Yield of int * int * Trivia array
    |   Plus of int * int * Trivia array
    |   Minus of int * int * Trivia array
    |   Mul of int * int * Trivia array
    |   Power of int * int * Trivia array
    |   Div of int * int * Trivia array
    |   FloorDiv of int * int * Trivia array
    |   Modulo of int * int * Trivia array
    |   Matrice of int * int * Trivia array
    |   ShiftLeft of int * int * Trivia array
    |   ShiftRight of int * int * Trivia array
    |   BitAnd of int * int * Trivia array
    |   BitOr of int * int * Trivia array
    |   BitXor of int * int * Trivia array
    |   BitInvert of int * int * Trivia array
    |   Less of int * int * Trivia array
    |   Greater of int * int * Trivia array
    |   LessEqual of int * int * Trivia array
    |   GreaterEqual of int * int * Trivia array
    |   Equal of int * int * Trivia array
    |   NotEqual of int * int * Trivia array
    |   LeftParen of int * int * Trivia array
    |   LeftBracket of int * int * Trivia array
    |   LeftCurly of int * int * Trivia array
    |   RightParen of int * int * Trivia array
    |   RightBracket of int * int * Trivia array
    |   RightCurly of int * int * Trivia array
    |   Comma of int * int * Trivia array
    |   Colon of int * int * Trivia array
    |   ColonAssign of int * int * Trivia array
    |   Dot of int * int * Trivia array
    |   Elipsis of int * int * Trivia array
    |   SemiColon of int * int * Trivia array
    |   Assign of int * int * Trivia array
    |   Ptr of int * int * Trivia array
    |   PlusAssign of int * int * Trivia array
    |   MinusAssign of int * int * Trivia array
    |   MulAssign of int * int * Trivia array
    |   PowerAssign of int * int * Trivia array
    |   DivAssign of int * int * Trivia array
    |   FloorDivAssign of int * int * Trivia array
    |   ModuloAssign of int * int * Trivia array
    |   BitAndAssign of int * int * Trivia array
    |   BitOrAssign of int * int * Trivia array
    |   BitXorAssign of int * int * Trivia array
    |   MatriceAssign of int * int * Trivia array
    |   ShiftLeftAssign of int * int * Trivia array
    |   ShiftRightAssign of int * int * Trivia array
    |   Name of int * int * string * Trivia array
    |   Number of int * int * string * Trivia array
    |   String of int * int * string array * Trivia array
    |   TypeComment of int * int * string
    |   Indent of Trivia array
    |   Dedent of Trivia array
    |   Newline of int * int * Trivia array
    |   EOF of Trivia array
    |   Empty

// Resulting nodes by parsing of sourcecode ///////////////////////////////////////////////////////

type ASTNode =
    |   SingleInput of int * int * ASTNode * Token
    |   FileInput of int * int * ASTNode array * Token array * Token
    |   EvalInput of int * int * ASTNode * Token array * Token
    |   Decorator of int * int *Token * ASTNode * Token * ASTNode * Token * Token
    |   Decorators of int * int * ASTNode array
    |   Decorated of int * int * ASTNode * ASTNode
    |   Class of int * int * Token * ASTNode * Token * ASTNode * Token * Token * ASTNode
    |   AsyncFuncDef of int * int * Token * ASTNode
    |   FuncDef of int * int * Token * ASTNode * ASTNode * Token * ASTNode * Token * Token * ASTNode
    |   Parameters of int * int * Token * ASTNode * Token
    |   TypedArgsList of int * int * ASTNode array * Token array * Token array
    |   TypedAssign of int * int * ASTNode * ASTNode * Token * ASTNode
    |   TypedMul of int * int * Token * ASTNode
    |   TypedPower of int * int * Token * ASTNode
    |   TFPDef of int * int * ASTNode * Token * ASTNode
    |   VarArgsList of int * int * ASTNode array * Token array * Token array
    |   VarAssign of int * int * ASTNode * ASTNode * Token * ASTNode
    |   VarMul of int * int * Token * ASTNode
    |   VarPower of int * int * Token * ASTNode
    |   SimpleStmtList of int * int * ASTNode array * Token array * Token
    |   PlusAssign of int * int * ASTNode * Token * ASTNode
    |   MinusAssign of int * int * ASTNode * Token * ASTNode
    |   MulAssign of int * int * ASTNode * Token * ASTNode
    |   MatriceAssign of int * int * ASTNode * Token * ASTNode
    |   DivAssign of int * int * ASTNode * Token * ASTNode
    |   ModuloAssign of int * int * ASTNode * Token * ASTNode
    |   AndAssign of int * int * ASTNode * Token * ASTNode
    |   OrAssign of int * int * ASTNode * Token * ASTNode
    |   XorAssign of int * int * ASTNode * Token * ASTNode
    |   ShiftLeftAssign of int * int * ASTNode * Token * ASTNode
    |   ShiftRightAssign of int * int * ASTNode * Token * ASTNode
    |   PowerAssign of int * int * ASTNode * Token * ASTNode
    |   FloorDivAssign of int * int * ASTNode * Token * ASTNode
    |   AnnAssign of int * int * ASTNode * Token * ASTNode * Token * ASTNode
    |   Assign of int * int * ASTNode * Token * Token * ASTNode
    |   Del of int * int * Token * ASTNode
    |   Pass of int * int * Token
    |   Break of int * int * Token
    |   Continue of int * int * Token
    |   Return of int * int * Token * ASTNode
    |   Raise of int * int * Token * ASTNode * Token * ASTNode
    |   Import of int * int * Token * ASTNode
    |   ImportFrom of int * int * Token * Token array * ASTNode * Token * Token * ASTNode * Token
    |   ImportAsName of int * int * ASTNode * Token * ASTNode
    |   DottedAsName of int * int * ASTNode * Token * ASTNode
    |   ImportAsNames of int * int * ASTNode array * Token array
    |   DottedAsNames of int * int *ASTNode array * Token array
    |   DottedName of int * int * ASTNode array * Token array
    |   Global of int * int * Token * ASTNode array * Token array
    |   Nonlocal of int * int * Token * ASTNode array * Token array
    |   Assert of int * int * Token * ASTNode * Token * ASTNode
    |   AsyncStmt of int * int * Token * ASTNode
    |   If of int * int * Token * ASTNode * Token * ASTNode * ASTNode array * ASTNode
    |   Elif of int * int * Token * ASTNode * Token * ASTNode
    |   Else of int * int * Token * Token * ASTNode
    |   While of int * int * Token * ASTNode * Token * ASTNode * ASTNode
    |   For of int * int * Token * ASTNode * Token * ASTNode * Token * Token * ASTNode * ASTNode
    |   Try of int * int * Token * Token * ASTNode * ASTNode array * ASTNode * ASTNode
    |   Finally of int * int * Token * Token * ASTNode
    |   With of int * int * Token * ASTNode array * Token array * Token * Token * ASTNode
    |   WithItem of int * int * ASTNode * Token * ASTNode
    |   Except of int * int * Token * ASTNode * Token * ASTNode * Token * ASTNode
    |   Suite of int * int * Token * Token * ASTNode array * Token
    |   NamedExpr of int * int * ASTNode * Token * ASTNode
    |   Test of int * int * ASTNode * Token * ASTNode * Token * ASTNode
    |   Lambda of int * int * Token * ASTNode * Token * ASTNode
    |   OrTest of int * int * ASTNode * Token * ASTNode
    |   AndTest of int * int * ASTNode * Token * ASTNode
    |   NotTest of int * int * Token * ASTNode
    |   Less of int * int * ASTNode * Token * ASTNode
    |   LessEqual of int * int * ASTNode * Token * ASTNode
    |   Equal of int * int * ASTNode * Token * ASTNode
    |   GreaterEqual of int * int * ASTNode * Token * ASTNode
    |   Greater of int * int * ASTNode * Token * ASTNode
    |   NotEqual of int * int * ASTNode * Token * ASTNode
    |   Is of int * int * ASTNode * Token * ASTNode
    |   IsNot of int * int * ASTNode * Token * Token * ASTNode
    |   NotIn of int * int * ASTNode * Token * Token * ASTNode
    |   In of int * int * ASTNode * Token * ASTNode
    |   StarExpr of int * int * Token * ASTNode
    |   OrExpr of int * int * ASTNode * Token * ASTNode
    |   XorExpr of int * int * ASTNode * Token * ASTNode
    |   AndExpr of int * int * ASTNode * Token * ASTNode
    |   ShiftLeft of int * int * ASTNode * Token * ASTNode
    |   ShiftRight of int * int * ASTNode * Token * ASTNode
    |   Plus of int * int * ASTNode * Token * ASTNode
    |   Minus of int * int * ASTNode * Token * ASTNode
    |   Mul of int * int * ASTNode * Token * ASTNode
    |   Matrice of int * int * ASTNode * Token * ASTNode
    |   Div of int * int * ASTNode * Token * ASTNode
    |   Modulo of int * int * ASTNode * Token * ASTNode
    |   FloorDiv of int * int * ASTNode * Token * ASTNode
    |   UnaryPlus of int * int * Token * ASTNode
    |   UnaryMinus of int * int * Token * ASTNode
    |   UnaryInvert of int * int * Token * ASTNode
    |   Power of int * int * ASTNode * Token * ASTNode
    |   AtomExpr of int * int * Token * ASTNode * ASTNode array
    |   Name of int * int * Token
    |   Number of int * int * Token
    |   String of int * int * Token array
    |   Elipsis of int * int * Token
    |   None of int * int * Token
    |   True of int * int * Token
    |   False of int * int * Token
    |   Tuple of int * int * Token * ASTNode * Token
    |   List of int * int * Token * ASTNode * Token
    |   Dictionary of int * int * Token * ASTNode array * Token array * Token
    |   Set of int * int * Token * ASTNode array * Token array * Token
    |   Call of int * int * Token * ASTNode * Token
    |   Index of int * int * Token * ASTNode * Token
    |   DotName of int * int * Token * ASTNode
    |   SubscriptList of int * int * ASTNode array * Token array
    |   Subscript of int * int * ASTNode * Token * ASTNode * Token * ASTNode
    |   ExprList of int * int * ASTNode array * Token array
    |   TestList of int * int * ASTNode array * Token array
    |   DictionaryEntry of int * int * ASTNode * Token * ASTNode
    |   SetEntry of int * int * ASTNode
    |   ArgumentList of int * int * ASTNode array * Token array
    |   Argument of int * int * ASTNode * Token * ASTNode
    |   SyncCompFor of int * int * Token * ASTNode * Token * ASTNode * ASTNode
    |   CompFor of int * int * Token * ASTNode
    |   CompIf of int * int * Token * ASTNode * ASTNode
    |   YieldExpr of int * int * Token * ASTNode
    |   YieldFromExpr of int * int * Token * Token * ASTNode
    |   FuncBodySuite of int * int * Token * Token * Token * Token * ASTNode array * Token
    |   FuncTypeInput of int * int * ASTNode * Token array * Token
    |   FuncType of int * int * Token * ASTNode * Token * Token * ASTNode
    |   TypeList of int * int * ASTNode array * Token array
    |   TypeListStar of int * int * Token * ASTNode
    |   TypeListPower of int * int * Token * ASTNode
    |   Empty 

// Exceptions provided in error situations ////////////////////////////////////////////////////////

exception LexicalError of int * string

exception SyntaxError of Token * string

// Interface for communication between tokenizer and parser ///////////////////////////////////////

type ITokenizer =

    abstract member Symbol : Token with get, set

    abstract member Position : int with get, set

    abstract member Advance : unit -> unit



///////////////////////////////////////////////////////////////////////////////////////////////////
// Tokenizer for Python 3.9 ///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////

type Tokenizer() =

    let keywords =  
        [ 
            ( "and",        Token.And );
            ( "as",         Token.As );
            ( "assert",     Token.Assert );
            ( "async",      Token.Async );
            ( "await",      Token.Await );
            ( "break",      Token.Break );
            ( "class",      Token.Class );
            ( "continue",   Token.Continue );
            ( "def",        Token.Def );
            ( "del",        Token.Del );
            ( "elif",       Token.Elif );
            ( "else",       Token.Else );
            ( "except",     Token.Except );
            ( "finally",    Token.Finally );
            ( "for",        Token.For );
            ( "from",       Token.From );
            ( "global",     Token.Global );
            ( "if",         Token.If );
            ( "import",     Token.Import );
            ( "in",         Token.In );
            ( "is",         Token.Is );
            ( "lambda",     Token.Lambda );
            ( "nonlocal",   Token.Nonlocal );
            ( "not",        Token.Not );
            ( "or",         Token.Or );
            ( "pass",       Token.Pass );
            ( "raise",      Token.Raise );
            ( "return",     Token.Return );
            ( "try",        Token.Try );
            ( "while",      Token.While );
            ( "with",       Token.With );
            ( "yield",      Token.Yield );
            ( "False",      Token.False );
            ( "None",       Token.None );
            ( "True",       Token.True );
        ] |> Map.ofList

    let mutable index = 0
    let mutable parenthezis : char array = Array.zeroCreate 100
    let mutable level = 0 
    let mutable indentStack : int array = Array.zeroCreate 100
    let mutable pending : int = 0
    let mutable indentLevel : int = 0
    let mutable atBOL : bool = true

    member val sourceBuffer : char array = [| |] with get, set

    
    interface ITokenizer with

        member val Symbol = Token.Empty with get, set
    
        member val Position = 0 with get, set

        member this.Advance() =
            this.NextSymbol() // Extend with indent / dedent etc later here !

    member this.IsReservedKeywordOrLiteralName(key : string) =
        match keywords.ContainsKey(key) with
        |   true    ->  
                Some(keywords.Item(key))
        |   _   ->
                option.None

    member this.IsOperatorOrDelimiter(c1, c2, c3) =
        match c1, c2, c3 with
        |   '<', '<', '='   ->  Some(Token.ShiftLeftAssign)
        |   '<', '<', _     ->  Some(Token.ShiftLeft)
        |   '<', '>', _     ->  Some(Token.NotEqual)
        |   '<', '=', _     ->  Some(Token.LessEqual)
        |   '<', _ , _      ->  Some(Token.Less)
        |   '>', '>', '='   ->  Some(Token.ShiftRightAssign)
        |   '>', '>', _     ->  Some(Token.ShiftRight)
        |   '>', '=', _     ->  Some(Token.GreaterEqual)
        |   '>', _ , _      ->  Some(Token.Greater)
        |   '*', '*', '='   ->  Some(Token.PowerAssign)
        |   '*', '*', _     ->  Some(Token.Power)
        |   '*', '=', _     ->  Some(Token.MulAssign)
        |   '*', _ , _      ->  Some(Token.Mul)
        |   '/', '/', '='   ->  Some(Token.FloorDivAssign)
        |   '/', '/', _     ->  Some(Token.FloorDiv)
        |   '/', '=', _     ->  Some(Token.DivAssign)
        |   '/', _ , _      ->  Some(Token.Div)
        |   '.', '.', '.'   ->  Some(Token.Elipsis)
        |   '.', _ , _      ->  Some(Token.Dot)
        |   '+', '=', _     ->  Some(Token.PlusAssign)
        |   '+', _ , _      ->  Some(Token.Plus)
        |   '-', '>', _     ->  Some(Token.Ptr)
        |   '-', '=', _     ->  Some(Token.MinusAssign)
        |   '-', _ , _      ->  Some(Token.Minus)
        |   '%', '=', _     ->  Some(Token.ModuloAssign)
        |   '%', _ , _      ->  Some(Token.Modulo)
        |   '&', '=', _     ->  Some(Token.BitAndAssign)
        |   '&', _ , _      ->  Some(Token.BitOrAssign)
        |   '|', '=', _     ->  Some(Token.BitOrAssign)
        |   '|', _ , _      ->  Some(Token.BitOr)
        |   '^', '=', _     ->  Some(Token.BitXorAssign)
        |   '^', _ , _      ->  Some(Token.BitXor)
        |   '@', '=', _     ->  Some(Token.MatriceAssign)
        |   '@', _ , _      ->  Some(Token.Matrice)
        |   '=', '=', _     ->  Some(Token.Equal)
        |   '=', _ , _      ->  Some(Token.Assign)
        |   '!', '=', _     ->  Some(Token.NotEqual)
        |   '(', _ , _      ->  Some(Token.LeftParen)
        |   '[', _ , _      ->  Some(Token.LeftBracket)
        |   '{', _ , _      ->  Some(Token.LeftCurly)
        |   ')', _ , _      ->  Some(Token.RightParen)
        |   ']', _ , _      ->  Some(Token.RightBracket)
        |   '}', _ , _      ->  Some(Token.RightCurly)
        |   ':', '=', _     ->  Some(Token.ColonAssign)
        |   ':', _ , _      ->  Some(Token.Colon)
        |   ';', _ , _      ->  Some(Token.SemiColon)
        |   ',', _ , _      ->  Some(Token.Comma)
        |   '~', _ , _      ->  Some(Token.BitInvert)
        |   _ , _ , _       ->  option.None

    // Main tokenizer loop ////////////////////////////////////////////////////////////////////////

    member this.NextSymbol() =

        let readOneChar() : char =
            if index < this.sourceBuffer.Length then
                this.sourceBuffer.[index]
            else
                ' '

        let readThreeChars() : char * char * char =
            if index < this.sourceBuffer.Length - 2 then
                this.sourceBuffer.[index], this.sourceBuffer.[index + 1], this.sourceBuffer.[index + 2]
            else if index < this.sourceBuffer.Length - 1 then
                this.sourceBuffer.[index], this.sourceBuffer.[index + 1], ' '
            else if index < this.sourceBuffer.Length then
                this.sourceBuffer.[index], ' ', ' '
            else 
                ' ', ' ', ' '

        while readOneChar() = ' ' || readOneChar() = '\t' do index <- index + 1 // Whitespace

        (this :> ITokenizer).Position <- index // Set start of current symbol to collect

        (this :> ITokenizer).Symbol <-  match this.IsOperatorOrDelimiter(readThreeChars()) with
                                        |   Some(x) -> x( (this :> ITokenizer).Position, index, [||] )
                                        |   Option.None -> Token.Empty

        if (this :> ITokenizer).Symbol <> Token.Empty then
            match (this :> ITokenizer).Symbol with
            |   Token.LeftParen _ ->
                    if level >= 100 then raise ( LexicalError(index, "Maximum 100 level of nested parethezis allowd!" ) )
                    parenthezis.[level] <- '('
                    level <- level + 1
            |   Token.LeftBracket _ ->
                    if level >= 100 then raise ( LexicalError(index, "Maximum 100 level of nested parethezis allowd!" ) )
                    parenthezis.[level] <- '['
                    level <- level + 1
            |   Token.LeftCurly _ ->
                    if level >= 100 then raise ( LexicalError(index, "Maximum 100 level of nested parethezis allowd!" ) )
                    parenthezis.[level] <- '{'
                    level <- level + 1
            |   Token.RightParen _  ->
                    if level > 0 && parenthezis.[level - 1] <> '(' then raise ( LexicalError(index, "Closing parenthezis ')' is not matched by a starting '('") )
                    level <- level - 1
            |   Token.RightBracket _ ->
                    if level > 0 && parenthezis.[level - 1] <> '[' then raise ( LexicalError(index, "Closing parenthezis ']' is not matched by a starting '['") )
                    level <- level - 1
            |   Token.RightCurly _ ->
                    if level > 0 && parenthezis.[level - 1] <> '{' then raise ( LexicalError(index, "Closing parenthezis '}' is not matched by a starting '{'") )
                    level <- level - 1
            |   _ ->
                    ()
        else if Char.IsLetter(readOneChar()) || readOneChar() = '_' then
            while Char.IsLetterOrDigit(readOneChar()) || readOneChar() = '_' do index <- index
            let check = new string( this.sourceBuffer.[ (this :> ITokenizer).Position .. index - 1] )
            
            if ( check.Length = 1 || check.Length = 2 ) && ( readOneChar() = '\'' || readOneChar() = '"' ) then
                (* Check for valid prefix before string *)
                if check.Length = 1 then
                    match check with
                    |   "u"
                    |   "U"
                    |   "f"
                    |   "F"
                    |   "b"
                    |   "B"
                    |   "r"
                    |   "R" ->
                        (this :> ITokenizer).Symbol <- this.HandleStrings()
                    |   _   ->
                        raise ( LexicalError(index, "Illegal prefix for string!") )
                else
                    match check with
                    |   "fr"
                    |   "Fr"
                    |   "fR"
                    |   "FR"
                    |   "rb"
                    |   "Rb"
                    |   "rB"
                    |   "RB"    ->  
                        (this :> ITokenizer).Symbol <- this.HandleStrings()
                    |   _   ->
                        raise ( LexicalError(index, "Illegal prefix for string!") ) 
            else
                match this.IsReservedKeywordOrLiteralName(check) with
                |   Some(x) ->
                        (this :> ITokenizer).Symbol <- x( (this :> ITokenizer).Position, index, [| |] )
                |   Option.None ->
                        (this :> ITokenizer).Symbol <- Token.Name( (this :> ITokenizer).Position, index, check, [| |] )

        else if Char.IsDigit(readOneChar()) || readOneChar() = '.' then

            (this :> ITokenizer).Symbol <- this.HandleNumbers()

        else if readOneChar() = '\'' || readOneChar() = '"' then

            (this :> ITokenizer).Symbol <- this.HandleStrings()
        
        else if readOneChar() = '#' then
            ()
        //else if this.sourceBuffer.[index] = 0x0000 then
        //    ()
        else
            raise ( LexicalError(index, "") )

    // All numbers is handled here ////////////////////////////////////////////////////////////////

    member this.HandleNumbers() =

        let isHexDigit(ch : char) : bool =
            match ch with
            | '0'| '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
            | 'a' | 'b' | 'c' | 'd' | 'e' | 'f'
            | 'A' | 'B' | 'C' | 'D' | 'E' | 'F'  ->  true
            |   _   ->  false

        let isOctetDigit(ch : char) : bool =
            match ch with
            | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' -> true
            |   _   ->  false

        let isBinaryDigit(ch : char ) : bool =
            match ch with
            | '0' | '1' ->  true
            |   _ ->    false

        let getChar() = if index < this.sourceBuffer.Length then this.sourceBuffer.[index] else ' '

        (this :> ITokenizer).Position <- index // Mark start of Number

        if getChar() = '0' then
            index <- index + 1
            if getChar() = 'x' || getChar() = 'X' then
                index <- index + 1
                let mutable isLock = true
                while isLock do
                    if getChar() = '_' then index <- index + 1
                    if isHexDigit(getChar()) = false then raise ( LexicalError(index, "") )
                    while isHexDigit(getChar()) do index <- index + 1
                    if getChar() <> '_' then isLock <- false
            else if getChar() = 'o' || getChar() = 'O' then
                index <- index + 1
                let mutable isLock = true
                while isLock do
                    if getChar() = '_' then index <- index + 1
                    if getChar() = '8' || getChar() = '9' then raise ( LexicalError(index, "Illegal octet digit '8' or '9' not allowed!") )
                    else if isOctetDigit(getChar()) = false then raise ( LexicalError(index, "Expecting octet digit(s) in octet number!") )
                    while isOctetDigit(getChar()) do index <- index + 1
                    if getChar() <> '_' then isLock <- false
            else if getChar() = 'b' || getChar() = 'B' then     
                index <- index + 1
                let mutable isLock = true
                while isLock do
                    if getChar() = '_' then index <- index + 1
                    if getChar() < '0' || getChar() > '1' then raise ( LexicalError(index, "Illegal binary digit, only '0' and '1' is allowed!") )
                    else if isBinaryDigit(getChar()) = false then raise ( LexicalError(index, "Expecting binary digit(s) in binary number!") )
                    while isBinaryDigit(getChar()) do index <- index + 1
                    if getChar() <> '_' then isLock <- false
                if Char.IsDigit(getChar()) then raise ( LexicalError(index, "Expecting binary digit(s) in binary number!") )
            else
                let mutable isLocked = true
                if Char.IsDigit(getChar()) <> true && getChar() <> '_' then isLocked <- false
                while isLocked do
                    if getChar() = '_' then index <- index + 1
                    if Char.IsDigit(getChar()) <> true then raise ( LexicalError(index, "Expecting digit in numbers after '_'") )
                    if getChar() <> '0' then isLocked <- false
                    else 
                        index <- index + 1
                        if Char.IsDigit(getChar()) <> true then isLocked <- false
                if Char.IsDigit(getChar()) = true then raise ( LexicalError(index, "illegal old octet style, not supported anymore!") )

                if getChar() = '.' then 
                    index <- index + 1
                    let mutable isLock = true
                    if Char.IsDigit(getChar()) <> true then isLock <- false
                    while isLock do
                        if Char.IsDigit(getChar()) <> true then raise ( LexicalError(index, "Expecting digit in number after '_'") )
                        else index <- index + 1
                        if getChar() = '_' then index <- index + 1
                        if Char.IsDigit(getChar()) <> true then isLock <- false
                    if getChar() = '_' then raise ( LexicalError(index, "Illegal '_' in number!") )
                        
                if getChar() = 'e' || getChar() = 'E' then 
                    index <- index + 1
                    if getChar() = '+' || getChar() = '-' then
                        index <- index + 1
                        if Char.IsDigit(getChar()) <> true then raise ( LexicalError(index, "Expecting digit in number after '+' or '-' in exponent!") )
                    else if Char.IsDigit(getChar()) <> true then
                        raise ( LexicalError(index, "Expecting digit after 'e' or 'E' in exponent!") )
                    let mutable isLock = true
                    while isLock do
                        if Char.IsDigit(getChar()) <> true then raise ( LexicalError(index, "Expecting digit in number after '_'") )
                        else index <- index + 1
                        if getChar() = '_' then index <- index + 1
                        if Char.IsDigit(getChar()) <> true then isLock <- false
                    if getChar() = '_' then raise ( LexicalError(index, "Illegal '_' in number!") )         

                if getChar() = 'j' || getChar() = 'J' then
                    index <- index + 1
        else
            let mutable isLocked = true
            if Char.IsDigit(getChar()) <> true then isLocked <- false
            while isLocked do
                if Char.IsDigit(getChar()) <> true then raise ( LexicalError(index, "Expecting digit in number after '_'") )
                else index <- index + 1
                if getChar() = '_' then index <- index + 1
                if Char.IsDigit(getChar()) <> true then isLocked <- false
            if getChar() = '_' then raise ( LexicalError(index, "Illegal '_' in number!") )   

            if getChar() = '.' then 
                index <- index + 1
                let mutable isLock = true
                if Char.IsDigit(getChar()) <> true then isLock <- false
                while isLock do
                    if Char.IsDigit(getChar()) <> true then raise ( LexicalError(index, "Expecting digit in number after '_'") )
                    else index <- index + 1
                    if getChar() = '_' then index <- index + 1
                    if Char.IsDigit(getChar()) <> true then isLock <- false
                if getChar() = '_' then raise ( LexicalError(index, "Illegal '_' in number!") )
                    
            if getChar() = 'e' || getChar() = 'E' then 
                index <- index + 1
                if getChar() = '+' || getChar() = '-' then
                    index <- index + 1
                    if Char.IsDigit(getChar()) <> true then raise ( LexicalError(index, "Expecting digit in number after '+' or '-' in exponent!") )
                else if Char.IsDigit(getChar()) <> true then
                    raise ( LexicalError(index, "Expecting digit after 'e' or 'E' in exponent!") )
                let mutable isLock = true
                while isLock do
                    if Char.IsDigit(getChar()) <> true then raise ( LexicalError(index, "Expecting digit in number after '_'") )
                    else index <- index + 1
                    if getChar() = '_' then index <- index + 1
                    if Char.IsDigit(getChar()) <> true then isLock <- false
                if getChar() = '_' then raise ( LexicalError(index, "Illegal '_' in number!") )         

            if getChar() = 'j' || getChar() = 'J' then
                index <- index + 1

        Token.Number( (this :> ITokenizer).Position, index, new string ( this.sourceBuffer.[ (this :> ITokenizer).Position .. index - 1 ] ), [| |] )

    // All strings are handled here ///////////////////////////////////////////////////////////////

    member this.HandleStrings() =
        Token.Empty
            


///////////////////////////////////////////////////////////////////////////////////////////////////
// Python parser for 3.9 grammar //////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////

type Parser(lexer : ITokenizer) =
    
    member val Lexer = lexer with get, set
    member val FlowLevel = 0 with get, set
    member val FuncFlowLevel = 0 with get, set


    // Start rules in Python 3.9 grammar //////////////////////////////////////////////////////////

    member this.ParseSingleInput() =
        this.FuncFlowLevel <- 0
        this.FlowLevel <- 0
        let startPos = 0
        let op, node =  match this.Lexer.Symbol with
                        |   Token.Newline _ ->
                                let tmpOp = this.Lexer.Symbol
                                this.Lexer.Advance()
                                tmpOp, ASTNode.Empty
                        |   Token.Matrice _
                        |   Token.Async _
                        |   Token.Def _
                        |   Token.Class _
                        |   Token.If _
                        |   Token.While _
                        |   Token.For _
                        |   Token.Try _
                        |   Token.With _ ->
                                let tmpNode = this.ParseStmt()
                                let tmpOp2 =    match this.Lexer.Symbol with
                                                |   Token.Newline _ ->
                                                        let tmpOp3 = this.Lexer.Symbol
                                                        this.Lexer.Advance()
                                                        tmpOp3
                                                |   _ ->
                                                        raise ( SyntaxError(this.Lexer.Symbol, "Expecting newline after single line input!") )
                                tmpOp2, tmpNode
                        |   _ ->
                                let tmpNode = this.ParseSimpleStmt()
                                Token.Empty, tmpNode
        ASTNode.SingleInput(startPos, this.Lexer.Position, node, op)

    member this.ParseFileInput() =
        this.FuncFlowLevel <- 0
        this.FlowLevel <- 0
        let startPos = 0
        let mutable ops : Token list = []
        let mutable nodes : ASTNode list = []
        while   match this.Lexer.Symbol with
                |   Token.Newline _ ->
                        ops <- this.Lexer.Symbol :: ops
                        this.Lexer.Advance()
                        true
                |   Token.EOF _ ->
                        false
                |   _ ->
                        nodes <- this.ParseStmt() :: nodes
                        true
            do ()
        let op =    match this.Lexer.Symbol with
                    |   Token.EOF _ ->
                            this.Lexer.Symbol
                    |   _ ->
                            raise ( SyntaxError(this.Lexer.Symbol, "Expecting end of file!") )
        ASTNode.FileInput(startPos, this.Lexer.Position, List.toArray(List.rev nodes), List.toArray(List.rev ops), op)

    member this.ParseEvalInput() =
        let startPos = 0
        let right = this.ParseTestList()
        let mutable nodes : Token list = []
        while   match this.Lexer.Symbol with
                |   Token.Newline _ ->
                        nodes <- this.Lexer.Symbol :: nodes
                        this.Lexer.Advance()
                        true
                |   _ ->
                        false
            do ()
        let op =    match this.Lexer.Symbol with
                    |   Token.EOF _ ->
                            this.Lexer.Symbol
                    |   _ ->
                            raise ( SyntaxError(this.Lexer.Symbol, "Expecting end of file!") )
        ASTNode.EvalInput(startPos, this.Lexer.Position, right, List.toArray(List.rev nodes), op)

    member this.ParseFuncTypeInput() =
        let startPos = 0
        let right = this.ParseFuncType()
        let mutable nodes : Token list = []
        while   match this.Lexer.Symbol with
                |   Token.Newline _ ->
                        nodes <- this.Lexer.Symbol :: nodes
                        this.Lexer.Advance()
                        true
                |   _ ->
                        false
            do ()
        let op =    match this.Lexer.Symbol with
                    |   Token.EOF _ ->
                            this.Lexer.Symbol
                    |   _ ->
                            raise ( SyntaxError(this.Lexer.Symbol, "Expecting end of file!") )
        ASTNode.FuncTypeInput(startPos, this.Lexer.Position, right, List.toArray(List.rev nodes), op)

    // Block rules in Python 3.9 grammar //////////////////////////////////////////////////////////

    member this.ParseDecorated() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.Matrice _ ->
                let left = this.ParseDecorators()
                match this.Lexer.Symbol with
                |   Token.Class _ ->
                        let right = this.ParseClassStmt()
                        ASTNode.Decorated(startPos, this.Lexer.Position, left, right)
                |   Token.Def _ ->
                        let right = this.ParseFuncDefStmt()
                        ASTNode.Decorated(startPos, this.Lexer.Position, left, right)
                |   Token.Async _ ->
                        let right = this.ParseAsyncFuncDefStmt()
                        ASTNode.Decorated(startPos, this.Lexer.Position, left, right)
                |   _ ->
                        raise ( SyntaxError(this.Lexer.Symbol, "Expecting 'class', 'def' or 'async' after decorator statement!") )
        |   _ ->
                raise ( SyntaxError(this.Lexer.Symbol, "Expecting '@' in decorator statement!") )

    member this.ParseDecorators() =
        match this.Lexer.Symbol with
        |   Token.Matrice _ ->
                let startPos = this.Lexer.Position
                let mutable nodes : ASTNode list = []
                while   match this.Lexer.Symbol with
                        |   Token.Matrice _ ->
                                nodes <- this.ParseDecorator() :: nodes
                                true
                        |   _ ->
                                false
                    do ()
                ASTNode.Decorators(startPos, this.Lexer.Position, List.toArray(List.rev nodes))
        |   _ ->
                 raise ( SyntaxError(this.Lexer.Symbol, "Expecting '@' in decorator statement!") )

    member this.ParseDecorator() =
        match this.Lexer.Symbol with
        |   Token.Matrice _ ->
                let startPos = this.Lexer.Position
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                let left = this.ParseDottedNameStmt()
                match this.Lexer.Symbol with
                |   Token.LeftParen _ ->
                        let op3 = this.Lexer.Symbol
                        this.Lexer.Advance()
                        let right = this.ParseArgsList()
                        let op4 =   match this.Lexer.Symbol with
                                    |   Token.RightParen _ ->
                                            let tmpOp = this.Lexer.Symbol
                                            this.Lexer.Advance()
                                            tmpOp
                                    |   _ ->
                                            raise ( SyntaxError(this.Lexer.Symbol, "Expecting ')' in decorator statement!") )
                        match this.Lexer.Symbol with
                        |   Token.Newline _ ->
                                let op2 = this.Lexer.Symbol
                                this.Lexer.Advance()
                                ASTNode.Decorator(startPos, this.Lexer.Position, op, left, op3, right, op4, op2)
                        |   _ ->
                                raise ( SyntaxError(this.Lexer.Symbol, "Expecting newline after decorator statement!") )
                |   _ ->
                        match this.Lexer.Symbol with
                        |   Token.Newline _ ->
                                let op2 = this.Lexer.Symbol
                                this.Lexer.Advance()
                                ASTNode.Decorator(startPos, this.Lexer.Position, op, left, Token.Empty, ASTNode.Empty, Token.Empty, op2)
                        |   _ ->
                                raise ( SyntaxError(this.Lexer.Symbol, "Expecting newline after decorator statement!") )
        |   _ ->
                raise ( SyntaxError(this.Lexer.Symbol, "Expecting '@' in decorator statement!") )

    member this.ParseClassStmt() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.Class _ ->
                let op1 = this.Lexer.Symbol
                this.Lexer.Advance()
                match this.Lexer.Symbol with
                |   Token.Name _ ->
                        let start2 = this.Lexer.Position
                        let name = this.Lexer.Symbol
                        this.Lexer.Advance()
                        let left = ASTNode.Name(start2, this.Lexer.Position, name)
                        let op2, right, op3 =   match this.Lexer.Symbol with
                                                |   Token.LeftParen _ ->
                                                        let tmpOp1 = this.Lexer.Symbol
                                                        this.Lexer.Advance()
                                                        let tmpRight =  match this.Lexer.Symbol with
                                                                        |   Token.RightParen _ ->
                                                                                ASTNode.Empty
                                                                        |   _ ->
                                                                                this.ParseArgsList()
                                                        match this.Lexer.Symbol with
                                                        |   Token.RightParen _ ->
                                                                let tmpOp2 = this.Lexer.Symbol
                                                                this.Lexer.Advance()
                                                                tmpOp1, tmpRight, tmpOp2
                                                        |   _ ->
                                                                raise ( SyntaxError(this.Lexer.Symbol, "Expecting ')' in class declaration!") )
                                                |   _ ->
                                                        Token.Empty, ASTNode.Empty, Token.Empty
                        let op4 =   match this.Lexer.Symbol with  
                                    |   Token.Colon _ ->
                                            let tmpOp = this.Lexer.Symbol
                                            this.Lexer.Advance()
                                            tmpOp
                                    |   _ ->
                                            Token.Empty
                        let next = this.ParseSuite()
                        ASTNode.Class(startPos, this.Lexer.Position, op1, left, op2, right, op3, op4, next)
                |   _ ->
                        raise ( SyntaxError(this.Lexer.Symbol, "Expecting name of class!") )
        |   _ ->
                raise ( SyntaxError(this.Lexer.Symbol, "Expecting 'class' in class declaration!") )

    member this.ParseAsyncFuncDefStmt() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.Async _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                let right = this.ParseFuncDefStmt()
                ASTNode.AsyncFuncDef(startPos, this.Lexer.Position, op, right)
        |   _ ->
                raise ( SyntaxError(this.Lexer.Symbol, "Expecting 'async' in async function declaration!") )

    member this.ParseFuncDefStmt() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.Def _ ->
                this.FlowLevel <- this.FlowLevel + 1
                let op1 = this.Lexer.Symbol
                this.Lexer.Advance()
                match this.Lexer.Symbol with
                |   Token.Name _ ->
                        let start2 = this.Lexer.Position
                        let name = this.Lexer.Symbol
                        this.Lexer.Advance()
                        let left = ASTNode.Name(start2, this.Lexer.Position, name)
                        let right = this.ParseParameters()
                        let op2, next = match this.Lexer.Symbol with    // '->' test
                                        |   Token.Ptr _ ->
                                                let tmpOp1 = this.Lexer.Symbol
                                                this.Lexer.Advance()
                                                let tmpNext = this.ParseTest()
                                                tmpOp1, tmpNext
                                        |   _ ->
                                                Token.Empty, ASTNode.Empty
                        let op3 =   match this.Lexer.Symbol with
                                    |   Token.Colon _ ->
                                            let tmpOp2 = this.Lexer.Symbol
                                            this.Lexer.Advance()
                                            tmpOp2
                                    |   _ ->
                                            raise ( SyntaxError(this.Lexer.Symbol, "Expecting ':' in function declaration!") )
                        let op4 =   match this.Lexer.Symbol with
                                    |   Token.TypeComment _ ->
                                            let tmpOp3 = this.Lexer.Symbol
                                            this.Lexer.Advance()
                                            tmpOp3
                                    |   _ ->
                                            Token.Empty
                        let suite = this.ParseFuncBodySuite()
                        this.FlowLevel <- this.FlowLevel - 1
                        ASTNode.FuncDef(startPos, this.Lexer.Position, op1, left, right, op2, next, op3, op4, ASTNode.Empty)
                |   _ ->
                        raise ( SyntaxError(this.Lexer.Symbol, "Expecting name of function!") )
        |   _ ->
                raise ( SyntaxError(this.Lexer.Symbol, "Expecting 'def' in function declaration!") )

    member this.ParseParameters() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.LeftParen _ ->
                let op1 = this.Lexer.Symbol
                this.Lexer.Advance()
                let right = match this.Lexer.Symbol with
                            |   Token.RightParen _ ->
                                    ASTNode.Empty
                            |   _ ->
                                    this.ParseTypedArgsList()
                let op2 =   match this.Lexer.Symbol with
                            |   Token.RightParen _ ->
                                    let tmpOp2 = this.Lexer.Symbol
                                    this.Lexer.Advance()
                                    tmpOp2
                            |   _ ->
                                    raise ( SyntaxError(this.Lexer.Symbol, "Expecting ')' in function declaration!") )
                ASTNode.Parameters(startPos, this.Lexer.Position, op1, right, op2)
        | _ ->  
                raise ( SyntaxError(this.Lexer.Symbol, "Expected '(' in function declaration!") )

    member this.ParseTypedArgsList() =
        this.ParseCommonArgsList(true)

    member this.ParseVarArgsList() =
        this.ParseCommonArgsList()

    member private this.ParseCommonArgsList(?isTyped : bool) =
        let typed = match isTyped with Some x -> x | _ -> false
        let startPos = this.Lexer.Position
        let mutable nodes : ASTNode list = []
        let mutable ops : Token list = []
        let mutable com : Token list = []
        match this.Lexer.Symbol with
        |   Token.Mul _ ->
                let start2 = this.Lexer.Position
                let opMul = this.Lexer.Symbol
                this.Lexer.Advance()
                let node =  this.ParseCommonAssignment(typed)
                match typed with
                |   true ->
                        nodes <- ASTNode.TypedMul(start2, this.Lexer.Position, opMul, node) :: nodes
                |   _ ->
                        nodes <- ASTNode.VarMul(start2, this.Lexer.Position, opMul, node) :: nodes
                while   match this.Lexer.Symbol with
                        |   Token.Comma _ ->
                                ops <- this.Lexer.Symbol :: ops
                                this.Lexer.Advance()
                                let typeComm1 = match this.Lexer.Symbol with
                                                |   Token.TypeComment _ ->
                                                        let tmpOp = this.Lexer.Symbol
                                                        this.Lexer.Advance()
                                                        tmpOp
                                                |   _ ->
                                                        Token.Empty
                                com <- typeComm1 :: com
                                match this.Lexer.Symbol with
                                |   Token.Colon _
                                |   Token.RightParen _ ->
                                        false
                                |   Token.Power _ ->
                                        let start2 = this.Lexer.Position
                                        let opPower = this.Lexer.Symbol
                                        this.Lexer.Advance()
                                        let node = this.ParseCommonAssignment(typed)
                                        match typed with
                                        |   true ->
                                                nodes <- ASTNode.TypedPower(start2, this.Lexer.Position, opPower, node) :: nodes
                                        |   _ ->
                                                nodes <- ASTNode.VarPower(start2, this.Lexer.Position, opPower, node) :: nodes
                                        match this.Lexer.Symbol with
                                        |   Token.Comma _ ->
                                                ops <- this.Lexer.Symbol :: ops
                                                this.Lexer.Advance()
                                                let typeComm1 = match this.Lexer.Symbol with
                                                                |   Token.TypeComment _ ->
                                                                        let tmpOp = this.Lexer.Symbol
                                                                        this.Lexer.Advance()
                                                                        tmpOp
                                                                |   _ ->
                                                                        Token.Empty
                                                com <- typeComm1 :: com
                                        |   _ ->
                                                ()
                                        false
                                |   _ ->
                                        nodes <- this.ParseCommonAssignment(typed) :: nodes
                                        true
                        |   _ ->
                                false
                    do ()
        |   Token.Power _ ->
                let start2 = this.Lexer.Position
                let opPower = this.Lexer.Symbol
                this.Lexer.Advance()
                let node = this.ParseCommonAssignment(typed)
                match typed with
                |   true ->
                        nodes <- ASTNode.TypedPower(start2, this.Lexer.Position, opPower, node) :: nodes
                |   _ ->
                        nodes <- ASTNode.VarPower(start2, this.Lexer.Position, opPower, node) :: nodes
                match this.Lexer.Symbol with
                |   Token.Comma _ ->
                        ops <- this.Lexer.Symbol :: ops
                        this.Lexer.Advance()
                        let typeComm1 = match this.Lexer.Symbol with
                                        |   Token.TypeComment _ ->
                                                let tmpOp = this.Lexer.Symbol
                                                this.Lexer.Advance()
                                                tmpOp
                                        |   _ ->
                                                Token.Empty
                        com <- typeComm1 :: com
                |   _ ->
                        ()
        |   _ ->
                nodes <- this.ParseTest() :: nodes
                while   match this.Lexer.Symbol with
                        |   Token.Comma _ ->
                                ops <- this.Lexer.Symbol :: ops
                                this.Lexer.Advance()
                                let typeComm1 = match this.Lexer.Symbol with
                                                |   Token.TypeComment _ ->
                                                        let tmpOp = this.Lexer.Symbol
                                                        this.Lexer.Advance()
                                                        tmpOp
                                                |   _ ->
                                                        Token.Empty
                                com <- typeComm1 :: com
                                match this.Lexer.Symbol with
                                |   Token.Colon _
                                |   Token.RightParen _ ->
                                        false
                                |   Token.Power _ ->
                                        let start2 = this.Lexer.Position
                                        let opPower = this.Lexer.Symbol
                                        this.Lexer.Advance()
                                        let node = this.ParseCommonAssignment(typed)
                                        match typed with
                                        |   true ->
                                                nodes <- ASTNode.TypedPower(start2, this.Lexer.Position, opPower, node) :: nodes
                                        |   _ ->
                                                nodes <- ASTNode.VarPower(start2, this.Lexer.Position, opPower, node) :: nodes
                                        match this.Lexer.Symbol with
                                        |   Token.Comma _ ->
                                                ops <- this.Lexer.Symbol :: ops
                                                this.Lexer.Advance()
                                                let typeComm1 = match this.Lexer.Symbol with
                                                                |   Token.TypeComment _ ->
                                                                        let tmpOp = this.Lexer.Symbol
                                                                        this.Lexer.Advance()
                                                                        tmpOp
                                                                |   _ ->
                                                                        Token.Empty
                                                com <- typeComm1 :: com
                                        |   _ ->
                                                ()
                                        false
                                |   Token.Mul _ ->
                                        let start2 = this.Lexer.Position
                                        let opMul = this.Lexer.Symbol
                                        this.Lexer.Advance()
                                        let node =  this.ParseCommonAssignment(typed)
                                        match typed with
                                        |   true ->
                                                nodes <- ASTNode.TypedMul(start2, this.Lexer.Position, opMul, node) :: nodes
                                        |   _ ->
                                                nodes <- ASTNode.VarMul(start2, this.Lexer.Position, opMul, node) :: nodes
                                        while   match this.Lexer.Symbol with
                                                |   Token.Comma _ ->
                                                        ops <- this.Lexer.Symbol :: ops
                                                        this.Lexer.Advance()
                                                        let typeComm1 = match this.Lexer.Symbol with
                                                                        |   Token.TypeComment _ ->
                                                                                let tmpOp = this.Lexer.Symbol
                                                                                this.Lexer.Advance()
                                                                                tmpOp
                                                                        |   _ ->
                                                                                Token.Empty
                                                        com <- typeComm1 :: com
                                                        match this.Lexer.Symbol with
                                                        |   Token.Colon _
                                                        |   Token.RightParen _ ->
                                                                false
                                                        |   Token.Power _ ->
                                                                let start2 = this.Lexer.Position
                                                                let opPower = this.Lexer.Symbol
                                                                this.Lexer.Advance()
                                                                let node = this.ParseCommonAssignment(typed)
                                                                match typed with
                                                                |   true ->
                                                                        nodes <- ASTNode.TypedPower(start2, this.Lexer.Position, opPower, node) :: nodes
                                                                |   _ ->
                                                                        nodes <- ASTNode.VarPower(start2, this.Lexer.Position, opPower, node) :: nodes
                                                                match this.Lexer.Symbol with
                                                                |   Token.Comma _ ->
                                                                        ops <- this.Lexer.Symbol :: ops
                                                                        this.Lexer.Advance()
                                                                        let typeComm1 = match this.Lexer.Symbol with
                                                                                        |   Token.TypeComment _ ->
                                                                                                let tmpOp = this.Lexer.Symbol
                                                                                                this.Lexer.Advance()
                                                                                                tmpOp
                                                                                        |   _ ->
                                                                                                Token.Empty
                                                                        com <- typeComm1 :: com
                                                                |   _ ->
                                                                        ()
                                                                false
                                                        |   _ ->
                                                                nodes <- this.ParseCommonAssignment(typed) :: nodes
                                                                true
                                                |   _ ->
                                                        false
                                            do ()
                                        false
                                |   _ ->
                                        nodes <- this.ParseCommonAssignment(typed) :: nodes
                                        true
                        |   _ ->
                                false
                    do ()
        match typed with
        |   true ->
                ASTNode.TypedArgsList(startPos, this.Lexer.Position, List.toArray(List.rev nodes), List.toArray(List.rev ops), List.toArray(List.rev com))
        |   _ ->
                ASTNode.VarArgsList(startPos, this.Lexer.Position, List.toArray(List.rev nodes), List.toArray(List.rev ops), List.toArray(List.rev com))

    member private this.ParseCommonAssignment(?isTyped : bool) =
        let typed = match isTyped with Some x -> x | _ -> false
        let startPos = this.Lexer.Position
        let left =  match typed with
                    |   true ->
                            this.ParseTFPDef()
                    |   _ ->
                            this.ParseVFPDef()
        match this.Lexer.Symbol with
        |   Token.Assign _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                let right = this.ParseTest()
                ASTNode.Assign(startPos, this.Lexer.Position, left, Token.Empty, op, right)
        |   _ ->
                left

    member this.ParseTFPDef() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.Name _ ->
                let name = this.Lexer.Symbol
                this.Lexer.Advance()
                let left = ASTNode.Name(startPos, this.Lexer.Position, name)
                let op1, right =    match this.Lexer.Symbol with
                                    |   Token.Colon _ ->
                                            let tmpOp = this.Lexer.Symbol
                                            this.Lexer.Advance()
                                            let tmpRight = this.ParseTest()
                                            tmpOp, tmpRight
                                    |   _ ->
                                            Token.Empty, ASTNode.Empty
                ASTNode.TFPDef(startPos, this.Lexer.Position, left, op1, right)
        |   _ ->
                raise ( SyntaxError(this.Lexer.Symbol, "Expecting name literal in argument!") )

    member this.ParseVFPDef() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.Name _ ->
                let name = this.Lexer.Symbol
                this.Lexer.Advance()
                ASTNode.Name(startPos, this.Lexer.Position, name)
        |   _ ->
                raise ( SyntaxError(this.Lexer.Symbol, "Expecting name literal in argument!") )

    // Simple Statement rules in Python 3.9 grammar ///////////////////////////////////////////////

    member this.ParseStmt() =
        match this.Lexer.Symbol with
        |   Token.If _
        |   Token.While _
        |   Token.For _
        |   Token.Try _
        |   Token.With _
        |   Token.Def _
        |   Token.Class _
        |   Token.Matrice _
        |   Token.Async _ ->
                this.ParseCompoundStmt()
        |   _   ->
                this.ParseSimpleStmt()

    member this.ParseSimpleStmt() =
        let startPos = this.Lexer.Position
        let mutable nodes : ASTNode list = []
        let mutable ops : Token list = []
        nodes <- this.ParseSmallStmt() :: nodes
        while   match this.Lexer.Symbol with
                |   Token.SemiColon _   ->
                        ops <- this.Lexer.Symbol :: ops
                        this.Lexer.Advance()
                        nodes <- this.ParseSmallStmt() :: nodes
                        true
                |   _   ->
                        false
            do ()
        let op =    match this.Lexer.Symbol with
                    |   Token.Newline _ ->
                            let tmpOp = this.Lexer.Symbol
                            this.Lexer.Advance()
                            tmpOp
                    |   _ ->
                            raise ( SyntaxError(this.Lexer.Symbol, "Expecting newline after statements!") )
        ASTNode.SimpleStmtList(startPos, this.Lexer.Position, List.toArray(List.rev nodes), List.toArray(List.rev ops), op)

    member this.ParseSmallStmt() =
        match this.Lexer.Symbol with
        |   Token.Del _ ->
                this.ParseDelStmt()
        |   Token.Pass _ ->
                this.ParsePassStmt()
        |   Token.Break _
        |   Token.Continue _
        |   Token.Return _
        |   Token.Raise _
        |   Token.Yield _ ->
                this.ParseFlowStmt()
        |   Token.Import _
        |   Token.From _ ->
                this.ParseImportStmt()
        |   Token.Global _ ->
                this.ParseGlobalStmt()
        |   Token.Nonlocal _ ->
                this.ParseNonlocalStmt()
        |   Token.Assert _ ->
                this.ParseAssertStmt()
        |   _ ->
                this.ParseExprStmt()

    member this.ParseExprStmt() =
        let startPos = this.Lexer.Position
        let left = this.ParseTestListStarExpr()
        match this.Lexer.Symbol with
        |   Token.PlusAssign _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                let right = match this.Lexer.Symbol with | Token.Yield _ -> this.ParseYieldExpr() | _ -> this.ParseTestList()
                ASTNode.PlusAssign(startPos, this.Lexer.Position, left, op, right)
        |   Token.MinusAssign _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                let right = match this.Lexer.Symbol with | Token.Yield _ -> this.ParseYieldExpr() | _ -> this.ParseTestList()
                ASTNode.MinusAssign(startPos, this.Lexer.Position, left, op, right)
        |   Token.MulAssign _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                let right = match this.Lexer.Symbol with | Token.Yield _ -> this.ParseYieldExpr() | _ -> this.ParseTestList()
                ASTNode.MulAssign(startPos, this.Lexer.Position, left, op, right)
        |   Token.PowerAssign _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                let right = match this.Lexer.Symbol with | Token.Yield _ -> this.ParseYieldExpr() | _ -> this.ParseTestList()
                ASTNode.PowerAssign(startPos, this.Lexer.Position, left, op, right)
        |   Token.FloorDivAssign _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                let right = match this.Lexer.Symbol with | Token.Yield _ -> this.ParseYieldExpr() | _ -> this.ParseTestList()
                ASTNode.FloorDivAssign(startPos, this.Lexer.Position, left, op, right)
        |   Token.DivAssign _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                let right = match this.Lexer.Symbol with | Token.Yield _ -> this.ParseYieldExpr() | _ -> this.ParseTestList()
                ASTNode.DivAssign(startPos, this.Lexer.Position, left, op, right)
        |   Token.ModuloAssign _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                let right = match this.Lexer.Symbol with | Token.Yield _ -> this.ParseYieldExpr() | _ -> this.ParseTestList()
                ASTNode.ModuloAssign(startPos, this.Lexer.Position, left, op, right)
        |   Token.MatriceAssign _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                let right = match this.Lexer.Symbol with | Token.Yield _ -> this.ParseYieldExpr() | _ -> this.ParseTestList()
                ASTNode.MatriceAssign(startPos, this.Lexer.Position, left, op, right)
        |   Token.BitAndAssign _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                let right = match this.Lexer.Symbol with | Token.Yield _ -> this.ParseYieldExpr() | _ -> this.ParseTestList()
                ASTNode.AndAssign(startPos, this.Lexer.Position, left, op, right)
        |   Token.BitOrAssign _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                let right = match this.Lexer.Symbol with | Token.Yield _ -> this.ParseYieldExpr() | _ -> this.ParseTestList()
                ASTNode.OrAssign(startPos, this.Lexer.Position, left, op, right)
        |   Token.BitXorAssign _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                let right = match this.Lexer.Symbol with | Token.Yield _ -> this.ParseYieldExpr() | _ -> this.ParseTestList()
                ASTNode.XorAssign(startPos, this.Lexer.Position, left, op, right)
        |   Token.ShiftLeftAssign _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                let right = match this.Lexer.Symbol with | Token.Yield _ -> this.ParseYieldExpr() | _ -> this.ParseTestList()
                ASTNode.ShiftLeftAssign(startPos, this.Lexer.Position, left, op, right)
        |   Token.ShiftRightAssign _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                let right = match this.Lexer.Symbol with | Token.Yield _ -> this.ParseYieldExpr() | _ -> this.ParseTestList()
                ASTNode.ShiftRightAssign(startPos, this.Lexer.Position, left, op, right)
        |   Token.Colon _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                let rigth = this.ParseTest()
                match this.Lexer.Symbol with
                |   Token.Assign _ ->
                        let op2 = this.Lexer.Symbol
                        this.Lexer.Advance()
                        let next = match this.Lexer.Symbol with | Token.Yield _ -> this.ParseYieldExpr() | _ -> this.ParseTestListStarExpr()
                        ASTNode.AnnAssign(startPos, this.Lexer.Position, left, op, rigth, op2, next)
                |   _ ->
                        ASTNode.AnnAssign(startPos, this.Lexer.Position, left, op, rigth, Token.Empty, ASTNode.Empty)          
        |   Token.Assign _ ->
                let mutable res = left
                while   match this.Lexer.Symbol with
                        |   Token.Assign _ ->
                                let op = this.Lexer.Symbol
                                this.Lexer.Advance()
                                let right = match this.Lexer.Symbol with | Token.Yield _ -> this.ParseYieldExpr() | _ -> this.ParseTestListStarExpr()
                                let op2 =   match this.Lexer.Symbol with
                                            |   Token.TypeComment _ ->
                                                    let tmpOp = this.Lexer.Symbol
                                                    this.Lexer.Advance()
                                                    tmpOp
                                            |   _ ->
                                                    Token.Empty
                                res <- ASTNode.Assign(startPos, this.Lexer.Position, res, op, op2, right)
                                true
                        |   _ ->
                                false
                    do ()
                res
        |   _   ->
                left

    member this.ParseTestListStarExpr() =
        let startPos = this.Lexer.Position
        let mutable nodes : ASTNode list = []
        let mutable ops : Token list = []
        nodes <- ( match this.Lexer.Symbol with | Token.Mul _ -> this.ParseStarExpr() | _ -> this.ParseTest() ) :: nodes
        while   match this.Lexer.Symbol with
                |   Token.Comma _ ->
                        ops <- this.Lexer.Symbol :: ops
                        this.Lexer.Advance()
                        match this.Lexer.Symbol with
                        |   Token.PlusAssign _
                        |   Token.MinusAssign _
                        |   Token.MulAssign _
                        |   Token.PowerAssign _
                        |   Token.FloorDivAssign _
                        |   Token.DivAssign _
                        |   Token.ModuloAssign _
                        |   Token.MatriceAssign _
                        |   Token.BitAndAssign _
                        |   Token.BitOrAssign _
                        |   Token.BitXorAssign _
                        |   Token.ShiftLeftAssign _
                        |   Token.ShiftRightAssign _
                        |   Token.Colon _
                        |   Token.Assign _
                        |   Token.Newline _
                        |   Token.SemiColon _ ->
                                false
                        |   _ ->
                               nodes <- ( match this.Lexer.Symbol with | Token.Mul _ -> this.ParseStarExpr() | _ -> this.ParseTest() ) :: nodes
                               true
                |   _ ->
                        false
            do ()
        ASTNode.TestList(startPos, this.Lexer.Position, List.toArray(List.rev nodes), List.toArray(List.rev ops))

    member this.ParseDelStmt() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.Del _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                let right = this.ParseExprList()
                ASTNode.Del(startPos, this.Lexer.Position, op, right)
        |   _ ->
                raise ( SyntaxError(this.Lexer.Symbol, "Expecting 'del' in del statement!") )

    member this.ParsePassStmt() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.Pass _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                ASTNode.Pass(startPos, this.Lexer.Position, op)
        |   _ ->
                raise ( SyntaxError(this.Lexer.Symbol, "Expecting 'pass' in pass statement!") )

    member this.ParseFlowStmt() =
        match this.Lexer.Symbol, this.FuncFlowLevel, this.FlowLevel with
        |   Token.Return _ , _ , _ when this.FuncFlowLevel <= 0 ->
                raise ( SyntaxError(this.Lexer.Symbol, "Found 'return' outside of function!") )
        |   Token.Return _ , _ , _ ->
                this.ParseReturnStmt()
        |   Token.Break _ , _ , _ when this.FuncFlowLevel <= 0 ->
                raise ( SyntaxError(this.Lexer.Symbol, "Found 'break' outside of loop statement!") )
        |   Token.Break _ , _ , _ ->
                this.ParseBreakStmt()
        |   Token.Continue _ , _ , _ when this.FuncFlowLevel <= 0 ->
                raise ( SyntaxError(this.Lexer.Symbol, "Found 'continue' outside of loop statement!") )
        |   Token.Continue _ , _ , _ ->
                this.ParseContinueStmt()
        |   Token.Raise _ , _ , _ when this.FuncFlowLevel <= 0 ->
                raise ( SyntaxError(this.Lexer.Symbol, "Found 'raise' outside of loop statement!") )
        |   Token.Raise _ , _ , _ ->
                this.ParseRaiseStmt()
        |   Token.Yield _ , _ , _ when this.FuncFlowLevel <= 0 ->
                raise ( SyntaxError(this.Lexer.Symbol, "Found 'yield' outside of loop statement!") )
        |   Token.Yield _ , _ , _ ->
                this.ParseYieldStmt()
        |   _ ->
                raise ( SyntaxError(this.Lexer.Symbol, "Illegal flow statement!") )

    member this.ParseBreakStmt() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.Break _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                ASTNode.Break(startPos, this.Lexer.Position, op)
        | _ ->
                raise ( SyntaxError(this.Lexer.Symbol, "Expecting 'break' statement!") )

    member this.ParseContinueStmt() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.Continue _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                ASTNode.Continue(startPos, this.Lexer.Position, op)
        | _ ->
                raise ( SyntaxError(this.Lexer.Symbol, "Expecting 'continue' statement!") )

    member this.ParseReturnStmt() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.Return _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                match this.Lexer.Symbol with
                |   Token.Newline _
                |   Token.SemiColon _ ->
                        ASTNode.Return(startPos, this.Lexer.Position, op, ASTNode.Empty)
                |   _ ->
                        let right = this.ParseTestListStarExpr()
                        ASTNode.Return(startPos, this.Lexer.Position, op, right)
        | _ ->
                raise ( SyntaxError(this.Lexer.Symbol, "Expecting 'return' statement!") )

    member this.ParseYieldStmt() =
        this.ParseYieldExpr()

    member this.ParseRaiseStmt() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.Raise _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                match this.Lexer.Symbol with
                |   Token.Newline _
                |   Token.SemiColon _ ->
                        ASTNode.Raise(startPos, this.Lexer.Position, op, ASTNode.Empty, Token.Empty, ASTNode.Empty)
                |   _ ->
                        let left = this.ParseTest()
                        match this.Lexer.Symbol with
                        |   Token.From _ ->
                                let op2 = this.Lexer.Symbol
                                this.Lexer.Advance()
                                let right = this.ParseTest()
                                ASTNode.Raise(startPos, this.Lexer.Position, op, left, op2, right)
                        |   _ ->
                            ASTNode.Raise(startPos, this.Lexer.Position, op, left, Token.Empty, ASTNode.Empty)
        | _ ->
                raise ( SyntaxError(this.Lexer.Symbol, "Expecting 'raise' statement!") )

    member this.ParseImportStmt() =
        match this.Lexer.Symbol with
        |   Token.Import _ ->
                this.ParseImportNameStmt()
        |   Token.From _ ->
                this.ParseImportFromStmt()
        |   _ ->
                raise ( SyntaxError(this.Lexer.Symbol, "Expected 'import' or 'from' in import statement!") )

    member this.ParseImportNameStmt() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.Import _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                let right = this.ParseDottedAsNameStmt()
                ASTNode.Import(startPos, this.Lexer.Position, op, right)
        |   _ ->
                raise ( SyntaxError(this.Lexer.Symbol, "Expecting 'import' in import statement!") )

    member this.ParseImportFromStmt() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.From _ ->
                let op1 = this.Lexer.Symbol
                this.Lexer.Advance()
                let mutable dots : Token list = []
                while   match this.Lexer.Symbol with
                        |   Token.Dot _
                        |   Token.Elipsis _ ->
                                dots <- this.Lexer.Symbol :: dots
                                this.Lexer.Advance()
                                true
                        |   _ ->
                                false
                    do ()
                let left =  match this.Lexer.Symbol, dots.Length with
                            |   Token.Import _ , _ when dots.Length = 0 ->
                                    raise ( SyntaxError(this.Lexer.Symbol, "Minimum one '.' in from part of import statement!") )
                            |   Token.Import _ , _ ->
                                    ASTNode.Empty
                            |   _ ->
                                    this.ParseDottedNameStmt()
                match this.Lexer.Symbol with
                |   Token.Import _ ->
                        let op2 = this.Lexer.Symbol
                        this.Lexer.Advance()
                        match this.Lexer.Symbol with
                        |   Token.Mul _ ->
                                let op3 = this.Lexer.Symbol
                                this.Lexer.Advance()
                                ASTNode.ImportFrom(startPos, this.Lexer.Position, op1, List.toArray(List.rev dots), left, op2, op3, ASTNode.Empty, Token.Empty)
                        |   Token.LeftParen _ ->
                                let op4 = this.Lexer.Symbol
                                this.Lexer.Advance()
                                let right = this.ParseImportAsNamesStmt()
                                let op5 =   match this.Lexer.Symbol with
                                            |   Token.RightParen _ ->
                                                   let tmpOp = this.Lexer.Symbol
                                                   this.Lexer.Advance()
                                                   tmpOp
                                            |   _ ->
                                                    raise ( SyntaxError(this.Lexer.Symbol, "Expecting ')' in import statement!") )
                                ASTNode.ImportFrom(startPos, this.Lexer.Position, op1, List.toArray(List.rev dots), left, op2, op4, right, op5)
                        |   _ ->
                                let right = this.ParseImportAsNamesStmt()
                                ASTNode.ImportFrom(startPos, this.Lexer.Position, op1, List.toArray(List.rev dots), left, op2, Token.Empty, right, Token.Empty)
                |   _ ->
                        raise ( SyntaxError(this.Lexer.Symbol, "Expected 'import' in import from statement!" ) )
        |   _ ->
                raise ( SyntaxError(this.Lexer.Symbol, "Expecting 'from' in imort statement!") )

    member this.ParseImportAsNameStmt() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.Name _ ->
                let name1 = ASTNode.Name(startPos, this.Lexer.Position, this.Lexer.Symbol)
                this.Lexer.Advance()
                match this.Lexer.Symbol with
                |   Token.As _ ->
                        let op = this.Lexer.Symbol
                        this.Lexer.Advance()
                        match this.Lexer.Symbol with
                        |   Token.Name _ ->
                                let name2 = ASTNode.Name(startPos, this.Lexer.Position, this.Lexer.Symbol)
                                this.Lexer.Advance()
                                ASTNode.ImportAsName(startPos, this.Lexer.Position, name1, op, name2)
                        |   _ ->
                                raise ( SyntaxError(this.Lexer.Symbol, "") )
                |   _ ->
                        raise ( SyntaxError(this.Lexer.Symbol, "") )
        |   _ ->
                raise ( SyntaxError(this.Lexer.Symbol, "") )

    member this.ParseDottedAsNameStmt() =
        let startPos = this.Lexer.Position
        let left = this.ParseDottedNameStmt()
        match this.Lexer.Symbol with
        |   Token.As _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                match this.Lexer.Symbol with
                |   Token.Name _ ->
                        let name = ASTNode.Name(startPos, this.Lexer.Position, this.Lexer.Symbol)
                        this.Lexer.Advance()
                        ASTNode.DottedAsName(startPos, this.Lexer.Position, left, op, name)
                |   _ ->
                        raise ( SyntaxError(this.Lexer.Symbol, "Expecting name literal after 'as'") )
        |   _ ->
                ASTNode.DottedAsName(startPos, this.Lexer.Position, left, Token.Empty, ASTNode.Empty)

    member this.ParseImportAsNamesStmt() =
        let startPos = this.Lexer.Position
        let mutable nodes : ASTNode list = []
        let mutable ops : Token list = []
        nodes <- this.ParseImportAsNameStmt() :: nodes
        while   match this.Lexer.Symbol with
                |   Token.Comma _ ->
                        ops <- this.Lexer.Symbol :: ops
                        this.Lexer.Advance()
                        nodes <- this.ParseImportAsNameStmt() :: nodes
                        true
                |   _ ->
                        false
            do ()
        ASTNode.ImportAsNames(startPos, this.Lexer.Position, List.toArray(List.rev nodes), List.toArray(List.rev ops))

    member this.ParseDottedAsNamesStmt() =
        let startPos = this.Lexer.Position
        let mutable nodes : ASTNode list = []
        let mutable ops : Token list = []
        nodes <- this.ParseDottedAsNameStmt() :: nodes
        while   match this.Lexer.Symbol with
                |   Token.Comma _ ->
                        ops <- this.Lexer.Symbol :: ops
                        this.Lexer.Advance()
                        nodes <- this.ParseDottedAsNameStmt() :: nodes
                        true
                |   _ ->
                        false
            do ()
        ASTNode.DottedAsNames(startPos, this.Lexer.Position, List.toArray(List.rev nodes), List.toArray(List.rev ops))

    member this.ParseDottedNameStmt() =
        let startPos = this.Lexer.Position
        let mutable nodes : ASTNode list = []
        let mutable ops : Token list = []
        match this.Lexer.Symbol with
        |   Token.Name _ ->
                nodes <- ASTNode.Name(startPos, this.Lexer.Position, this.Lexer.Symbol) :: nodes
                this.Lexer.Advance()
                while   match this.Lexer.Symbol with
                        |   Token.Dot _ ->
                                ops <- this.Lexer.Symbol :: ops
                                this.Lexer.Advance()
                                match this.Lexer.Symbol with
                                |   Token.Name _ ->
                                        nodes <- ASTNode.Name(startPos, this.Lexer.Position, this.Lexer.Symbol) :: nodes
                                        this.Lexer.Advance()
                                |   _ ->
                                        raise ( SyntaxError(this.Lexer.Symbol, "Expecting name literal after '.'") )
                                true
                        |   _ ->
                                false
                    do ()
                ASTNode.DottedName(startPos, this.Lexer.Position, List.toArray(List.rev nodes), List.toArray(List.rev ops))
        |   _ ->
                raise ( SyntaxError(this.Lexer.Symbol, "Missing name literal!") )

    member this.ParseGlobalStmt() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.Global _ ->
                let mutable nodes : ASTNode list = []
                let mutable ops : Token list = []
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                match this.Lexer.Symbol with
                |   Token.Name _ ->
                        let name1 = ASTNode.Name(startPos, this.Lexer.Position, this.Lexer.Symbol)
                        this.Lexer.Advance()
                        nodes <- name1 :: nodes
                        while   match this.Lexer.Symbol with
                                |   Token.Comma _ ->
                                        ops <- this.Lexer.Symbol :: ops
                                        this.Lexer.Advance()
                                        match this.Lexer.Symbol with
                                        |   Token.Name _ ->
                                                let name2 = ASTNode.Name(startPos, this.Lexer.Position, this.Lexer.Symbol)
                                                this.Lexer.Advance()
                                                nodes <- name2 :: nodes
                                                true
                                        |   _ ->
                                                raise ( SyntaxError(this.Lexer.Symbol, "Expecting name literal after ',' in global statement!") )
                                |   _ ->
                                        false
                            do ()
                        ASTNode.Global(startPos, this.Lexer.Position, op, List.toArray(List.rev nodes), List.toArray(List.rev ops))
                |   _ ->
                    raise ( SyntaxError(this.Lexer.Symbol, "Expecting name literal in global statement!") )
        |   _ ->
            raise ( SyntaxError(this.Lexer.Symbol, "Expecting 'global' in global statement!") )

    member this.ParseNonlocalStmt() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.Nonlocal _ ->
                let mutable nodes : ASTNode list = []
                let mutable ops : Token list = []
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                match this.Lexer.Symbol with
                |   Token.Name _ ->
                        let name1 = ASTNode.Name(startPos, this.Lexer.Position, this.Lexer.Symbol)
                        this.Lexer.Advance()
                        nodes <- name1 :: nodes
                        while   match this.Lexer.Symbol with
                                |   Token.Comma _ ->
                                        ops <- this.Lexer.Symbol :: ops
                                        this.Lexer.Advance()
                                        match this.Lexer.Symbol with
                                        |   Token.Name _ ->
                                                let name2 = ASTNode.Name(startPos, this.Lexer.Position, this.Lexer.Symbol)
                                                this.Lexer.Advance()
                                                nodes <- name2 :: nodes
                                                true
                                        |   _ ->
                                                raise ( SyntaxError(this.Lexer.Symbol, "Expecting name literal after ',' in nonlocal statement!") )
                                |   _ ->
                                        false
                            do ()
                        ASTNode.Nonlocal(startPos, this.Lexer.Position, op, List.toArray(List.rev nodes), List.toArray(List.rev ops))
                |   _ ->
                    raise ( SyntaxError(this.Lexer.Symbol, "Expecting name literal in nonlocal statement!") )
        |   _ ->
            raise ( SyntaxError(this.Lexer.Symbol, "Expecting 'nonlocal' in nonlocal statement!") )

    member this.ParseAssertStmt() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.Assert _ ->
                let op1 = this.Lexer.Symbol
                this.Lexer.Advance()
                let left = this.ParseTest()
                match this.Lexer.Symbol with
                |   Token.Comma _ ->
                        let op2 = this.Lexer.Symbol
                        this.Lexer.Advance()
                        let right = this.ParseTest()
                        ASTNode.Assert(startPos, this.Lexer.Position, op1, left, op2, right)
                |   _ ->
                        ASTNode.Assert(startPos, this.Lexer.Position, op1, left, Token.Empty, ASTNode.Empty)
        |   _ ->
                raise ( SyntaxError(this.Lexer.Symbol, "Expecting 'assert' in assert statement!") )

    // Compound Statement rules in Python 3.9 grammar /////////////////////////////////////////////
    
    member this.ParseCompoundStmt() =
        match this.Lexer.Symbol with
        |   Token.If _ -> this.ParseIfStmt()
        |   Token.While _ -> this.ParseWhileStmt()
        |   Token.For _ -> this.ParseForStmt()
        |   Token.Try _ -> this.ParseTryStmt()
        |   Token.With _ -> this.ParseWithStmt()
        |   Token.Def _ -> this.ParseFuncDefStmt()
        |   Token.Class _ -> this.ParseClassStmt()
        |   Token.Matrice _ -> this.ParseDecorated()
        |   Token.Async _ -> this.ParseAsyncStmt()
        |   _ -> raise ( SyntaxError(this.Lexer.Symbol, "Unexpected statement!") )

    member this.ParseAsyncStmt() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.Async _ ->
                let op1 = this.Lexer.Symbol
                this.Lexer.Advance()
                match this.Lexer.Symbol with
                |   Token.Def _ ->
                        let right = this.ParseFuncDefStmt()
                        ASTNode.AsyncFuncDef(startPos, this.Lexer.Position, op1, right)
                |   Token.With _ 
                |   Token.For _ ->
                        let right = this.ParseStmt()
                        ASTNode.AsyncStmt(startPos, this.Lexer.Position, op1, right)
                |   _ ->
                        raise ( SyntaxError(this.Lexer.Symbol, "Expecting either 'def', 'with' or 'for' in async statement!") )
        |   _   ->
                raise ( SyntaxError(this.Lexer.Symbol, "Expecting 'async' in async statement!") )

    member this.ParseIfStmt() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.If _ ->
                this.FlowLevel <- this.FlowLevel + 1
                let op1 = this.Lexer.Symbol
                this.Lexer.Advance()
                let left = this.ParseNamedExpr()
                match this.Lexer.Symbol with
                |   Token.Colon _ ->
                        let op2 = this.Lexer.Symbol
                        this.Lexer.Advance()
                        let right = this.ParseSuite()
                        let mutable nodes : ASTNode list = []
                        while   match this.Lexer.Symbol with
                                |   Token.Elif _ ->
                                        let start2 = this.Lexer.Position
                                        let op3 = this.Lexer.Symbol
                                        this.Lexer.Advance()
                                        let left2 = this.ParseNamedExpr()
                                        match this.Lexer.Symbol with
                                        |   Token.Colon _ ->
                                                let op4 = this.Lexer.Symbol
                                                this.Lexer.Advance()
                                                let right2 = this.ParseSuite()
                                                nodes <- ASTNode.Elif(start2, this.Lexer.Position, op3, left2, op4, right2) :: nodes
                                        |   _   ->
                                                raise ( SyntaxError(this.Lexer.Symbol, "Missing ':' in elif statement!") )
                                        true
                                |   _   ->  
                                        false
                            do ()
                        match this.Lexer.Symbol with
                        |   Token.Else _    ->
                                let start3 = this.Lexer.Position
                                let op5 = this.Lexer.Symbol
                                this.Lexer.Advance()
                                match this.Lexer.Symbol with
                                |   Token.Colon _ ->
                                        let op6 = this.Lexer.Symbol
                                        this.Lexer.Advance()
                                        let right3 = this.ParseSuite()
                                        let node = ASTNode.Else(start3, this.Lexer.Position, op5, op6, right3)
                                        this.FlowLevel <- this.FlowLevel - 1
                                        ASTNode.If(startPos, this.Lexer.Position, op1, left, op2, right, List.toArray(List.rev nodes), node)
                                |   _ ->
                                        raise ( SyntaxError(this.Lexer.Symbol, "Expected ':' in else statement!") )
                        |   _   ->
                                this.FlowLevel <- this.FlowLevel - 1
                                ASTNode.If(startPos, this.Lexer.Position, op1, left, op2, right, List.toArray(List.rev nodes), ASTNode.Empty)
                |   _ -> 
                        raise ( SyntaxError(this.Lexer.Symbol, "Expecting ':' in if statement!") )
        |   _   ->
                raise ( SyntaxError(this.Lexer.Symbol, "Expecting 'if' statement!") )

    member this.ParseWhileStmt() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.While _ ->
                this.FlowLevel <- this.FlowLevel + 1
                let op1 = this.Lexer.Symbol
                this.Lexer.Advance()
                let left = this.ParseNamedExpr()
                match this.Lexer.Symbol with
                |   Token.Colon _ ->
                        let op2 = this.Lexer.Symbol
                        this.Lexer.Advance()
                        let right = this.ParseSuite()
                        match this.Lexer.Symbol with
                        |   Token.Else _ ->
                                let start2 = this.Lexer.Position
                                let op3 = this.Lexer.Symbol
                                this.Lexer.Advance()
                                match this.Lexer.Symbol with
                                |   Token.Colon _ ->
                                        let op4 = this.Lexer.Symbol
                                        this.Lexer.Advance()
                                        let right2 = this.ParseSuite()
                                        let node = ASTNode.Else(start2, this.Lexer.Position, op3, op4, right2)
                                        this.FlowLevel <- this.FlowLevel - 1
                                        ASTNode.While(startPos, this.Lexer.Position, op1, left, op2, right, node)
                                |   _ ->
                                        raise ( SyntaxError(this.Lexer.Symbol, "Expected ':' in else statement!") )
                        |   _ ->
                                this.FlowLevel <- this.FlowLevel - 1
                                ASTNode.While(startPos, this.Lexer.Position, op1, left, op2, right, ASTNode.Empty)
                |   _ ->
                        raise ( SyntaxError(this.Lexer.Symbol, "Missing ':' in while statement!") )
        |   _ ->
                raise ( SyntaxError(this.Lexer.Symbol, "Expected 'while' statement!") )

    member this.ParseForStmt() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.For _ ->
                this.FlowLevel <- this.FlowLevel + 1
                let op1 = this.Lexer.Symbol
                this.Lexer.Advance()
                let left = this.ParseExprList()
                match this.Lexer.Symbol with
                |   Token.In _ ->
                        let op2 = this.Lexer.Symbol
                        this.Lexer.Advance()
                        let right = this.ParseTestList()
                        match this.Lexer.Symbol with
                        |   Token.Colon _ ->
                                let op3 = this.Lexer.Symbol
                                this.Lexer.Advance()
                                let op4 =   match this.Lexer.Symbol with
                                            |   Token.TypeComment _ ->
                                                    let tmpOp = this.Lexer.Symbol
                                                    this.Lexer.Advance()
                                                    tmpOp
                                            |   _   ->
                                                    Token.Empty
                                let next = this.ParseSuite()
                                match this.Lexer.Symbol with
                                |   Token.Else _ ->
                                        let start2 = this.Lexer.Position
                                        let op5 = this.Lexer.Symbol
                                        this.Lexer.Advance()
                                        match this.Lexer.Symbol with
                                        |   Token.Colon _ ->
                                                let op6 = this.Lexer.Symbol
                                                this.Lexer.Advance()
                                                let right2 = this.ParseSuite()
                                                let node = ASTNode.Else(start2, this.Lexer.Position, op5, op6, right2)
                                                this.FlowLevel <- this.FlowLevel - 1
                                                ASTNode.For(startPos, this.Lexer.Position, op1, left, op2, right, op3, op4, next, node)
                                        |   _ ->
                                                raise ( SyntaxError(this.Lexer.Symbol, "Expected ':' in else statement!") )
                                |   _   ->
                                        this.FlowLevel <- this.FlowLevel - 1
                                        ASTNode.For(startPos, this.Lexer.Position, op1, left, op2, right, op3, op4, next, ASTNode.Empty)
                        |   _   ->
                                raise ( SyntaxError(this.Lexer.Symbol, "Expected ':' in for statement!") )
                |   _   ->
                        raise ( SyntaxError(this.Lexer.Symbol, "Expected 'in' in for statement!") )
        |   _   ->
                raise ( SyntaxError(this.Lexer.Symbol, "Expecting 'for' statement!") )

    member this.ParseTryStmt() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.Try _ ->
                this.FlowLevel <- this.FlowLevel + 1
                let op1 = this.Lexer.Symbol
                this.Lexer.Advance()
                match this.Lexer.Symbol with
                |   Token.Colon _ ->
                        let op2 = this.Lexer.Symbol
                        this.Lexer.Advance()
                        let left = this.ParseSuite()
                        match this.Lexer.Symbol with
                        |   Token.Finally _ ->
                                let op3 = this.Lexer.Symbol
                                this.Lexer.Advance()
                                match this.Lexer.Symbol with
                                |   Token.Colon _ ->
                                        let start2 = this.Lexer.Position
                                        let op4 = this.Lexer.Symbol
                                        this.Lexer.Advance()
                                        let right3 = this.ParseSuite()
                                        let node = ASTNode.Finally(start2, this.Lexer.Position, op3, op4, right3)
                                        this.FlowLevel <- this.FlowLevel - 1
                                        ASTNode.Try(startPos, this.Lexer.Position, op1, op2, left, [||], ASTNode.Empty, node)
                                |   _ ->
                                        raise ( SyntaxError(this.Lexer.Symbol, "Expected ':' in finally statement!") )
                        |   _ ->
                                match this.Lexer.Symbol with
                                |   Token.Except _ ->
                                        let mutable nodes : ASTNode list = []
                                        while   match this.Lexer.Symbol with
                                                | Token.Except _ ->
                                                        nodes <- this.ParseExceptClause() :: nodes
                                                        true
                                                | _ ->
                                                        false
                                            do ()
                                        let node =  match this.Lexer.Symbol with
                                                    |   Token.Else _ ->
                                                            let start2 = this.Lexer.Position
                                                            let op6 = this.Lexer.Symbol
                                                            this.Lexer.Advance()
                                                            match this.Lexer.Symbol with
                                                            |   Token.Colon _ ->
                                                                    let op7 = this.Lexer.Symbol
                                                                    this.Lexer.Advance()
                                                                    let right5 = this.ParseSuite()
                                                                    ASTNode.Else(start2, this.Lexer.Position, op6, op7, right5)
                                                            |   _ ->
                                                                    raise ( SyntaxError(this.Lexer.Symbol, "Expected ':' in else statement!") )
                                                    |   _ ->
                                                            ASTNode.Empty
                                        let fin =   match this.Lexer.Symbol with
                                                    |   Token.Finally _ ->
                                                            let op5 = this.Lexer.Symbol
                                                            this.Lexer.Advance()
                                                            match this.Lexer.Symbol with
                                                            |   Token.Colon _ ->
                                                                    let start3 = this.Lexer.Position
                                                                    let op6 = this.Lexer.Symbol
                                                                    this.Lexer.Advance()
                                                                    let right4 = this.ParseSuite()
                                                                    ASTNode.Finally(start3, this.Lexer.Position, op5, op6, right4)
                                                            | _ ->
                                                                    raise ( SyntaxError(this.Lexer.Symbol, "Missing ':' in finally statement!") )
                                                    |   _ ->
                                                            ASTNode.Empty
                                        this.FlowLevel <- this.FlowLevel - 1
                                        ASTNode.Try(startPos, this.Lexer.Position, op1, op2, left, List.toArray(List.rev nodes), node, fin)
                                |   _ ->
                                        raise ( SyntaxError(this.Lexer.Symbol, "Missing expect statement!") )
                |   _   ->
                        raise ( SyntaxError(this.Lexer.Symbol, "Missing ':' in try statement!") )
        |    _   ->
                raise ( SyntaxError(this.Lexer.Symbol, "Expected 'try' in try statement!") )

    member this.ParseWithStmt() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.With _ ->
                this.FlowLevel <- this.FlowLevel + 1
                let mutable nodes : ASTNode list = []
                let mutable ops : Token list = []
                let op1 = this.Lexer.Symbol
                this.Lexer.Advance()
                nodes <- this.ParseWithItem() :: nodes
                while   match this.Lexer.Symbol with
                        |   Token.Comma _ ->
                                ops <- this.Lexer.Symbol :: ops
                                this.Lexer.Advance()
                                nodes <- this.ParseWithItem() :: nodes
                                true
                        |   _ ->
                                false
                    do ()
                match this.Lexer.Symbol with
                |   Token.Colon _ ->
                        let op2 = this.Lexer.Symbol
                        this.Lexer.Advance()
                        let op3 =   match this.Lexer.Symbol with
                                    |   Token.TypeComment _ ->
                                            let tmpOp = this.Lexer.Symbol
                                            this.Lexer.Advance()
                                            tmpOp
                                    |   _ ->
                                            Token.Empty
                        let right = this.ParseSuite()
                        this.FlowLevel <- this.FlowLevel - 1
                        ASTNode.With(startPos, this.Lexer.Position, op1, List.toArray(List.rev nodes), List.toArray(List.rev ops), op2, op3, right)
                |   _   ->
                        raise ( SyntaxError(this.Lexer.Symbol, "Expected ':' in with statement!") )
        |   _ ->
                raise ( SyntaxError(this.Lexer.Symbol, "Expecting 'with' in with statement!") )

    member this.ParseWithItem() =
        let startPos = this.Lexer.Position
        let left = this.ParseTest()
        match this.Lexer.Symbol with
        |   Token.As _ ->
                let op1 = this.Lexer.Symbol
                this.Lexer.Advance()
                let right = this.ParseExpr()
                ASTNode.WithItem(startPos, this.Lexer.Position, left, op1, right)
        |   _ ->
                ASTNode.WithItem(startPos, this.Lexer.Position, left, Token.Empty, ASTNode.Empty)

    member this.ParseExceptClause() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.Except _ ->
                let op1 = this.Lexer.Symbol
                this.Lexer.Advance()
                let left, op2, right =  match this.Lexer.Symbol with
                                        |   Token.Colon _ ->    
                                                ASTNode.Empty, Token.Empty, ASTNode.Empty
                                        |   _   ->
                                                let tmp = this.ParseTest()
                                                match this.Lexer.Symbol with
                                                |   Token.As _ ->
                                                        let op3 = this.Lexer.Symbol
                                                        this.Lexer.Advance()
                                                        match this.Lexer.Symbol with
                                                        |   Token.Name _ ->
                                                                let start3 = this.Lexer.Position 
                                                                let name = this.Lexer.Symbol
                                                                this.Lexer.Advance()
                                                                let nodeRight = ASTNode.Name(start3, this.Lexer.Position, name)
                                                                tmp, op3, nodeRight
                                                        |   _ ->
                                                                raise ( SyntaxError(this.Lexer.Symbol, "Missing name literal afer 'as' in except statement!") )
                                                |   _ ->
                                                        tmp, Token.Empty, ASTNode.Empty
                match this.Lexer.Symbol with
                |   Token.Colon _ ->
                        let op4 = this.Lexer.Symbol
                        this.Lexer.Advance()
                        let next = this.ParseSuite()
                        ASTNode.Except(startPos, this.Lexer.Position, op1, left, op2, right, op4, next)
                |   _ ->
                        raise ( SyntaxError(this.Lexer.Symbol, "Expecting ':' in except statement!") )
        |   _ ->
                raise ( SyntaxError(this.Lexer.Symbol, "Expecting 'except' in except statement!") )

    member this.ParseSuite() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Newline _   ->
                let op1 = this.Lexer.Symbol
                this.Lexer.Advance()
                match this.Lexer.Symbol with
                |   Token.Indent _ ->
                        let op2 = this.Lexer.Symbol
                        this.Lexer.Advance()
                        let mutable nodes : ASTNode list = []
                        nodes <- this.ParseStmt() :: nodes
                        while   match this.Lexer.Symbol with
                                |   Token.Dedent _ ->
                                        false
                                |   _ ->
                                        nodes <- this.ParseStmt() :: nodes
                                        true
                            do ()
                        let op3 = this.Lexer.Symbol
                        this.Lexer.Advance()
                        ASTNode.Suite(startPos, this.Lexer.Position, op1, op2, List.toArray(List.rev nodes), op3)
                |   _ ->
                        raise ( SyntaxError(this.Lexer.Symbol, "Expecting indentation in statement block!") )
        |   _   ->
                this.ParseSimpleStmt()

    // Expression rules in Python 3.9 grammar /////////////////////////////////////////////////////

    member this.ParseNamedExpr() =
        let startPos = this.Lexer.Position
        let left = this.ParseTest()
        match this.Lexer.Symbol with
        |   Token.ColonAssign _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                let right = this.ParseTest();
                ASTNode.NamedExpr(startPos, this.Lexer.Position, left, op, right)
        |   _   ->  left

    member this.ParseTest() =
        match this.Lexer.Symbol with
        |   Token.Lambda _ ->   this.ParseLambda()
        |   _   ->
                let startPos = this.Lexer.Position
                let left = this.ParseOrTest()
                match this.Lexer.Symbol with
                |   Token.If _ ->
                        let op1 = this.Lexer.Symbol
                        this.Lexer.Advance()
                        let right = this.ParseOrTest()
                        match this.Lexer.Symbol with
                        |   Token.Else _ ->
                                let op2 = this.Lexer.Symbol
                                this.Lexer.Advance()
                                let next = this.ParseTest()
                                ASTNode.Test(startPos, this.Lexer.Position, left, op1, right , op2, next)
                        |   _   ->
                                raise ( SyntaxError(this.Lexer.Symbol, "Expected 'else' in test expression!") )
                |   _   ->
                        left

    member this.ParseTestNoCond() =
        match this.Lexer.Symbol with | Token.Lambda _ -> this.ParseLambda(false) | _ -> this.ParseOrTest()

    member this.ParseLambda(?isCond : bool) =
        let conditional = match isCond with Some x -> x | _ -> true
        match this.Lexer.Symbol with
        |   Token.Lambda _ ->
                let startPos = this.Lexer.Position
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                let left =  match this.Lexer.Symbol with
                            |   Token.Colon _ ->
                                    ASTNode.Empty
                            |   _   ->
                                    this.ParseVarArgsList()
                let op2 =   match this.Lexer.Symbol with
                            |   Token.Colon _ ->
                                    let tmpOp = this.Lexer.Symbol
                                    this.Lexer.Advance()
                                    tmpOp
                            |   _ ->    raise ( SyntaxError(this.Lexer.Symbol, "Expected ':' in lambda expression!") )
                let right = match conditional with
                            |   true ->
                                    this.ParseTest()
                            |   _   ->
                                    this.ParseTestNoCond()
                ASTNode.Lambda(startPos, this.Lexer.Position, op, left, op2, right)
        |   _   ->
                raise ( SyntaxError(this.Lexer.Symbol, "Expected 'lambda' in lambda expression!") )

    member this.ParseOrTest() =
        let startPos = this.Lexer.Position
        let mutable res = this.ParseAndTest()
        while   match this.Lexer.Symbol with
                |   Token.Or _ ->
                        let op = this.Lexer.Symbol;
                        this.Lexer.Advance()
                        let right = this.ParseAndTest()
                        res <- ASTNode.OrTest(startPos, this.Lexer.Position, res, op, right)
                        true
                |   _   ->  false
            do ()
        res

    member this.ParseAndTest() =
        let startPos = this.Lexer.Position
        let mutable res = this.ParseNotTest()
        while   match this.Lexer.Symbol with
                |   Token.And _ ->
                        let op = this.Lexer.Symbol;
                        this.Lexer.Advance()
                        let right = this.ParseNotTest()
                        res <- ASTNode.AndTest(startPos, this.Lexer.Position, res, op, right)
                        true
                |   _   ->  false
            do ()
        res

    member this.ParseNotTest() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.Not _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                let right = this.ParseNotTest()
                ASTNode.NotTest(startPos, this.Lexer.Position, op, right)
        |   _   ->  this.ParseComparison()

    member this.ParseComparison() =
        let startPos = this.Lexer.Position
        let mutable res = this.ParseExpr()
        while   match lexer.Symbol with
                |   Token.Less _ ->
                        let op = lexer.Symbol
                        this.Lexer.Advance()
                        let right = this.ParseExpr()
                        res <- ASTNode.Less(startPos, this.Lexer.Position, res, op, right)
                        true
                |   Token.LessEqual _   ->
                        let op = lexer.Symbol
                        this.Lexer.Advance()
                        let right = this.ParseExpr()
                        res <- ASTNode.LessEqual(startPos, this.Lexer.Position, res, op, right)
                        true
                |   Token.Equal _ ->
                        let op = lexer.Symbol
                        this.Lexer.Advance()
                        let right = this.ParseExpr()
                        res <- ASTNode.Equal(startPos, this.Lexer.Position, res, op, right)
                        true
                |   Token.GreaterEqual _ ->
                        let op = lexer.Symbol
                        this.Lexer.Advance()
                        let right = this.ParseExpr()
                        res <- ASTNode.GreaterEqual(startPos, this.Lexer.Position, res, op, right)
                        true
                |   Token.Greater _ ->
                        let op = lexer.Symbol
                        this.Lexer.Advance()
                        let right = this.ParseExpr()
                        res <- ASTNode.Greater(startPos, this.Lexer.Position, res, op, right)
                        true
                |   Token.NotEqual _ ->
                        let op = lexer.Symbol
                        this.Lexer.Advance()
                        let right = this.ParseExpr()
                        res <- ASTNode.NotEqual(startPos, this.Lexer.Position, res, op, right)
                        true
                |   Token.In _ ->
                        let op = lexer.Symbol
                        this.Lexer.Advance()
                        let right = this.ParseExpr()
                        res <- ASTNode.In(startPos, this.Lexer.Position, res, op, right)
                        true
                |   Token.Not _ ->
                        let op1 = lexer.Symbol
                        this.Lexer.Advance()
                        match lexer.Symbol with
                        |   Token.In _ ->
                                let op2 = this.Lexer.Symbol
                                this.Lexer.Advance()
                                let right = this.ParseExpr()
                                res <- ASTNode.NotIn(startPos, this.Lexer.Position, res, op1, op2, right)
                        |   _   ->  raise ( SyntaxError(this.Lexer.Symbol, "Missing 'in' in 'not in ' expression!") )
                        true
                |   Token.Is _ ->
                        let op1 = lexer.Symbol
                        this.Lexer.Advance()
                        match lexer.Symbol with
                        |   Token.Not _ ->
                                let op2 = this.Lexer.Symbol
                                this.Lexer.Advance()
                                let right = this.ParseExpr()
                                res <- ASTNode.IsNot(startPos, this.Lexer.Position, res, op1, op2, right)
                        |   _   ->
                                let right = this.ParseExpr()
                                res <- ASTNode.Is(startPos, this.Lexer.Position, res, op1, right)
                        true
                |   _   ->  false
            do ()
        res

    member this.ParseStarExpr() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.Mul _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                let right = this.ParseExpr()
                ASTNode.StarExpr(startPos, this.Lexer.Position, op, right)
        |   _   ->  raise ( SyntaxError(this.Lexer.Symbol, "Expecting '*' in expression!") )

    member this.ParseExpr() =
        let startPos = this.Lexer.Position
        let mutable res = this.ParseXorExpr()
        while   match this.Lexer.Symbol with
                |   Token.BitOr _ ->
                        let op = this.Lexer.Symbol;
                        this.Lexer.Advance()
                        let right = this.ParseXorExpr()
                        res <- ASTNode.OrExpr(startPos, this.Lexer.Position, res, op, right)
                        true
                |   _   ->  false
            do ()
        res

    member this.ParseXorExpr() =
        let startPos = this.Lexer.Position
        let mutable res = this.ParseAndExpr()
        while   match this.Lexer.Symbol with
                |   Token.BitXor _ ->
                        let op = this.Lexer.Symbol;
                        this.Lexer.Advance()
                        let right = this.ParseAndExpr()
                        res <- ASTNode.XorExpr(startPos, this.Lexer.Position, res, op, right)
                        true
                |   _   ->  false
            do ()
        res

    member this.ParseAndExpr() =
        let startPos = this.Lexer.Position
        let mutable res = this.ParseShiftExpr()
        while   match this.Lexer.Symbol with
                |   Token.BitAnd _ ->
                        let op = this.Lexer.Symbol;
                        this.Lexer.Advance()
                        let right = this.ParseShiftExpr()
                        res <- ASTNode.AndExpr(startPos, this.Lexer.Position, res, op, right)
                        true
                |   _   ->  false
            do ()
        res

    member this.ParseShiftExpr() =
        let startPos = this.Lexer.Position
        let mutable res = this.ParseArithExpr()
        while   match this.Lexer.Symbol with
                |   Token.ShiftLeft _ ->
                        let op = this.Lexer.Symbol;
                        this.Lexer.Advance()
                        let right = this.ParseArithExpr()
                        res <- ASTNode.ShiftLeft(startPos, this.Lexer.Position, res, op, right)
                        true
                |   Token.ShiftRight _ ->
                        let op = this.Lexer.Symbol;
                        this.Lexer.Advance()
                        let right = this.ParseArithExpr()
                        res <- ASTNode.ShiftRight(startPos, this.Lexer.Position, res, op, right)
                        true
                |   _   ->  false
            do ()
        res

    member this.ParseArithExpr() =
        let startPos = this.Lexer.Position
        let mutable res = this.ParseTerm()
        while   match this.Lexer.Symbol with
                |   Token.Plus _ ->
                        let op = this.Lexer.Symbol;
                        this.Lexer.Advance()
                        let right = this.ParseTerm()
                        res <- ASTNode.Plus(startPos, this.Lexer.Position, res, op, right)
                        true
                |   Token.Minus _ ->
                        let op = this.Lexer.Symbol;
                        this.Lexer.Advance()
                        let right = this.ParseTerm()
                        res <- ASTNode.Minus(startPos, this.Lexer.Position, res, op, right)
                        true
                |   _   ->  false
            do ()
        res

    member this.ParseTerm() =
        let startPos = this.Lexer.Position
        let mutable res = this.ParseFactor()
        while   match this.Lexer.Symbol with
                |   Token.Mul _ ->
                        let op = this.Lexer.Symbol;
                        this.Lexer.Advance()
                        let right = this.ParseFactor()
                        res <- ASTNode.Mul(startPos, this.Lexer.Position, res, op, right)
                        true
                |   Token.Div _ ->
                        let op = this.Lexer.Symbol;
                        this.Lexer.Advance()
                        let right = this.ParseFactor()
                        res <- ASTNode.Div(startPos, this.Lexer.Position, res, op, right)
                        true
                |   Token.Matrice _ ->
                        let op = this.Lexer.Symbol;
                        this.Lexer.Advance()
                        let right = this.ParseFactor()
                        res <- ASTNode.Matrice(startPos, this.Lexer.Position, res, op, right)
                        true
                |   Token.Modulo _ ->
                        let op = this.Lexer.Symbol;
                        this.Lexer.Advance()
                        let right = this.ParseFactor()
                        res <- ASTNode.Modulo(startPos, this.Lexer.Position, res, op, right)
                        true
                |   Token.FloorDiv _ ->
                        let op = this.Lexer.Symbol;
                        this.Lexer.Advance()
                        let right = this.ParseFactor()
                        res <- ASTNode.FloorDiv(startPos, this.Lexer.Position, res, op, right)
                        true
                |   _   ->  false
            do ()
        res

    member this.ParseFactor() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.Plus _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                let right = this.ParseFactor()
                ASTNode.UnaryPlus(startPos, this.Lexer.Position, op, right)
        |   Token.Minus _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                let right = this.ParseFactor()
                ASTNode.UnaryMinus(startPos, this.Lexer.Position, op, right)
        |   Token.BitInvert _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                let right = this.ParseFactor()
                ASTNode.UnaryInvert(startPos, this.Lexer.Position, op, right)
        |   _   ->  this.ParsePower();

    member this.ParsePower() =
        let startPos = this.Lexer.Position
        let mutable res = this.ParseAtomExpr()
        while   match this.Lexer.Symbol with
                |   Token.Power _ ->
                        let op = this.Lexer.Symbol;
                        this.Lexer.Advance()
                        let right = this.ParseFactor()
                        res <- ASTNode.Power(startPos, this.Lexer.Position, res, op, right)
                        true
                |   _   ->  false
            do ()
        res

    member this.ParseAtomExpr() =
        let startPos = this.Lexer.Position
        let op = match this.Lexer.Symbol with
                 |  Token.Async _ ->
                        let tmpOp = this.Lexer.Symbol
                        this.Lexer.Advance()
                        tmpOp
                 |   _   ->  Token.Empty
        let right = this.ParseAtom()
        let mutable nodes : ASTNode list = []
        while   match this.Lexer.Symbol with
                |   Token.LeftParen _ 
                |   Token.LeftBracket _ 
                |   Token.Dot _ ->
                        nodes <- this.ParseTrailer() :: nodes
                        true
                |   _   ->  false
            do ()
        match op, nodes with
        |   Token.Empty, [] ->
                right
        |   _   ->
                ASTNode.AtomExpr(startPos, this.Lexer.Position, op, right, List.toArray( List.rev nodes ))

    member this.ParseAtom() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.Name _    ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                ASTNode.Name(startPos, this.Lexer.Position, op)
        |   Token.Number _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                ASTNode.Number(startPos, this.Lexer.Position, op)
        |   Token.String _  ->
                let mutable nodes : Token list = []
                while   match this.Lexer.Symbol with
                        |   Token.String _ ->
                                nodes <- this.Lexer.Symbol :: nodes
                                this.Lexer.Advance()
                                true
                        |   _   -> false
                    do ()
                ASTNode.String(startPos, this.Lexer.Position, List.toArray( List.rev nodes ))
        |   Token.Elipsis _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                ASTNode.Elipsis(startPos, this.Lexer.Position, op)
        |   Token.None _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                ASTNode.None(startPos, this.Lexer.Position, op)
        |   Token.True _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                ASTNode.True(startPos, this.Lexer.Position, op)
        |   Token.False _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                ASTNode.False(startPos, this.Lexer.Position, op)
        |   Token.LeftParen _   ->
                let op1 = this.Lexer.Symbol
                this.Lexer.Advance()
                match this.Lexer.Symbol with
                |   Token.RightParen _ ->
                        let op2 = this.Lexer.Symbol
                        this.Lexer.Advance()
                        ASTNode.Call(startPos, this.Lexer.Position, op1, ASTNode.Empty, op2)
                |   _ ->
                        let node =  match this.Lexer.Symbol with
                                    |   Token.Yield _   ->  this.ParseYieldExpr()
                                    |   _   ->  this.ParseTestListComp()
                        match this.Lexer.Symbol with
                        |   Token.RightParen _  ->
                                let op2 = this.Lexer.Symbol
                                this.Lexer.Advance()
                                ASTNode.Call(startPos, this.Lexer.Position, op1, node, op2)
                        |   _   ->
                                raise ( SyntaxError(this.Lexer.Symbol, "Missing ')' in expression!") )
        |   Token.LeftBracket _ ->
                let op1 = this.Lexer.Symbol
                this.Lexer.Advance()
                match this.Lexer.Symbol with
                |   Token.RightBracket _ ->
                        let op2 = this.Lexer.Symbol
                        this.Lexer.Advance()
                        ASTNode.Call(startPos, this.Lexer.Position, op1, ASTNode.Empty, op2)
                |   _ ->
                        let node = this.ParseTestListComp()
                        match this.Lexer.Symbol with
                        |   Token.RightBracket _  ->
                                let op2 = this.Lexer.Symbol
                                this.Lexer.Advance()
                                ASTNode.Index(startPos, this.Lexer.Position, op1, node, op2)
                        |   _   ->
                                raise ( SyntaxError(this.Lexer.Symbol, "Missing ']' in expression!") )
        |   Token.LeftCurly _   ->
                let mutable nodes : ASTNode list = []
                let mutable ops : Token list = []
                let op1 = this.Lexer.Symbol
                this.Lexer.Advance()
                match this.Lexer.Symbol with
                |   Token.RightCurly _ ->
                        let op2 = this.Lexer.Symbol
                        this.Lexer.Advance()
                        ASTNode.Dictionary(startPos, this.Lexer.Position, op1, [| |], [| |], op2)
                |   _   ->
                        let a, b, c = this.ParseDictorSetMaker() // a = nodes, b = commas, c = isSet 
                        match this.Lexer.Symbol with
                        |   Token.RightCurly _ ->
                                let op2 = this.Lexer.Symbol
                                this.Lexer.Advance()
                                match c with
                                |   true    ->
                                        ASTNode.Set(startPos, this.Lexer.Position, op1, List.toArray(List.rev a), List.toArray(List.rev b), op2)
                                |   _ ->
                                        ASTNode.Dictionary(startPos, this.Lexer.Position, op1, List.toArray(List.rev a), List.toArray(List.rev b), op2)
                        |   _   ->
                                raise ( SyntaxError(this.Lexer.Symbol, "Missing '} in dictionary!") )
        |   _   ->
                raise ( SyntaxError(this.Lexer.Symbol, "Illegal literal!") )

    member this.ParseTestListComp() =
        let startPos = this.Lexer.Position
        let mutable nodes : ASTNode list = []
        let mutable ops : Token list = []
        nodes <- ( match this.Lexer.Symbol with | Token.Mul _ -> this.ParseStarExpr() | _ -> this.ParseTest() ) :: nodes
        match this.Lexer.Symbol with
        |   Token.Async _ 
        |   Token.For _ ->
                nodes <- this.ParseCompFor() :: nodes
        |   _   ->
                while   match this.Lexer.Symbol with
                        |   Token.Comma _   ->  
                                ops <- this.Lexer.Symbol :: ops
                                this.Lexer.Advance()
                                match this.Lexer.Symbol with
                                |   Token.RightParen _
                                |   Token.RightBracket _ -> ()
                                |   _   ->
                                        nodes <- ( match this.Lexer.Symbol with | Token.Mul _ -> this.ParseStarExpr() | _ -> this.ParseTest() ) :: nodes
                                true
                        |   _   -> false
                    do ()
        ASTNode.TestList(startPos, this.Lexer.Position, List.toArray(List.rev nodes), List.toArray(List.rev ops))

    member this.ParseTrailer() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.LeftParen _   ->
                let op1 = this.Lexer.Symbol
                this.Lexer.Advance()
                let right = match this.Lexer.Symbol with
                            |   Token.RightParen _ ->
                                    ASTNode.Empty
                            |   _   ->
                                    this.ParseArgsList()
                match this.Lexer.Symbol with
                |   Token.RightParen _  ->
                        let op2 = this.Lexer.Symbol
                        this.Lexer.Advance()
                        ASTNode.Call(startPos, this.Lexer.Position, op1, right, op2)
                |   _   ->  raise ( SyntaxError(this.Lexer.Symbol, "") )
        |   Token.LeftBracket _ ->
                let op1 = this.Lexer.Symbol
                this.Lexer.Advance()
                let right = match this.Lexer.Symbol with
                            |   Token.RightBracket _ ->
                                    ASTNode.Empty
                            |   _   ->
                                    this.ParseSubscriptList()
                match this.Lexer.Symbol with
                |   Token.RightBracket _  ->
                        let op2 = this.Lexer.Symbol
                        this.Lexer.Advance()
                        ASTNode.Index(startPos, this.Lexer.Position, op1, right, op2)
                |   _   ->  raise ( SyntaxError(this.Lexer.Symbol, "") )
        |   Token.Dot _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                match this.Lexer.Symbol with
                |   Token.Name _    ->
                        let op2 = this.Lexer.Symbol
                        this.Lexer.Advance()
                        ASTNode.DotName(startPos, this.Lexer.Position, op, ASTNode.Name(startPos, this.Lexer.Position, op2))
                |   _   ->  raise ( SyntaxError(this.Lexer.Symbol, "Expecting name literal after '.'") )
        |   _   ->  raise ( SyntaxError(this.Lexer.Symbol, "Expected '(', '[' or '.' in trailer expression!") )
    
    member this.ParseSubscriptList() =
        let startPos = this.Lexer.Position
        let mutable nodes : ASTNode list = []
        let mutable ops : Token list = []
        nodes <- this.ParseSubscript() :: nodes
        while   match this.Lexer.Symbol with
                |   Token.Comma _ ->
                        ops <- this.Lexer.Symbol :: ops
                        this.Lexer.Advance()
                        match this.Lexer.Symbol with
                        |   Token.RightBracket _    ->  ()
                        |   _   ->
                                nodes <- this.ParseSubscript() :: nodes
                        true
                |   _   ->  false
            do ()
        ASTNode.SubscriptList(startPos, this.Lexer.Position, List.toArray( List.rev nodes ), List.toArray( List.rev ops ))

    member this.ParseSubscript() =
        let startPos = this.Lexer.Position
        let left =  match this.Lexer.Symbol with
                    |   Token.Colon _ ->    ASTNode.Empty
                    |   Token.Comma _
                    |   Token.RightBracket _ ->   raise ( SyntaxError(this.Lexer.Symbol, "Missing subscript item!") )
                    |   _   ->  this.ParseTest()
        match this.Lexer.Symbol with
        |   Token.Colon _ ->
                let op1 = this.Lexer.Symbol
                this.Lexer.Advance()
                let right = match this.Lexer.Symbol with
                            |   Token.RightParen _
                            |   Token.Comma _
                            |   Token.Colon _   ->  ASTNode.Empty
                            |   _   ->  this.ParseTest()
                match this.Lexer.Symbol with
                |   Token.Colon _ ->
                        let op2 = this.Lexer.Symbol
                        this.Lexer.Advance()
                        match this.Lexer.Symbol with
                        |   Token.RightBracket _
                        |   Token.Comma _ ->
                                ASTNode.Subscript(startPos, this.Lexer.Position, left, op1, right, op2, ASTNode.Empty)
                        |   _   ->
                                let next = this.ParseTest()
                                ASTNode.Subscript(startPos, this.Lexer.Position, left, op1, right, op2, next)
                |   _   ->
                        ASTNode.Subscript(startPos, this.Lexer.Position, left, op1, right, Token.Empty, ASTNode.Empty)
        |   _   ->
            ASTNode.Subscript(startPos, this.Lexer.Position, left, Token.Empty, ASTNode.Empty, Token.Empty, ASTNode.Empty)

    member this.ParseExprList() =
        let startPos = this.Lexer.Position
        let mutable nodes : ASTNode list = []
        let mutable ops : Token list = []
        nodes <- ( match this.Lexer.Symbol with | Token.Mul _ -> this.ParseStarExpr() | _ -> this.ParseTest() ) :: nodes
        while   match this.Lexer.Symbol with
                |   Token.Comma _ ->
                        ops <- this.Lexer.Symbol :: ops
                        this.Lexer.Advance()
                        match this.Lexer.Symbol with
                        |   Token.In _  ->  ()
                        |   _   ->
                                nodes <- ( match this.Lexer.Symbol with | Token.Mul _ -> this.ParseStarExpr() | _ -> this.ParseTest() ) :: nodes
                        true
                |   _   ->  false
            do ()
        ASTNode.ExprList(startPos, this.Lexer.Position, List.toArray(List.rev nodes), List.toArray(List.rev ops))

    member this.ParseTestList() =
        let startPos = this.Lexer.Position
        let mutable nodes : ASTNode list = []
        let mutable ops : Token list = []
        nodes <- this.ParseTest() :: nodes
        while   match this.Lexer.Symbol with
                |   Token.Comma _   ->
                        ops <- this.Lexer.Symbol :: ops
                        match this.Lexer.Symbol with
                        |   Token.SemiColon _
                        |   Token.Newline _     ->  false
                        |   _   ->
                                nodes <- this.ParseTest() :: nodes
                                true
                |   _   ->  false
            do ()
        ASTNode.TestList(startPos, this.Lexer.Position, List.toArray(List.rev nodes), List.toArray(List.rev ops))

    member this.ParseDictorSetMaker() : ASTNode list * Token list * bool =
        let startPos = this.Lexer.Position
        let mutable nodes : ASTNode list = []
        let mutable ops : Token list = []
        let mutable isSet = true
        match this.Lexer.Symbol with
        |   Token.Mul _ ->
                let right = this.ParseStarExpr()
                nodes <- ASTNode.SetEntry(startPos, this.Lexer.Position, right) :: nodes
        |   Token.Power _   ->
                let op1 = this.Lexer.Symbol
                this.Lexer.Advance()
                let right = this.ParseExpr()
                isSet <- false
                nodes <- ASTNode.DictionaryEntry(startPos, this.Lexer.Position, ASTNode.Empty, op1, right) :: nodes
        |   _   ->
                let left = this.ParseTest()
                match this.Lexer.Symbol with
                |   Token.Colon _   ->
                        let op1 = this.Lexer.Symbol
                        this.Lexer.Advance()
                        let right = this.ParseTest()
                        isSet <- false
                        nodes <- ASTNode.DictionaryEntry(startPos, this.Lexer.Position, left, op1, right) :: nodes
                |   _   ->
                        nodes <- ASTNode.SetEntry(startPos, this.Lexer.Position, left) :: nodes
        match this.Lexer.Symbol with
        |   Token.Async _
        |   Token.For _ ->
                let right = this.ParseCompIter()
                match isSet with
                |   true    ->
                        nodes <- ASTNode.SetEntry(startPos, this.Lexer.Position, right) :: nodes
                |   _   ->
                        nodes <- ASTNode.DictionaryEntry(startPos, this.Lexer.Position, ASTNode.Empty, Token.Empty, right) :: nodes
        |   _   ->  
                while   match this.Lexer.Symbol with
                        |   Token.Comma _   ->
                                ops <- this.Lexer.Symbol :: ops
                                this.Lexer.Advance()
                                match this.Lexer.Symbol with
                                |   Token.RightCurly _ ->
                                        false
                                |   _   ->
                                        match isSet with
                                        |   true    ->
                                                match this.Lexer.Symbol with
                                                |   Token.Mul _ ->
                                                        let right = this.ParseStarExpr()
                                                        nodes <- ASTNode.SetEntry(startPos, this.Lexer.Position, right) :: nodes
                                                        true
                                                |   _   ->  
                                                        let right = this.ParseTest()
                                                        nodes <- ASTNode.SetEntry(startPos, this.Lexer.Position, right) :: nodes
                                                        true
                                        |   _   ->
                                                match this.Lexer.Symbol with
                                                |   Token.Power _   ->
                                                        let op1 = this.Lexer.Symbol
                                                        this.Lexer.Advance()
                                                        let right = this.ParseExpr()
                                                        nodes <- ASTNode.DictionaryEntry(startPos, this.Lexer.Position, ASTNode.Empty, op1, right) :: nodes
                                                        true
                                                |   _   ->
                                                        let left = this.ParseTest()
                                                        match this.Lexer.Symbol with
                                                        |   Token.Colon _ ->
                                                                let op1 = this.Lexer.Symbol
                                                                this.Lexer.Advance()
                                                                let right = this.ParseTest()
                                                                nodes <- ASTNode.DictionaryEntry(startPos, this.Lexer.Position, left, op1, right) :: nodes
                                                                true
                                                        |   _   ->
                                                                raise ( SyntaxError(this.Lexer.Symbol, "Missing ':' in dictionary entry!") )
                        |   _   ->
                                false
                    do ()
        ( nodes, ops, isSet )

    member this.ParseArgsList() =
        let startPos = this.Lexer.Position
        let mutable nodes : ASTNode list = []
        let mutable ops : Token list = []
        nodes <- this.ParseArgument() :: nodes
        while   match this.Lexer.Symbol with
                |   Token.Comma _   ->
                        ops <- this.Lexer.Symbol :: ops
                        match this.Lexer.Symbol with
                        |   Token.RightParen _ ->   false
                        |   _   ->
                                nodes <- this.ParseArgument() :: nodes
                                true
                |   _   ->  false
            do ()
        ASTNode.ArgumentList(startPos, this.Lexer.Position, List.toArray(List.rev nodes), List.toArray(List.rev ops))

    member this.ParseArgument() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.Mul _ 
        |   Token.Power _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                match this.Lexer.Symbol with
                |   Token.Name _    ->
                        let name = this.Lexer.Symbol
                        this.Lexer.Advance()
                        ASTNode.Argument(startPos, this.Lexer.Position, ASTNode.Empty, op, ASTNode.Name(startPos, this.Lexer.Position, name))
                |   _   ->  raise ( SyntaxError(this.Lexer.Symbol, "Missing argument!") )     
        |   Token.Name _    ->
                let name = this.Lexer.Symbol
                this.Lexer.Advance()
                let left = ASTNode.Name(startPos, this.Lexer.Position, name)
                match this.Lexer.Symbol with
                |   Token.Async _
                |   Token.For _ ->
                        let right = this.ParseCompFor()
                        ASTNode.Argument(startPos, this.Lexer.Position, left, Token.Empty, right)
                |   Token.ColonAssign _
                |   Token.Assign _   ->
                        let op = this.Lexer.Symbol
                        this.Lexer.Advance()
                        let right = this.ParseTest()
                        ASTNode.Argument(startPos, this.Lexer.Position, left, op, right)
                |   _   ->
                        ASTNode.Argument(startPos, this.Lexer.Position, left, Token.Empty, ASTNode.Empty)
        |   _   ->  raise ( SyntaxError(this.Lexer.Symbol, "Missing argument!") ) 
                
    member this.ParseCompIter() =
        match this.Lexer.Symbol with
        |   Token.Async _
        |   Token.For _     ->
                this.ParseCompFor()
        |   _   ->
                this.ParseCompIf()

    member this.ParseCompFor() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.Async _ ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                let right = this.ParseSyncCompFor()
                ASTNode.CompFor(startPos, this.Lexer.Position, op, right)
        |   _   ->
                this.ParseSyncCompFor()

    member this.ParseSyncCompFor() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.For _ ->
                let op1 = this.Lexer.Symbol
                this.Lexer.Advance()
                let left = this.ParseExprList()
                match this.Lexer.Symbol with
                |   Token.In _  ->
                        let op2 = this.Lexer.Symbol
                        this.Lexer.Advance()
                        let right = this.ParseOrTest()
                        match this.Lexer.Symbol with
                        |   Token.Async _
                        |   Token.For _
                        |   Token.If _  ->
                                let next = this.ParseCompIter()
                                ASTNode.SyncCompFor(startPos, this.Lexer.Position, op1, left, op2, right, next)
                        |   _   ->
                                ASTNode.SyncCompFor(startPos, this.Lexer.Position, op1, left, op2, right, ASTNode.Empty)
                |   _   ->
                        raise ( SyntaxError(this.Lexer.Symbol, "Missing 'in' in for comprehension expression!") )
        |   _   ->
                raise ( SyntaxError(this.Lexer.Symbol, "Missing 'for' in comprehension expression!") )
       
    member this.ParseCompIf() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.If _  ->
                let op = this.Lexer.Symbol
                this.Lexer.Advance()
                let right = this.ParseTestNoCond()
                match this.Lexer.Symbol with
                |   Token.Async _
                |   Token.For _
                |   Token.If _  ->
                        let next = this.ParseCompIter()
                        ASTNode.CompIf(startPos, this.Lexer.Position, op, right, next)
                |   _   ->
                        ASTNode.CompIf(startPos, this.Lexer.Position, op, right, ASTNode.Empty)
        |   _   ->
                raise ( SyntaxError(this.Lexer.Symbol, "Missing 'if' in comprehension expression!") )

    member this.ParseYieldExpr() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.Yield _   ->
                let op1 = this.Lexer.Symbol
                this.Lexer.Advance()
                match this.Lexer.Symbol with
                |   Token.From _ ->
                        let op2 = this.Lexer.Symbol
                        this.Lexer.Advance()
                        let right = this.ParseTest()
                        ASTNode.YieldFromExpr(startPos, this.Lexer.Position, op1, op2, right)
                |   _   ->
                        let right = this.ParseTestListComp()
                        ASTNode.YieldExpr(startPos, this.Lexer.Position, op1, right)
        |   _   ->
                raise ( SyntaxError(this.Lexer.Symbol, "Missing 'yield' in yield expression!") )

// Func rules in Python 3.9 grammar ///////////////////////////////////////////////////////////////

    member this.ParseFuncBodySuite() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.Newline _ ->
                let mutable nodes : ASTNode list = [] 
                let op1 = this.Lexer.Symbol
                this.Lexer.Advance()
                match this.Lexer.Symbol with
                |   Token.Indent _ ->
                        let op2 = this.Lexer.Symbol
                        this.Lexer.Advance()
                        let tc, nl =    match this.Lexer.Symbol with
                                        |   Token.TypeComment _ ->
                                                let tmp1 = this.Lexer.Symbol
                                                this.Lexer.Advance()
                                                match this.Lexer.Symbol with
                                                |   Token.Newline _ ->
                                                        let tmp2 = this.Lexer.Symbol
                                                        this.Lexer.Advance()
                                                        tmp1, tmp2
                                                |   _ ->
                                                        raise ( SyntaxError(this.Lexer.Symbol, "Expecting newline after type comment!") )
                                        |   _ ->
                                            Token.Empty, Token.Empty
                        nodes <- this.ParseStmt() :: nodes
                        while   match this.Lexer.Symbol with
                                |   Token.Dedent _ ->
                                        false
                                |   _ ->
                                        nodes <- this.ParseStmt() :: nodes
                                        true
                            do ()
                        let op3 = this.Lexer.Symbol
                        this.Lexer.Advance()
                        ASTNode.FuncBodySuite(startPos, this.Lexer.Position, op1, op2, tc, nl, List.toArray(List.rev nodes), op3)
                |   _ ->
                        raise ( SyntaxError(this.Lexer.Symbol, "Expecting indentation in function block statement!") )
        |   _ ->
                this.ParseSimpleStmt()

    member this.ParseFuncType() =
        let startPos = this.Lexer.Position
        match this.Lexer.Symbol with
        |   Token.LeftParen _ ->
                let op1 = this.Lexer.Symbol
                this.Lexer.Advance()
                let left =  match this.Lexer.Symbol with
                            |   Token.RightParen _ ->
                                    ASTNode.Empty
                            |   _ ->
                                    this.ParseTypeList()
                let op2 =   match this.Lexer.Symbol with
                            |   Token.RightParen _ ->
                                    let tmp1 = this.Lexer.Symbol
                                    this.Lexer.Advance()
                                    tmp1
                            |   _ ->
                                    raise ( SyntaxError(this.Lexer.Symbol, "Expecting ')' in func definition!") )
                let op3 =   match this.Lexer.Symbol with
                            |   Token.Ptr _ ->
                                    let tmp2 = this.Lexer.Symbol
                                    this.Lexer.Advance()
                                    tmp2
                            |   _ ->
                                    raise ( SyntaxError(this.Lexer.Symbol, "Expecting '->' in func definition!") )
                let right = this.ParseTest()
                ASTNode.FuncType(startPos, this.Lexer.Position, op1, left, op2, op3, right)
        |   _ ->
            raise ( SyntaxError(this.Lexer.Symbol, "Expected '(' in func definition!") )

    member this.ParseTypeList() =
        let startPos = this.Lexer.Position
        let mutable nodes : ASTNode list = []
        let mutable ops : Token list = []
        match this.Lexer.Symbol with
        |   Token.Mul _ ->
                let start2 = this.Lexer.Position
                let opMul = this.Lexer.Symbol
                this.Lexer.Advance()
                let node = this.ParseTest()
                nodes <- ASTNode.TypedMul(start2, this.Lexer.Position, opMul, node) :: nodes
                while   match this.Lexer.Symbol with
                        |   Token.Comma _ ->
                                ops <- this.Lexer.Symbol :: ops
                                this.Lexer.Advance()
                                match this.Lexer.Symbol with
                                |   Token.RightParen _ ->
                                        false
                                |   Token.Power _ ->
                                        let start2 = this.Lexer.Position
                                        let opPower = this.Lexer.Symbol
                                        this.Lexer.Advance()
                                        let node = this.ParseTest()
                                        nodes <- ASTNode.TypedPower(start2, this.Lexer.Position, opPower, node) :: nodes
                                        false
                                |   _ ->
                                        nodes <- this.ParseTest() :: nodes
                                        true
                        |   _ ->
                                false
                    do ()
        |   Token.Power _ ->
                let start2 = this.Lexer.Position
                let opPower = this.Lexer.Symbol
                this.Lexer.Advance()
                let node = this.ParseTest()
                nodes <- ASTNode.TypedPower(start2, this.Lexer.Position, opPower, node) :: nodes
        |   _ ->
                nodes <- this.ParseTest() :: nodes
                while   match this.Lexer.Symbol with
                        |   Token.Comma _ ->
                                ops <- this.Lexer.Symbol :: ops
                                this.Lexer.Advance()
                                match this.Lexer.Symbol with
                                |   Token.RightParen _ ->
                                        false
                                |   Token.Power _ ->
                                        let start2 = this.Lexer.Position
                                        let opPower = this.Lexer.Symbol
                                        this.Lexer.Advance()
                                        let node = this.ParseTest()
                                        nodes <- ASTNode.TypedPower(start2, this.Lexer.Position, opPower, node) :: nodes
                                        false
                                |   Token.Mul _ ->
                                        let start3 = this.Lexer.Position
                                        let opMul = this.Lexer.Symbol
                                        this.Lexer.Advance()
                                        let node = this.ParseTest()
                                        nodes <- ASTNode.TypedMul(start3, this.Lexer.Position, opMul, node) :: nodes
                                        while   match this.Lexer.Symbol with
                                                |   Token.Comma _ ->
                                                        ops <- this.Lexer.Symbol :: ops
                                                        this.Lexer.Advance()
                                                        match this.Lexer.Symbol with
                                                        |   Token.RightParen _ ->
                                                                false
                                                        |   Token.Power _ ->
                                                                let start2 = this.Lexer.Position
                                                                let opPower = this.Lexer.Symbol
                                                                this.Lexer.Advance()
                                                                let node = this.ParseTest()
                                                                nodes <- ASTNode.TypedPower(start2, this.Lexer.Position, opPower, node) :: nodes
                                                                false
                                                        |   _ ->
                                                                nodes <- this.ParseTest() :: nodes
                                                                true
                                                |   _ ->
                                                        false
                                            do ()
                                        false
                                |   _ ->
                                        nodes <- this.ParseTest() :: nodes
                                        true
                        |   _ ->
                                false
                    do ()
        ASTNode.TypeList(startPos, this.Lexer.Position, List.toArray(List.rev nodes), List.toArray(List.rev ops))
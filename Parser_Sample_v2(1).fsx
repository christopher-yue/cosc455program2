(*
COURSE: COSC455003
Assignment: Program 2

Name: Yue, Christopher
*)

(* Parser Recursive Descent Parser in F# *)


////////////////////////////////////////////////////////////////////////////////////////////////////
// The following is but one of many possible structures. In fact F#/Ocaml has many features that
// make parsing complex grammars pretty easy... but... to understand those requires a much deeper
// understanding of the language than we have/will explored/explore.  Unfortunately, the result is
// that the code will not be nearly as concise or elegant as it could otherwise be. However, if you
// which to explore the additional features of the language, feel free to explore!!!
//
// NOTE: A more concise approach could involve "Active Patterns", but those are a little more
// difficult to understand, especially while still trying to grasp "static patterns".
// https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/active-patterns
// https://fsharpforfunandprofit.com/posts/convenience-active-patterns/
//////////////////////////////////////////////////////////////////////////////////////////////////////

(* 
    Program grammar:

    <program>    ::= <stmt_list> $$
    <stmt_list>  ::= <stmt> <stmt_list> | ε
    <stmt>       ::= ID <id_tail> | <read_stmt> | <write_stmt> | <if_stmt> | <do_stmt> | <while_stmt>
    <id_tail>    ::= <fun_call> | <assignment>
    <expr>       ::= ID <expr_tail> | OPEN_PAREN <expr> CLOSE_PAREN
    <expr_tail>  ::= ARITH_OP <expr> | ε
    <cond>       ::= <expr> REL_OPER <expr>
    <assignment> ::= EQUALS <expr>
    <read_stmt>  ::= READ ID
    <write_stmt> ::= WRITE <expr>
    <if_stmt>    ::= IF <cond> THEN <stmt_list> <else_stmt>
    <else_stmt>  ::= ELSE <stmt_list> ENDIF | ENDIF
    <fun_call>   ::= ARROW ID OPEN_PAREN <param_list> CLOSE_PAREN
    <param_list> ::= <expr> <param_tail>
    <param_tail> ::= COMMA <expr> <param_tail> | ε
    <while_stmt> ::= WHILE <cond> DO <stmt_list> DONE
    <do_stmt>    ::= DO <stmt_list> UNTIL <cond>
    
    <ID>          ::= any lexeme/token not already expressed as a terminal
    <OPEN_PAREN>  ::= (
    <CLOSE_PAREN> ::= )
    <ARITH_OP>    ::= - | + | * | /
    <REL_OPER>    ::= > | < | ==
    <EQUALS>      ::= =
    <READ>        ::= read
    <WRITE>       ::= write
    <IF>          ::= if
    <THEN>        ::= then
    <ELSE>        ::= else
    <ENDIF>       ::= endif
    <ARROW>       ::= <-
    <COMMA>       ::= ,
    <WHILE>       ::= while
    <DO>          ::= do
    <DONE>        ::= done
    <UNTIL>       ::= until
*)

// Token Type
type Token =
    | OPEN_PAREN   // (
    | CLOSE_PAREN  // )
    | ARITH_OP     // - | + | * | /
    | REL_OPER     // > | < | ==
    | EQUALS       // =
    | READ         // read
    | WRITE        // write
    | IF           // if
    | THEN         // then
    | ELSE         // else
    | ENDIF        // endif
    | ARROW        // <-
    | COMMA        // ,
    | WHILE        // while
    | DO           // do
    | DONE         // done
    | UNTIL        // until 
    | EOF          // End of file ($$)
    | ID of string // id
    
    // Member (of the type) function to get a token from a lexeme (String)
    static member tokenFromLexeme str =
        match str with
            | "("                   -> OPEN_PAREN
            | ")"                   -> CLOSE_PAREN
            | "-" | "+" | "*" | "/" -> ARITH_OP
            | ">" | "<" | "=="      -> REL_OPER
            | "="                   -> EQUALS
            | "read"                -> READ
            | "write"               -> WRITE
            | "if"                  -> IF
            | "then"                -> THEN
            | "else"                -> ELSE
            | "endif"               -> ENDIF
            | "<-"                  -> ARROW
            | ","                   -> COMMA
            | "while"               -> WHILE
            | "do"                  -> DO
            | "done"                -> DONE
            | "until"               -> UNTIL
            | "$$"                  -> EOF
            | x             -> ID str


let matchToken (theExpectedToken: Token) theList =
    match theList with
    // resolve to the rest of the list when head is the expected type.
    | head :: tail when head = theExpectedToken -> tail

    // head of list did not match the expected type, so we don't even care about "the rest" (_)
    | head :: _ -> failwithf $"Wrong Type! Expected %A{theExpectedToken} but found %A{head}"

    // Couldn't match anything!
    | _ -> failwithf $"Nothing to match! Expected a list with a head of type %A{theExpectedToken}"


// NOTE: The |> operator sends (pipes) the output of one function directly to the next one in line.
// "and" just allows multiple, mutually recursive functions to be defined under a single "let"
let rec parse theList = theList |> program


// TODO <program> ::= <stmt_list> $$
and program lst = lst |> stmt_list |> matchToken EOF


// TODO <stmt_list> ::= <stmt> <stmt_list> | ε
// ! FIGURE OUT ε
and stmt_list = 
    function
    | xs -> stmt |> stmt_list
    | xs -> xs // ε


// TODO <stmt> ::= ID <id_tail> | <read_stmt> | <write_stmt> | <if_stmt> | <do_stmt> | <while_stmt>
// ! FIGURE OUT ID AND EVERYTHING ELSE
and stmt lst = lst |> id_tail

// TODO <id_tail> ::= <fun_call> | <assignment>
// ! FIGURE OUT IF THIS IS CORRECT
and id_tail =
    function
    | xs -> xs |> fun_call
    | xs -> xs |> assignment


// TODO <expr> ::= ID <expr_tail> | OPEN_PAREN <expr> CLOSE_PAREN
// ! FIGURE OUT ID
and expr = 
    function 
    | OPEN_PAREN :: xs -> xs |> expr |> matchToken CLOSE_PAREN
    | xs -> xs |> expr_tail


// <expr_tail> ::= ARITH_OP <expr> | ε
and expr_tail = 
    function
    | ARITH_OP :: xs -> xs |> expr
    | xs -> xs // ε


// <cond> ::= <expr> REL_OPER <expr>
and cond lst = lst |> expr |> matchToken REL_OPER |> expr


// <assignment> ::= EQUALS <expr>
and assignment lst = lst |> matchToken EQUALS |> expr


// TODO <read_stmt>  ::= READ ID
// ! FIGURE OUT ID
and read_stmt lst = lst |> matchToken READ


// <write_stmt> ::= WRITE <expr>
and write_stmt lst = lst |> matchToken WRITE |> expr


// TODO <if_stmt>    ::= IF <cond> THEN <stmt_list> <else_stmt>
and if_stmt lst = lst |> matchToken IF |> cond |> matchToken THEN |> stmt_list |> else_stmt


// TODO <else_stmt>  ::= ELSE <stmt_list> ENDIF | ENDIF
// ! FIGURE OUT IF THIS IS CORRECT
and else_stmt =
    function
    | ELSE :: xs -> xs |> stmt_list |> matchToken ENDIF
    | xs -> xs |> matchToken ENDIF


// TODO <fun_call>   ::= ARROW ID OPEN_PAREN <param_list> CLOSE_PAREN
// ! FIGURE OUT ID
and fun_call lst = lst |> matchToken ARROW |> matchToken OPEN_PAREN |> param_list |> matchToken CLOSE_PAREN


// <param_list> ::= <expr> <param_tail>
and param_list lst = lst |> expr |> param_tail


// <param_tail> ::= COMMA <expr> <param_tail> | ε
and param_tail =
    function
    | COMMA :: xs -> xs |> expr |> param_tail
    | xs -> xs


// <while_stmt> ::= WHILE <cond> DO <stmt_list> DONE
and while_stmt lst = lst |> matchToken WHILE |> cond |> matchToken DO |> stmt_list |> matchToken DONE


// <do_stmt> ::= DO <stmt_list> UNTIL <cond>
and do_stmt lst = lst |> matchToken DO |> stmt_list |> matchToken UNTIL |> cond




(* **********************************************************************************************
   YOU MAY LEAVE THE FOLLOWING CODE AS IS.  IT IS NOT NECESSARY TO MODIFY IT FOR THIS ASSIGNMENT.
   *********************************************************************************************** *)

(* Get the user input and start parsing *)
open System.Text.RegularExpressions

// NOTE: To make the let assignment be a function that accepts no parameters,
// an "empty tuple" must be accepted in ML/SML/OCaml/F#.
let main () =

    // Convert a list of stings to Tokens:
    //    Split the String (which creates an Array)
    //             -> convert the Array to a List
    //             -> MAP the list of strings into a list of Tokens.
    //
    // (Note, though arrays are a lot like lists, lists are a bit easier to use for the pattern matching.)
    
    // 'mapTokens' is mainly it's own function as an example of the ">>" operator.
    // This just means that the mapTokens function is a combination of the convert
    // to list function and the Map to list function. (No parameters are specified!)
    let mapTokens = Array.toList >> List.map Token.tokenFromLexeme  

    // This is very ".NET" specific. Split is part of the .NET API.        
    let getTokenList (str: string) = Regex.Split(str.Trim(), "\\s+") |> mapTokens

    (* Begin Parsing Process *)
    let startParsing str =
        // Display our list of tokens...
        printfn $"\nInitial String: %s{str}"

        // Try to parse the list of tokens and display the results.
        try
            let tokenList = getTokenList str
            printfn $"Tokens Before Parsing: %A{tokenList}"
            let parsedList = parse tokenList

            if (parsedList.Length > 0) then
                printfn $"Parsing Failed because we have extra tokens! %A{parsedList}"
                printfn $"Extra Tokens:\t%A{parsedList}"
            else
                printfn "Done!"

        // If an exception ("failwith") is thrown, display the error message.
        with Failure msg ->
            printfn $"Error: %s{msg}"

    // Get the user input and start parsing
    let getUserInput () =
        printf "Enter (Space Delimited) String\n=> "
        
        // A case where it's easier to use the .NET ReadLine as opposed to the more restrictive OCaml native variant.        
        System.Console.ReadLine()

    in
    // Get the user input and start parsing
    getUserInput () |>  startParsing |> ignore  // Just ignore the result, as we are just printing results above.



(* EXAMPLE TEST DATA:  the small , slow dog quietly chases the fast cat up a tall tree .     *)

// Execute the main function!
main ()
open Scanner

type
  thing =
    Closure of thing * thing * environment |
    Cons of thing * thing |
    Nil |
    Number of int |
    Primitive of (thing -> environment -> thing) |
    Symbol of string
and
  environment = (string * thing) list ;;

module type Parserish =
sig
  exception Can'tParse of string
  val initialize : string -> unit
  val nextThing : unit -> thing
end

module Parser : Parserish = struct
  exception Can'tParse of string
  let token = ref EndToken

  let initialize path =
    Scanner.initialize path;
    token := Scanner.nextToken ()

  let nextToken () =
    token := Scanner.nextToken ()

  let rec nextThing () =
    match !token with
    | CloseParenToken ->
        raise (Can'tParse "Wrong/Dumb placement of ')'")
    | EndToken ->
        raise (Can'tParse "open list not closed, missing ')' at the end")
    | NumberToken n ->
        nextToken ();
        Number n
    | OpenParenToken ->
        nextToken ();
        nextThings ()
    | SymbolToken "nil" ->
        nextToken ();
        Nil
    | SymbolToken s ->
        nextToken ();
        Symbol s

  and nextThings () =
    match !token with
    | CloseParenToken ->
        nextToken ();
        Nil
    | EndToken ->
        raise (Can'tParse "open list not closed, missing ')' at the end")
    | _ ->
        let car = nextThing () in
        let cdr = nextThings () in
        Cons (car, cdr)
end

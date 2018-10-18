(* You may need the line below or not depending on how you are testing
things.  Keep it or remove it as you want.  We will insert what we need for
our testing purposes.  We do not need any other header information like
your name or student id as myCourses tracks this for us. *)

(* This is the type definition for the expressions that we will produce as a result
of parsing the input strings. *)

type exptree = Var of char | Expr of char * exptree * exptree

let charSet = ['a' .. 'z']

let example = "(a+(b+(c*d)+e)*f)*g"

let isin (x: char) L = List.exists (fun y -> x = y) L

let parse (inputexp: string): exptree =
  let sym = ref inputexp.[0]
  let cursor = ref 0
  let getsym () =
    cursor := !cursor + 1
    sym := inputexp.[!cursor]
  let rec expr (): exptree =
    let result = term()
    if not (!cursor = inputexp.Length - 1) then
      if (!sym = '+') then
        getsym()
        Expr ('+', result, expr())
      elif (!sym = ')') then
        result
      else
        failwith "Error: invalid syntax"
    else result
  and term (): exptree =
    let result = primary()
    if (not (!cursor = inputexp.Length - 1 || !sym = '+')) then
      if (!sym = '*') then
        getsym()
        Expr ('*', result, term())
      elif (!sym = ')') then
        result
      else
        failwith "Error: invalid syntax"
    else result

  and primary (): exptree =  //I did this for you.
    if (!sym = '(') then 
      getsym()
      let result = expr ()
      if not (!sym = ')') then failwith "Error: mismatched brackets"
      else 
        if (!cursor = inputexp.Length - 1) then result
        else 
          getsym()
          result
    elif (isin !sym charSet) then 
      if (!cursor = inputexp.Length - 1) then (Var !sym) 
      else let result = Var !sym in (getsym(); result)
    else failwith "Error: invalid syntax"
  expr() //This is part of the top-level function parse.


(* Now for Question 2 *)

(*  Do not change this.  tempstore will be a global variable.  Remember to reset this between
tests of the program. *)
(* This is done as to the pseudo-code written in class. tempstore keeps
a memory variable which resets after being used. *)
let mutable tempstore = 0

let codegen (e: exptree) = 
  let rec helper (e: exptree, tag: char) = 
    if (tag = '=') then
      match e with
      |Var v -> printf "LOAD  %c \n" v
      |Expr (c, l, r) ->
        helper (l, '=')
        helper (r, c)
    elif (tag = '+' || tag = '*') then
      match e with
      |Var v ->
        if (tag = '+') then printf "ADD  %c \n" v
        elif (tag = '*') then printf "MUL  %c \n" v
      |Expr (c, l, r) ->
        tempstore <- tempstore + 1
        printf "STORE  %d \n" tempstore
        helper (l, '=')
        helper (r, c)
        if (tag = '+') then printf "ADD  %d \n" tempstore
        elif (tag = '*') then printf "MUL  %d \n" tempstore
        else failwith "Invalid tag"
        tempstore <- tempstore - 1
    else
      failwith "Invalid tag"

  helper(e,'=') //This is part of the top-level function codegen.  Do not change it.
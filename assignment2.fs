module Hw2

(* Assignment 2 *) (* Do not edit this line. *)
(* Student name: Marie Payne, Id Number: 260686859 *) (* Edit this line. *)

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like, except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  It is OK to change a "rec" declaration and put the
recursive function inside a helper if you want to.  Your code MUST compile
and must NOT go into infinite loops.  An assignment like that means you
have not tested it.  You will get ZERO FOR THE ENTIRE ASSIGMENT even if the
problem is only with one question.  If you are not able to get the code to
compile and run do not submit it.  *)

(* Question 1 *) 

let deriv (f, dx: float) = fun x -> ((f(x + dx) - f(x))/dx)

let rec newton(f,guess:float,tol:float,dx:float) = 
  let nxt xn = xn - ((f(xn)) / (deriv(f, dx) xn))
  let abs x:float = if (x < 0.0) then -x else x
  if (abs (f guess)) < tol then guess else newton (f, nxt guess, tol, dx)

 
(* let make_cubic(a:float,b,c) = fun x -> (x*x*x + a * x*x + b*x + c) *)
(* newton(make_cubic(2.0,-3.0,1.0),0.0,0.0001,0.0001) *)

(* let root = newton(sin,5.0,0.0001,0.0001) *)

(* Question 2 *)

type term = Term of float * int
type poly = Poly of (float * int) list

exception EmptyList

let multiplyPolyByTerm(Term (c,e):term, Poly p:poly):poly = 
  try
    if (p.IsEmpty) then 
      raise EmptyList 
    else
      let rec helper(Term (c,e):term, l) = 
        match l with
        | [] -> []
        | x::xs -> 
          match x with 
          | (a:float,b:int) -> (a*c,b+e)::helper((Term (c,e):term), xs)
      let result = helper ((Term (c,e):term), p)
      Poly result:poly
  with
  | EmptyList ->  (printfn "This is an empty polynomial!")
                  ( Poly p:poly )

(* Testing code 
let term1:term = Term (1.0,2)
let poly1:poly = Poly [(4.0,2);(3.0,1);(1.0,0)]
let poly2:poly = Poly [] *)

let addTermToPoly(Term (c,e):term, Poly p:poly):poly = 
  try
    if (p.IsEmpty) then 
      raise EmptyList 
    else
      let rec helper((Term (c,e):term), Poly p:poly) = 
        match p with
        | [] -> []
        | x::xs -> 
          match x with 
          | (a:float,b:int) -> if (b=e) then (a+c,b)::xs elif (b<e) then (c,e)::p else x:: helper((Term (c,e):term), (Poly xs:poly))
      let result = helper ((Term (c,e):term), (Poly p:poly))
      Poly result:poly
  with
  | EmptyList ->  (printfn "This is an empty polynomial!")
                  ( Poly p:poly ) 
                
let addPolys(Poly p1:poly, Poly p2:poly):poly = 
  try
    if (p1.IsEmpty || p2.IsEmpty) then 
      raise EmptyList 
    else
      let rec helper(Poly l1:poly, Poly l2:poly) = 
        match l1 with
        | [] -> l2
        | x::xs -> helper ((Poly xs:poly), addTermToPoly((Term x:term), (Poly l2:poly)))
      let result = helper ((Poly p1:poly), (Poly p2:poly))
      Poly result:poly
  with
  | EmptyList ->  (printfn "This is an empty polynomial!")
                  if (p1.IsEmpty) then 
                    ( Poly p1:poly ) 
                  else 
                    ( Poly p2:poly )

(* I couldn't figure out how to deconstruct a poly type (at the time), so I rewrote the functions. Once I figured out how later in the assignment I didn't feel like rewriting it :D *)
let multPolys(Poly p1:poly, Poly p2:poly) = 
  try
    if (p1.IsEmpty || p2.IsEmpty) then 
      raise EmptyList 
    else
      let rec helper(Poly l1:poly, Poly l2:poly) = 
        match l1 with
        | [] -> [0.0,0]
        | x::xs -> 
          let rec help(Term (c,e):term, l) = 
            match l with
            | [] -> []
            | x::xs -> 
              match x with 
              | (a:float,b:int) -> (a*c,b+e)::help((Term (c,e):term), xs)
          let rec help2(Poly l1:poly, Poly l2:poly) = 
            match l1 with
            | [] -> l2
            | x::xs -> help2 ((Poly xs:poly), addTermToPoly((Term x:term), (Poly l2:poly)))
          let prod = help((Term x:term), l2)
          let recurs = helper((Poly xs:poly), (Poly l2:poly))
          help2((Poly prod:poly), (Poly recurs:poly))
      let result = helper ((Poly p1:poly), (Poly p2:poly))
      Poly result:poly
  with
  | EmptyList ->  (printfn "This is an empty polynomial!")
                  if (p1.IsEmpty) then 
                    ( Poly p1:poly ) 
                  else 
                    ( Poly p2:poly )

let exp(b:float, e:int) =
  let rec helper(b:float, e:int, a: float) =
    if (b = 0.0) then 0.0
    elif (e = 0) then a
    elif (e % 2 = 1) then helper(b,e-1, b*a)
    else helper(b*b,e/2,a)
  helper(b,e,1.0)

let evalTerm (v:float) (Term (c,e):term) = if (e=0) then c else c * exp(v,e)

let evalPoly(Poly p:poly,v:float):float = 
  try
    if (p.IsEmpty) then 
      raise EmptyList 
    else
      let rec helper((Poly p:poly),v:float) = 
        match p with
        | [] -> 0.0
        | x::xs -> (evalTerm v (Term x:term)) + helper((Poly xs:poly),v)
      helper((Poly p:poly),v)
  with
    | EmptyList ->  (printfn "This is an empty polynomial!")
                    v

let diffPoly (Poly p) = 
  try
    if (p.IsEmpty) then 
      raise EmptyList 
    else
      let rec helper(Poly p:poly) =
        match p with
        | [] -> []
        | x::xs -> 
          match x with
          | (a:float,b:int) -> 
            if (b<=0) then
              []
            else
              (a*float b,b-1)::helper(Poly xs:poly)
      let result = helper(Poly p:poly)
      Poly result:poly
  with
    | EmptyList ->  (printfn "This is an empty polynomial!")
                    (Poly p:poly)

(* Question 3 *)
type Exptree =
  | Const of int 
  | Var of string 
  | Add of Exptree * Exptree 
  | Mul of Exptree * Exptree

type Bindings = (string * int) list

(* exception notFound *)

let rec lookup(name:string, env: Bindings) = 
  match env with
  | [] -> None
  | x::xs -> 
    match x with 
    | (a:string,b:int) -> if (name=a) then Some(b) else lookup (name, xs)

let rec insert(name:string, value: int, b: Bindings) = 
  match b with
  | [] -> [((name:string), (value:int))]
  | x::xs -> if (name <= fst x) then ((name:string),(value:int))::b else insert(name,value,xs)
                                           
let rec eval(exp : Exptree, env:Bindings) = 
  match exp with
  | Add (a,b) ->
    match eval(a,env) with
    | None -> None
    | Some(a) ->
      match eval(b,env) with
      | None -> None
      | Some(b) -> Some(a + b)
  | Mul (a,b) ->
    match eval(a,env) with
      | None -> None
      | Some(a) ->
        match eval(b,env) with
          | None -> None
          | Some(b) -> Some(a * b)
  | Var a ->
    match (lookup (a,env)) with
      | None -> None
      | Some(b) -> Some(b)
  | Const a -> Some(a)

let env:Bindings = [("a",3);("b",4);("c",5)]                                

let exp1 = Add(Const 3, Const 4)
let exp2 = Add(Const 3, Var "b")
let exp3 = Add(Var "c", Var "b")
let exp4 = Mul(exp3,exp2)
let exp5 = Add(Var "d",exp3)
let env2 = insert("b",10,env)



(* Question 4 *)

type Team = string
type Goals = Goals of int
type Points = Points of int
type Fixture = Team * Team  
type Result = ((Team * Goals) * (Team * Goals))
type Table = Map<Team,Points>
    
let league =
  ["Chelsea"; "Spurs"; "Liverpool"; "ManCity"; "ManUnited"; "Arsenal"; "Everton"; "Leicester"]

let pointsMade (r: Result) = 
  match r with 
  | (a:string,(Goals b)),(c:string,(Goals d)) -> 
    if (b>d) then 
      ((a,(Points 3)),(c,(Points 0)))
    elif (b=d) then
      ((a,(Points 1)),(c,(Points 1)))
    else
      ((a,(Points 0)),(c,(Points 3)))

let initEntry (name:Team) = (name, Points 0)
           
let initializeTable l = Map.ofList (List.map initEntry l)

let weekend1:Result list = [(("Chelsea", Goals 2),("Spurs", Goals 1)); (("Liverpool", Goals 3),("ManCity", Goals 2));(("ManUnited", Goals 1),("Arsenal", Goals 4));(("Everton", Goals 1),("Leicester", Goals 5))]

let weekend2:Result list = [(("Chelsea", Goals 5),("Arsenal", Goals 0)); (("Spurs", Goals 3),("ManCity",Goals 2)); (("ManUnited", Goals 1),("Liverpool", Goals 0));(("Everton", Goals 3),("Leicester", Goals 5))]

let s = [weekend2;weekend1]

let updateTable(t:Table,r:Result):Table = 
  let updatePoints = pointsMade(r:Result)
  match updatePoints with
  | (a:string,(Points b)),(c:string,(Points d)) ->
    match (Map.find a t) with
    | (Points k) ->
      match (Map.find c t) with
      | (Points l) ->
        match t with 
        | (s:Table) ->
          let map1 = s.Add((a:Team), (Points(k+b)))
          let map2 = map1.Add((c:Team),(Points(l+d)))
          let map3 = Map.toSeq map2
          Table map3:Table

let tab = initializeTable league

let rec weekendUpdate(t:Table,rl: Result list): Table = 
  match (rl:(Result list)) with
  | [] -> t:Table
  | x::xs -> weekendUpdate((updateTable((t:Table),(x:Result))),(xs: Result list))

let rec seasonUpdate(t:Table, sll:Result list list) : Table = 
  match (sll:(Result list list)) with
  | [] -> t:Table
  | x::xs -> seasonUpdate(weekendUpdate((t:Table),(x:(Result list))),(xs: Result list list))

let less((s1,n1):Team * Points, (s2,n2):Team * Points) =  
  match n1 with 
  | (Points n1) ->
    match n2 with 
    | (Points n2) ->
      if (n1<n2) then 
        true
      else
        false

let rec myinsert item lst =
  match lst with
  | [] -> [item]
  | x::xs -> if less(item,x) then x::(myinsert item xs) else item::lst

let rec isort lst =
  match lst with
  | [] -> []
  | x::xs -> myinsert x (isort xs)

let showStandings (t:Table) = isort (Map.toList t)

(* Question 5 *)

type Destination = City of string
type RoadMap = Roads of Map<Destination, Set<Destination>>

let roadData = [
  "Andulo", ["Bibala"; "Cacolo"; "Dondo"]
  "Bibala", ["Andulo"; "Dondo"; "Galo"]
  "Cacolo", ["Andulo"; "Dondo"]
  "Dondo",  ["Andulo"; "Bibala"; "Cacolo"; "Ekunha"; "Funda"]
  "Ekunha", ["Dondo"; "Funda"]
  "Funda",  ["Dondo"; "Ekunha"; "Galo"; "Kuito"]
  "Galo",   ["Bibala"; "Funda"; "Huambo"; "Jamba"]
  "Huambo", ["Galo"]
  "Jamba",  ["Galo"]
  "Kuito",  ["Ekunha"; "Funda"]
]

let makeRoadMap data = 
  let rec helper data = 
    match data with 
    | [] -> []
    | x::xs -> 
      match x with  
      | (x:string, y:(string list)) ->
        let rec helper2 g = 
          match g with 
          | [] -> [] 
          | z::zs -> (City z)::(helper2 zs)
        let j:(Destination list) = helper2 y
        (City x, Set.ofList j)::(helper xs)
  let u = helper data
  let d = u |> Map.ofList
  Roads d:RoadMap

let rec upToManySteps (Roads r) n startCity = 
  if (n<=0) then
    Set.empty
  else 
    let g = Map.find startCity r
    let o = Set.toList g
    let rec helper (Roads r) n cities =
      if (n<=0) then
        []
      else
        match cities with 
        | [] -> []
        | x::xs ->
          let w = Map.find x r
          let v = Set.toList w
          let a = v@cities
          let b = a@(helper (Roads r) (n-1) v)
          b@(helper (Roads r) n xs)
    let j = helper (Roads r) (n-1) o
    Set.ofList j

let theMap = makeRoadMap roadData

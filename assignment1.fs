(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Marie Payne, Id Number: 260686859 *) (* Edit this line. *)

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like, except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  Your code MUST compile and must NOT go into infinite
loops.  An assignment like that means you have not tested it.  You will get
ZERO FOR THE ENTIRE ASSIGNMENT even if the problem is only with one
question.  If you are not able to get the code to compile and run do not
submit it.  *)

(* module hw1_sol.  Use this if you want to load the file into an interactive session.*)

(* Question 1 *) (* Do not edit this line. *)

let rec sumlist l = 
  match l with 
  | [] -> 0.0
  | (x::xs) -> x + sumlist xs 

(* Assuming that matching two lists together means pairing each element with the element at the same index,
e.g. in two lists [1;2;3] and [4;5;6], pairing them gives [(1,4); (2,5); (3,6)]. *)
let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y) :: pairlists(xs, ys)

let w_mean weights data =  
  let pairs = pairlists(weights, data)
  let rec numerator l =
    match l with 
      | [] -> []
      | (x,y)::xs -> (x*y) :: numerator xs
  let sum = sumlist (numerator pairs)
  let denom = sumlist weights
  sum / denom

  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
  match pair with
  | (a,[]) -> false
  | (a, x::xs) -> if (a=x) then true else memberof (a, xs)


let rec remove(item, lst) = 
  match (item, lst) with
  | (item, []) -> lst
  | (item, x::xs) -> if (item=x) then xs else x::(remove(item, xs)) 


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with
    | [] -> m
    | x::xs -> if (x>m) then helper(xs, x) else helper(xs, m)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
(* Assuming we can use the functions from previous questions in the body of the
selsort function*)
let rec selsort l = 
  match l with
  | [] -> []
  | x::xs -> 
    match xs with 
    | [] -> [x]    
    | lst ->
      if (x>(findMax xs)) then x::(selsort xs) 
      elif (x=(findMax xs)) then (selsort xs) 
      else 
        let lst = remove((findMax xs), xs)
        selsort ((findMax xs)::x::lst)

(* Question 5. *)  (* Do not edit this line. *)
let rec common twolists = 
  match twolists with
  | ([],[]) -> []
  | ([],l) -> []
  | (x::xs, l) -> 
    if (memberof(x,l)) then 
      x::common(xs, l)
    else 
      common (xs,l)


(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
  match l with
  | [] -> ([],[])
  | [x] -> ([x], [])
  | x::y::xs -> 
    let (k,l) = split xs
    in (x::k,y::l)

let rec merge twolists = 
  match twolists with
  | ([], l) -> l
  | (l, []) -> l
  | (x::xs,y::ys) -> 
    if(x<=y) then
      x::(merge (xs, y::ys))
    else
      y::(merge (x::xs, ys))

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | ns -> 
    let (k,l) = split ns
    in merge((mergesort k), (mergesort l))
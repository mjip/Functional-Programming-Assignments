(* Assignment 3 *) (* Do not edit this line. *)
(* Student name: Marie Payne, Id Number: ********* *) (* Edit this line. *)

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like, except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  It is OK to change a "rec" declaration and put the
recursive function inside a helper if you want to.  Your code MUST compile
and must NOT go into infinite loops. *)

(* Question 1 *)
type Cell = { data : int; next : RList}
and RList = Cell option ref

(* This converts an RList to an ordinary list, which is then displayed. *)
let rec displayList (c : RList) =
  match !c with
    | None -> []
    | Some { data = d; next = l } -> d :: (displayList l)

(* This converts a cell to a list.  You may find it useful for testing.  No need to
use it in your solution. *)

let cellToRList (c:Cell):RList = ref (Some c)

(* This is what you need to code. *)
let reverse (lst: RList) = 
  let lstcpy = ref lst
  let rec helper ((lstcpy: RList), (revlst: RList)) =
      match !lstcpy with
      | None -> revlst
      | Some { data = d; next = l } -> 
        lstcpy := Some { data = d; next = revlst }
        helper(l, lstcpy)
  helper(lst, (ref None))



let c1 = {data = 1; next = ref None}
let c2 = {data = 2; next = ref (Some c1)}
let c3 = {data = 3; next = ref (Some c2)}
let c5 = {data = 5; next = ref (Some c3)}

let result = cellToRList c5 |> reverse |> displayList


(* Question 2*)

type transaction = Withdraw of int | Deposit of int | CheckBalance | ChangePassword of string | Close

(* I couldn't get the boolean to update correctly by using a combination of refs and mutables and records, so I did it inefficiently like this.*)

let makeProtectedAccount(openingBalance: int, password: string) =
    let pass = ref password
    let balance = ref openingBalance
    let closed = ref false

    let makeAccount = fun (password: string, act: transaction) ->
        if password.Equals(!pass) then
            match act with
            | Withdraw(m) -> 
                if(!closed) then printfn "Account closed."
                else
                    if (!balance >= m) then
                        balance := !balance - m
                        printfn "The new balance is: %i." !balance
                    else
                        printfn "Insufficient funds."
            | Deposit(m) ->
                if(!closed) then printfn "Account closed."
                else
                    balance := !balance + m
                    printfn "The new balance is: %i." !balance
            | CheckBalance -> 
                if(!closed) then printfn "Account closed."
                else printfn "The new balance is: %i." !balance
            | ChangePassword(p) -> 
                if(!closed) then printfn "Account closed."
                else
                    pass := p
                    printfn "Password changed."
            | Close ->  
                if(!closed) then printfn "Account closed."
                else
                    closed := true
                    printfn "Account successfully closed."
        else
            printfn "Incorrect password."
    makeAccount


(* Question 3 *)

open System.Collections.Generic;;

type ListTree<'a> = Node of 'a * (ListTree<'a> list)

let bfIter f ltr = 
    let q = new Queue<ListTree<'a>>()
    q.Enqueue(ltr)
    while (q.Count > 0) do
        let (parent, child) =
            match q.Dequeue() with
            | Node(a,b) -> (a,b)
        f parent
        for i in child do
            let (p, c) =
                match i with
                | Node(a,b) -> (a,b)
            f p
            q.Enqueue(i)

(* Some examples I used for testing.  *)
let n5 = Node(5,[])
let n6 = Node(6,[])
let n7 = Node(7,[])
let n8 = Node(8,[])
let n9 = Node(9,[])
let n10 = Node(10,[])
let n12 = Node(12,[])
let n11 = Node(11,[n12])
let n2 = Node(2,[n5;n6;n7;n8])
let n3 = Node(3,[n9;n10])
let n4 = Node(4,[n11])
let n1 = Node(1,[n2;n3;n4])

(* Just for testing, not needed for your solution. *)
let showNode n =
  match n with
    | Node(i,_) -> (printfn "%i" i)

    
bfIter (fun n -> printfn "%i" n) n1

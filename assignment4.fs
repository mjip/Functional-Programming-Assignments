type id = string

type term =
  | Var of id
  | Const of int 
  | Term of id * term list

(* invariant for substitutions: *)
(* no id on a lhs occurs in any term earlier in the list *)
type substitution = (id * term) list

(* check if a variable occurs in a term *)
let rec occurs (x : id) (t : term) : bool = 
  match t with
  | v when v.Equals(Var x) -> true
  | Term (c, y) -> List.exists (fun z -> occurs x z) y
  | e -> false
  

(* substitute term s for all occurrences of variable x in term t *)
let rec subst (s : term) (x : id) (t : term) : term = 
  if (occurs x t) then     
    match t with
    | v when v.Equals(Var x) -> s
    | Term (c, y) -> Term (c, (List.map (fun z -> subst s x z) y))
    | e -> t
  else t


(* apply a substitution right to left; use foldBack *)
let apply (s : substitution) (t : term) : term = 
  List.foldBack (fun x acc -> subst (snd x) (fst x) acc) s t 


(* unify one pair *)
let rec unify (s : term) (t : term) : substitution = 
  match s with
  | Term (c, y) -> 
    match t with
    | Term (d, z) ->
      if (c=d) then 
        if ((List.length y) = (List.length z)) then
          let rec uni (g : term list) (h : term list) =
            match g with
            | [] -> []
            | j::js ->
              match h with
              | [] -> []
              | k::ks ->
                match j with 
                | Const o -> 
                  match k with 
                  | Const p -> if (o<>p) then failwith "not unifiable: clashing constants" else (uni js ks)
                  | Term (a,b) -> failwith "not unifiable: term constant clash" 
                  | Var x -> ((x:id),j)::(uni js ks)
                | Term (q,r) ->
                  match k with
                  | Term (a,b) -> unify j k
                  | Const u -> failwith "not unifiable: term constant clash" 
                  | Var i -> if (occurs (i:id) j) then failwith "not unifiable: circularity" else ((i:id),j)::(uni js ks)
                | Var w -> 
                  match k with
                  | Var e -> if (w=e) then [] else ((w:id),k)::(uni js ks)
                  | _ -> if (occurs (w:id) k) then failwith "not unifiable: circularity" else ((w:id),k)::(uni js ks)
          let rec dups e b lst =
            match lst with 
            | t::ts ->
              match t with
              | (u,w) -> 
                if (e=u) then let jsdf = uni [b] [w] in (dups e b ts) else (u,w)::(dups e b ts)
            | [] -> []
          let rec helper f =
            match f with 
            | x::xs ->
              match x with 
              | (y,z) -> (y, (apply (dups y z xs) z)) :: (helper (dups y z xs))
            | [] -> [] 
          helper (uni y z)
          (* Going backwards through the list again ensures that every sub is caught,
          and dup removed.           helper (List.rev solution)*)

        else failwith "not unifiable: not same arity" 
      else failwith "not unifiable: head symbol conflict" 
    | _ -> failwith "not unifiable" 
  | _ -> failwith "not unifiable" 

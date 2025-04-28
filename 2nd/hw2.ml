(*Problem 1 - fib*)

let rec fib(n: int) : int =
  if n < 0 then (-1) else
    match n with
    | 0 -> 0
    | 1 -> 1
    | n -> fib (n-2) + fib (n-1)

(*Problem 2 - fib-opt*)

let fib_opt(n: int) : int =
  if n < 0 then (-1) else
  let rec tail_fib n prev acc =
    if n = 0 then acc else
      if n = 1 then acc+prev else
        tail_fib (n-1) acc (acc+prev)
  in tail_fib n 1 0
    
(*Problem 3 - last*)

let rec last(ls: int list) : int =
    match ls with
    | [] -> failwith "The given list is empty"
    | h :: t -> if t = [] then h else last t

(*Problem 4 - second_last*)

let rec second_last (ls: int list) : int =
  match ls with
  | [] -> failwith "The given list is empty"
  | _ :: [] -> failwith "The given list has a sole element"
  | h :: s :: t -> if t = [] then h else
    second_last (s::t)

(*Problem 5 - len*)

let rec len (ls : int list) : int =
  match ls with
  | [] -> 0
  | _ :: t -> 1 + len t

(*Problem 6 - rev*)

let rev (ls: int list) : int list =
  let rec inner_rev ls acc =
    match ls with
    | [] -> acc
    | h :: t -> if t = [] then h::acc else
    inner_rev t (h::acc)
  in inner_rev ls []

(*Problem 7 - is_palindrome*)

let is_palindrome (ls: int list) : bool =
    let rec tail_palindrome ls rev =
      match ls with
      | [] -> true
      | h :: t -> match rev with
        | [] -> true
        | i :: u -> if h = i then tail_palindrome t u
        else false
    in tail_palindrome ls (rev ls)


(*Problem 8 - compress*)

let rev_tuple (ls: (int * char) list) : (int * char) list = 
  let rec inner_rev_tuple ls acc = 
    match ls with
    | [] -> acc
    | h :: t -> if t = [] then h::acc else
    inner_rev_tuple t (h::acc)
  in inner_rev_tuple ls []

let compress (s: string) : (int * char) list =
  let rec tail_compress str_list prev num acc =
    match str_list with
    | [] -> (num, prev) :: acc
    | h :: t -> if prev = h then tail_compress t h (num+1) acc else
      tail_compress t h 1 ((num, prev) :: acc)
    in match (List.of_seq(String.to_seq s)) with
    | [] -> []
    | h :: t -> rev_tuple (tail_compress t h 1 [])

(*Problem 9 - sort*)

let rec insert f ls e = 
  match ls with
  | [] -> [e]
  | h :: t -> if f e h then e :: ls else h :: insert f t e

let sort (f: int -> int -> bool) ( ls: int list) : int list =
  let rec insert_sort f ls acc =
    match ls with
    | [] -> acc
    | h :: t -> insert_sort f t (insert f acc h)
  in insert_sort f ls []

(*Problem 10 - traverse*)

type id = int
type tree = Nil | N of id * tree * tree


let tree_rev (ls: int list) : int list =
  let rec inner_tree_rev ls acc =
    match ls with
    | [] -> acc
    | h :: t -> if t = [] then h::acc else
    inner_tree_rev t (h::acc)
  in inner_tree_rev ls []

let rec post t acc =
  match t with
  | Nil -> acc
  | N (id, left, right) -> 
      let acc_left = post left acc in
      let acc_right = post right acc_left in
      id :: acc_right

  let traverse (t: tree) : id list =
    tree_rev (post t [])
  

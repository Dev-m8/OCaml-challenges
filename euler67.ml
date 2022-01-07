#load "str.cma" 

let rec foldr1 f xs =
  match xs with
  | []        -> failwith "Empty list"
  | [x]       -> x
  | x::tl     -> f x (foldr1 f tl)

let rec zipWidth f lst1 lst2 lst3 = match lst1,lst2, lst3 with
  | [],_,_ -> []
  | _,[],_-> []
  | _,_,[] -> []
  | (x::xs),(y::ys),(z::zs) -> (f x y z)::(zipWidth f xs ys zs)

(* Recursive function to compute max sum pairs of 2 rows *)
let maxOfrows xs ys = 
  let t = List.tl ys in
  let maxpair x y z = x + max y z in
  zipWidth maxpair xs ys t

let triangle = [[3];[7;4];[2;4;6];[8;5;9;3]]

let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

let strToint xs = List.map int_of_string (Str.split (Str.regexp "[^0-9]+") xs)

(* Combine above functions to read lines of numbers from file
and compute maxSum recrusively from bottom up *)
let maxSum filename = 
  let listOfstrings = read_file filename in
  let listOfnumbers = List.map (fun x -> strToint x) listOfstrings in
  let ls = foldr1 maxOfrows listOfnumbers in
  List.hd ls

(* Test utop # #use "btg_assign.ml";;
val foldr1 : ('a -> 'a -> 'a) -> 'a list -> 'a = <fun>
val zipWidth :
  ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list -> 'c list -> 'd list = <fun>
val maxOfrows : int list -> int list -> int list = <fun>
val triangle : int list list = [[3]; [7; 4]; [2; 4; 6]; [8; 5; 9; 3]]
val read_file : string -> string list = <fun>
val strToint : string -> int list = <fun>
val maxSum : string -> int = <fun>
─( 00:36:13 )─< command 3 >──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # maxSum "example.txt";;
- : int = 23
─( 00:36:34 )─< command 4 >──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # maxSum "example2.txt";;
- : int = 1074
─( 00:37:11 )─< command 5 >──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # *)

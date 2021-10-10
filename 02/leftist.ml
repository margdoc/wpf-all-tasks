(* Drzewa Lewicowe *)
(* Mikołaj Grzebieluch *)
(* Review - Wojciech Przytuła *)

(* typ kolejki priorytetowej zaimplentowanej na drzewie lewicowym *)
(* Leaf - puste drzewo *)
(* Node - niepuste drzewo, gdzie 
      left - jest lewym poddrzewem
      right - jest prawym poddrzewem
      value - jest wartością w danym wierzchołku
      right_path - jest długością skrajnie prawej ścieżki 
                   wychodzącą z danego wierzchołka *)
type 'a queue = 
  | Leaf
  | Node of {left : 'a queue; right : 'a queue; 
             value : 'a; right_path : int}

let empty = Leafk

(* 'a queue -> bool *)
let is_empty q = q = Leaf

exception Empty


(* 'a queue -> int*)
let right_path q = 
  match q with
  | Leaf -> 0
  | Node n -> n.right_path


(* 'a queue -> 'a queue -> 'a queue *)
let rec join q1 q2 =
  match (q1, q2) with
  | (Leaf, _) -> q2
  | (_, Leaf) -> q1
  | (Node d1, Node d2) ->
    if d2.value < d1.value then
      join_nodes (Node d2) (Node d1)
    else
      join_nodes (Node d1) (Node d2)


(* Łączenie dwóch drzew, w przypadku, gdy nie są liśćmi oraz d1.value < d2.value *)
(* 'a queue -> 'a queue -> 'a queue *)
and join_nodes (Node d1) (Node d2) = 
  let linked = join d1.right (Node d2)
  in let (_left, _right) = 
    if right_path linked < right_path d1.left then
      (d1.left, linked)
    else
      (linked, d1.left)
  in Node {left = _left; right = _right; 
           value = d1.value; right_path = right_path _right + 1}


(* 'a queue -> 'a * 'a queue *)
let delete_min q = 
  match q with
  | Leaf -> raise Empty
  | Node d -> (d.value, join d.left d.right)


(* 'a -> 'a queue -> 'a queue *)
let add x q = 
  join (Node {left = Leaf; right = Leaf; value = x; right_path = 1}) q

  
(* Testy pochodzą ze wspólnej puli testów *)
(*
(* delete_min string tests *)
let b = add "a" empty;;
let b = add "aca" b;;
let b = add "nzbzad" b;;
let b = add "nzbza" b;;
let b = add "bxbxc" b;;
let (a,b) = delete_min b;;
assert (a = "a");;
let (a,b) = delete_min b;;
assert (a = "aca");;
let (a,b) = delete_min b;;
assert (a = "bxbxc");;
let (a,b) = delete_min b;;
assert (a = "nzbza");;
let (a,b) = delete_min b;;
assert (a = "nzbzad");;
assert(is_empty b = true);;
assert (try let _=delete_min b in false with Empty -> true);;

(* join tests *)
let b = add 1 empty;;
let b = add 3 b;;
let b = add (-1) b;;
let b = add 2 b;;
let b = add 1 b;;
let c = add 10 empty;;
let c = add (-5) c;;
let c = add 1 c;;
let c = add 4 c;;
let c = add 0 c;;
let b = join b c;;
let (a,b) = delete_min b;;
assert (a = (-5));;
let (a,b) = delete_min b;;
assert (a = (-1));;
let (a,b) = delete_min b;;
assert (a = 0);;
let (a,b) = delete_min b;;
assert (a = 1);;
let (a,b) = delete_min b;;
assert (a = 1);;
let (a,b) = delete_min b;;
assert (a = 1);;
let (a,b) = delete_min b;;
assert (a = 2);;
let (a,b) = delete_min b;;
assert (a = 3);;
let (a,b) = delete_min b;;
assert (a = 4);;
let (a,b) = delete_min b;;
assert (a = 10);;
assert (try let _=delete_min b in false with Empty -> true);;
*)

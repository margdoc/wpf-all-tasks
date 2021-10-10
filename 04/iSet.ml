(*
 * ISet - Interval sets
 * Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl, Jacek Chrzaszcz
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(* Mikołaj Grzebieluch *)
(* Review - Rafał Szulc *)


(* typ reprezentujący zbiór liczb całkowitych 
   zaimplementowany za pomocą drzewa AVL, gdzie

  Empty - zbiór pusty
  Node (l, v, r, h, p) - niepusty zbiór, w którym: 
    l - lewe podrzewo
    v - przedział znajdujący się w najwyższym wierzchołku
    r - prawe poddrzewo
    h - wysokość drzewa
    p - liczba liczb całkowitych w zbiorze *)
type t = 
  | Empty
  | Node of t * (int * int) * t * int * int


(* we wszystkim funkcjach zakładam, 
  że drzewa podane na wejściu spełniają warunek drzewa AVL *)
(* funckje zwracające drzewa, zwracają drzewa spełniające warunek drzew AVL *)


(* funkcja zwracająca wysokość drzewa *)
(* t -> int *)
let height = function
  | Node (_, _, _, h, _) -> h
  | Empty -> 0


(* funkcja zwracająca liczbę liczb całkowitych w zbiorze *)
(* t -> int *)
let set_power = function
  | Node (_, _, _, _, p) -> p
  | Empty -> 0


(* funkcja obsługująca dodawanie dużych liczb *)
(* int -> int -> int *)
let add_int a b =
  if a > 0 && b > 0 then
    if max_int - a <= b then max_int
    else a + b
  else if a < 0 && b < 0 then
    if min_int - a >= b then min_int
    else a + b
  else a + b


(* funkcja sumująca listę liczb całkowitych *)
(* int list -> int *)
let add_ints = List.fold_left add_int 0


(* funkcja zwracająca liczbę liczb całkowitych w danym przedziale *)
(* interval -> int *)
let interval_size k =
  if fst k = min_int then add_ints [snd k; -(fst k + 1); 2]
  else add_ints [snd k; -fst k; 1]


(* funkcja tworząca drzewo z podanego lewego i prawego poddrzewa 
  oraz przedziału znajdującego się w korzeniu drzewa *)
(* t -> interval -> t -> t *)
let make l k r = Node (l, k, r, max (height l) (height r) + 1, 
    add_ints [set_power l; set_power r; interval_size k])


(* funkcja balansująca drzewo *)
(* t -> interval -> t -> t *)
let bal l k r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _, _) ->
      if height ll >= height lr then make ll lk (make lr k r)
      else
        (match lr with
        | Node (lrl, lrk, lrr, _, _) ->
          make (make ll lk lrl) lrk (make lrr k r)
        | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _, _) ->
      if height rr >= height rl then make (make l k rl) rk rr
      else
        (match rl with
        | Node (rll, rlk, rlr, _, _) ->
          make (make l k rll) rlk (make rlr rk rr)
        | Empty -> assert false)
    | Empty -> assert false
  else make l k r


(* funkcja zwracająca najmniejszy element drzewa *)
(* t -> int *)
let rec min_elt = function
  | Node (Empty, k, _, _, _) -> k
  | Node (l, _, _, _, _) -> min_elt l
  | Empty -> raise Not_found


(* funkcja zwracająca drzewo bez jego najmniejszego elementu *)
(* t -> t *)
let rec remove_min_elt = function
  | Node (Empty, _, r, _, _) -> r
  | Node (l, k, r, _, _) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "PSet.remove_min_elt"


(* pusty zbiór *)
(* t *)
let empty = Empty


(* funkcja zwracająca true, jeśli zbiór jest pusty *)
(* t -> bool *)
let is_empty t = (t = Empty)


(* funkcja zwracająca true, jeśli podana liczba znajduje się w zbiorze *)
(* int -> t -> bool *)
let rec mem a = function 
  | Empty -> false
  | Node (l, k, r, _, _) ->
    if a < fst k then mem a l
    else if a <= snd k then true
    else mem a r


(* funkcja, która dodaje jeden element do drzewa, przy założeniu, 
  że element ten nie koliduje z żadnym innym elementem drzewa *)
(* interval -> t -> t *)
let rec add_one x = function
  | Empty -> Node (Empty, x, Empty, 1, interval_size x)
  | Node (l, k, r, h, p) ->
    let c = compare x k in
    if c = 0 then Node (l, x, r, h, p)
    else if c < 0 then
      let nl = add_one x l in
      bal nl k r
    else
      let nr = add_one x r in
      bal l k nr


(* funkcja, która tworzy i równoważy drzewo z dwóch drzew i nowego elementu *)
(* t -> interval -> t -> t *)
let rec join l k r =
  match (l, r) with
  | Empty, _ -> add_one k r
  | _, Empty -> add_one k l
  | Node(ll, lk, lr, lh, _), Node(rl, rk, rr, rh, _) ->
    if lh > rh + 2 then bal ll lk (join lr k r) else
    if rh > lh + 2 then bal (join l k rl) rk rr else
    make l k r


(* funkcja, która łączy i równoważy dwa drzewa *)
(* t -> t -> t *)
let merge t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
    let k = min_elt t2 in
    join t1 k (remove_min_elt t2)


(* funkcja porównująca liczbę i przedział *)
(* int -> interval -> int *)
let cmp x k =
  if x < fst k then -1
  else if x > snd k then 1
  else 0


(* funkcja, która usuwa z drzewa wszystkie przedziały, które nie są rozłączne 
  lub sąsiadują z przedziałem (min_int, b) 
  oraz zwraca największą usuniętą liczbę *)
(* int -> t -> t * int *)
let rec find_max b = function
  | Empty -> (Empty, b)
  | Node (l, k, r, _, _) ->
    if b < fst k - 1 then
      let (nl, nb) = find_max b l
      in (bal nl k r, nb)
    else if b <= snd k then (r, snd k)  
    else find_max b r


(* funkcja, która usuwa z drzewa wszystkie przedziały, które nie są rozłączne 
  lub sąsiadują z przedziałem (a, max_int) 
  oraz zwraca najmniejszą usuniętą liczbę *)
(* int -> t -> t * int *)
let rec find_min a = function
  | Empty -> (Empty, a)
  | Node (l, k, r, _, _) ->
    if a > snd k + 1 && snd k <> max_int then
      let (nr, na) = find_min a r
      in (bal l k nr, na)
    else if a >= fst k then (l, fst k)
    else find_min a l


(* funkcja, która dodaje przedział liczb do zbioru *)
(* interval -> t -> t *)
let rec add x = function
  | Empty -> add_one x Empty
  | Node (l, k, r, _, _) ->
    if snd x < fst k - 1 && fst k <> min_int then bal (add x l) k r
    else if fst x > snd k + 1 && snd k <> max_int then bal l k (add x r)
    else 
      let (nl, a) = find_min (min (fst x) (fst k)) l
      and (nr, b) = find_max (max (snd x) (snd k)) r
      in add_one (a, b) (merge nl nr)


(* funkcja, która dla podanej liczby (x) i zbioru (t) 
  zwraca krotkę (left, present, right), gdzie

  left - zbiór liczb mniejszych od x w zbiorze t
  present - true jeśli x należy do zbioru t
  right - zbiór liczb większych od x w zbiorze t *)
(* int -> t -> t * bool * t *)
let split =
  let rec loop x = function
    | Empty ->
      (Empty, false, Empty)
    | Node (l, k, r, _, _) ->
      let c = cmp x k in
      if c = 0 then
        let nl = 
          if fst k = x then l
          else add_one (fst k, x - 1) l
        and nr =
          if snd k = x then r
          else add_one (x + 1, snd k) r   
        in (nl , true, nr)
      else if c < 0 then
        let (ll, pres, rl) = loop x l in (ll, pres, join rl k r)
      else
        let (lr, pres, rr) = loop x r in (join l k lr, pres, rr)
  in loop


(* funkcja która usuwa przedział liczb ze zbioru *)
(* interval -> t -> t *)
let remove x = function
  | Empty -> Empty
  | t ->
    let (l, _, r) = split (fst x) t
    in let (_, _, rr) = split (snd x) r
    in merge l rr 


(* funkcja, która wywołuje funkcje ('a -> unit) dla każdego przedziału 
  w zbiorze w kolejności rosnącej *)
(* (interval -> unit) -> t -> unit *)
let iter f =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _, _) -> loop l; f k; loop r 
  in loop


(* funkcja, która wywołuje funkcje dla każdego przedziału w zbiorze 
  w kolejności rosnącej *)
(* (interval -> 'a -> 'a) -> t -> 'a -> 'a *)
let fold f t acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _, _) ->
      loop (f k (loop acc l)) r 
  in loop acc t


(* funkjca zwracająca wszystkie przedziały w zbiorze w kolejności rosnącej *)
(* t -> interval list *)
let elements t = List.rev (fold (fun k acc -> k :: acc) t [])


(* funkcja zwracająca liczbę liczb mniejszych lub równych a w zbiorze t *)
(* int -> t -> int *)
let below a =
  let rec below_help a acc = function
    | Empty -> acc
    | Node (l, k, r, _, _) ->
      if a > snd k then 
        below_help a (add_ints [acc; set_power l; interval_size k]) r
      else if a >= fst k then 
        add_ints [acc; set_power l; interval_size (fst k, a)]
      else below_help a acc l
  in below_help a 0

(* Testy pochodzą ze wspólnej puli testów *)
(*
(* Copyright Artur "mrowqa" Jamro 2015 *)
let zle = ref 0
let test (id:int) (result:bool) (expected:bool) : unit =
    if result <> expected then begin
        Printf.printf "Zly wynik testu %d!\n" id;
        incr zle
    end;;


let s = empty;;
test 11 (is_empty s) true;;
test 12 (is_empty (add (1, 1) s)) false;;


(* niestety musimy zalozyc poprawnosc mem... *)

let s = add (10, 12) empty;;
test 21 (mem 9 s) false;;
test 22 (mem 10 s) true;;
test 23 (mem 12 s) true;;
test 24 (mem 13 s) false;;

let s = add (4, 7) s;;
test 25 (mem 8 s) false;;
test 26 (mem 11 s) true;;
test 27 (mem 5 s) true;;
test 28 (mem 3 s) false;;


let s = add (1, 1) (add (15, 16) (add (10, 14) (add (6, 9) empty)));;
test 31 (mem 10 (remove (10, 10) s)) false;;
test 32 (is_empty (remove (1, 20) s)) true;;
test 33 (mem 7 (remove (8, 15) s)) true;;

let s = add (-1, 1) (add (3, 7) (add (10, 12) (add (15, 18)
        (add (-15, -13) empty))));;
let s = remove (-10, 12) s;;
test 34 (is_empty s) false;;
test 35 (mem 5 s) false;;
test 36 (mem (-10) s) false;;
test 37 (mem (-15) s) true;;
test 38 (mem 17 s) true;;


test 41 (elements (add (4, 5) (add (7, 8) empty)) = [(4, 5); (7, 8)]) true;;
test 42 (elements (add (1, 1) (add (11, 14) (add (6, 9) (add (4, 5) empty))))
        = [(1, 1); (4, 9); (11, 14)]) true;;


let s = add (3, 4) (add (8, 10) (add (15, 20) empty));;
test 51 (below 2 s = 0) true;;
test 52 (below 3 s = 1) true;;
test 53 (below 10 s = 5) true;;
test 54 (below 15 s = 6) true;;
test 55 (below 100 s = 11) true;;
let s = add (1, max_int) (add (-1, 0) empty);;
test 56 (below max_int s = max_int) true;;
let s = add (-min_int, max_int) empty;;
test 57 (below max_int s = max_int) true;;
test 58 (below min_int s = 1) true;;


let s = add (3, 4) (add (8, 10) (add (15, 20) empty));;
let l, pres, r = split 9 s;;
test 61 (mem 9 l) false;;
test 62 (mem 9 r) false;;
test 63 (mem 8 l) true;;
test 64 (mem 10 r) true;;
test 65 pres true;;
test 66 (mem 7 l) false;;
test 67 (mem 4 l) true;;
test 68 (mem 11 r) false;;
test 69 (mem 16 r) true;;


let s = add (1, 1) (add (11, 14) (add (6, 9) (add (4, 5) empty)));;
let a = ref [];;
let foo x = a := x::!a; ();;
test 71 (iter foo s; !a = [(11, 14); (4, 9); (1, 1)]) true;;


let s = add (1, 1) (add (11, 14) (add (6, 9) (add (4, 5) empty)));;
let foo x a = x::a;;
test 81 (fold foo s [] = [(11, 14); (4, 9); (1, 1)]) true;;


let _ =
    if !zle = 0 then
        ()
    else
        Printf.printf "\nZlych odpowiedzi: %d.\n" !zle
;;
*)

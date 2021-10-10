(* Mikołaj Grzebieluch *)
(* Code review - Kamil Ciebiera *)


(* Wyjątek rzucany, gdy znajdziemy stan, który mieliśmy uzyskać *)
exception Found of int


(* Funkcja zwracająca NWD dwóch liczb *)
(* int -> int -> int *)
let rec gcd a b =
  if b = 0 then a else gcd b (a mod b)


(* Ruchy które możemy wykonać *)
type action =
  | Empty of int
  | Fill of int
  | Transfer of int * int


(* (int * int) array -> int *)
let przelewanka arr =
  (* usunięcie pustych pojemników *)
  let new_arr = Array.of_list (Array.fold_right (
    fun v acc -> if (fst v) = 0 then acc else v :: acc
    ) arr [])
  in let n = Array.length new_arr
  in if n = 0 then 0
  else

    let capacities = 
      Array.init n (function i -> fst new_arr.(i))


    and search_for = 
      Array.init n (function i -> snd new_arr.(i))
    
      
    (* sprawdzenie, czy stan końcowy jest osiągalny *)

    (* przypadek gdy NWD wszystkich pojemności nie dzieli któregoś stanu końcowego *)
    in let _gcd = Array.fold_left gcd capacities.(0) capacities
    in if _gcd <> 1 && (Array.fold_left (
        fun acc v -> if v mod _gcd <> 0 then true else acc
        ) false search_for) then -1
    else
    (* przypadek gdy nie ma w stanie końcowym pustego lub pełnego pojemnika *)
      if not (Array.fold_left (
        fun acc (c, s) -> if c = s || s = 0 then true else acc
        ) false new_arr) then -1
      else
        (* Kolejka do algorytmu BFS *)
        let queue = Queue.create ()
        
        
        (* Tablica przetrzymująca osiągnięte stany *)
        and hashtbl = Hashtbl.create 4096


        (* Sprawdzenie czy stan jest stanem końcowym 
        i dodanie stanu do tych już osiągniętych *)
        (* 'a -> int -> unit *)
        in let add_state state moves =
            if state = search_for then raise (Found moves)
            else
              if Hashtbl.mem hashtbl state then ()
              else
                begin
                  Hashtbl.add hashtbl state ();
                  Queue.add (state, moves) queue
                end 


        (* Wykonanie ruchu *)
        (* int array * int -> action  -> unit *)
        in let take_action (state, moves) = function
        | Empty i ->
          if state.(i) = 0 then ()
          else
            let new_state = Array.copy state in
            new_state.(i) <- 0;
            add_state new_state (moves + 1)
          
        | Fill i ->
          if state.(i) = capacities.(i) then ()
          else
            let new_state = Array.copy state in
            new_state.(i) <- capacities.(i);
            add_state new_state (moves + 1)

        | Transfer (i, j) ->
          if state.(i) = 0 || state.(j) = capacities.(j) then ()
          else
            let x = min (state.(i)) (capacities.(j) - state.(j))
            and new_state = Array.copy state in
            new_state.(i) <- state.(i) - x;
            new_state.(j) <- state.(j) + x;
            add_state new_state (moves + 1)


        (* unit -> int *)
        in let bfs () = 
        add_state (Array.make n 0) 0;
        while not (Queue.is_empty queue) do
          let elem = Queue.take queue in
            for i = 0 to n - 1 do
              take_action elem (Empty i);
              take_action elem (Fill i);
              for j = i + 1 to n - 1 do
                take_action elem (Transfer (i, j));
                take_action elem (Transfer (j, i));
              done
            done 
        done; -1
        

        in 
          try (bfs ())
          with Found x -> x;;


(* Testy pochodzą ze wspólnej puli testów *)
(* 
Printf.printf "\027[33m
=============================================================================
Mateusz Gienieczko's tests to Przelewanka task.
(I learnt how to use colors, now nothing's the same.)
The tests' author is me, unless explicitly stated otherwise.
=============================================================================";;
print_newline ();;

let num = ref 1;;

let test a answ =
  Printf.printf "\027[34mTest #%d: \027[31m" !num;
  flush stdout;
  incr num;
  let start_time = Sys.time ()
  in
  let usr_answ = przelewanka a
  in
  if answ <> usr_answ then
    Printf.printf "\027[31mWrong answer: read %d, expected %d " usr_answ answ
  else
    Printf.printf "\027[32mOK ";
  Printf.printf "in %.2f s" ((Sys.time ()) -. start_time);
  print_newline ();
  answ = usr_answ
;;

Printf.printf "\027[34mBeginning simple, small tests";;
print_newline ();;

let a = [|(1, 1); (2, 1)|];;
assert(test a 2);;

let a = [||];;
assert(test a 0);;

let a = [|(10, 5); (4, 3); (3, 2); (2, 0)|];;
assert(test a 5);;

let a = [|(50, 50); (50, 48); (2, 2)|];;
assert(test a 3);;

let a = [|(50, 50); (50, 47); (2, 2)|];;
assert(test a (-1));;

let a = [|(13, 9); (17, 3); (7, 2); (2, 2)|];;
assert(test a 9);;

let a = [|(1, 0); (1000000, 999999)|];;
assert(test a 3);;

let a = [|(1, 0); (1000000, 999997)|];;
assert(test a 7);;

let a = [|(9, 6); (12, 9); (12, 3); (999, 411)|];;
assert(test a (-1));;

let a = [|(37, 35); (55, 36)|];;
assert(test a (-1));;

let a = [|(2, 1); (0, 0); (4, 2);|];;
assert(test a (-1));;

let a = [|(0, 0); (0, 0); (0, 0); (0, 0); (0, 0); (0, 0); (99, 66); (3, 3)|];;
assert(test a 22);;

let a = [|(37, 3); (42, 37); (69, 33)|];;
assert(test a (-1));;

let a = [|(1, 0); (1000, 999); (1000000, 999999); (1000000000000, 999999999999)|];;
assert(test a 9);;

let a = [|(24, 13); (12, 5); (6, 2); (1, 0)|];;
assert(test a 10);;

let a = [|(100, 0); (50, 0); (100000, 0); (35, 0)|];;
assert(test a 0);;

Printf.printf "\027[33m\
=============================================================================
These may take a little longer, but not more than 10 - 30 seconds each
=============================================================================";;
print_newline ();;

let a = [|(100, 50); (1000, 500); (50, 25); (5, 5)|];;
assert(test a 20);;

let a = [|(1, 0); (2, 1); (4, 3); (8, 7); (16, 15); (32, 31)|];;
assert(test a 11);;

let a = [|(100, 33); (25, 11); (13, 11); (3, 0); (1, 1)|];;
assert(test a 13);;

let a = [|(1, 0); (2, 1); (3, 1); (4, 2); (5, 2); (6, 3); (7, 3); (8, 4)|];;
assert(test a 10);;

let a = [|(6, 3); (9, 3); (12, 3); (15, 3); (18, 3); (477, 3); (0, 0)|];;
assert(test a (-1));;

let a = [|(1000, 999); (2000, 1998); (2, 0); (0, 0)|];;
assert(test a (-1));;

let a = [|(1000, 498); (2000, 1498); (2, 0)|];;
assert(test a 503);;

let a = [|(1, 0); (77, 43); (150, 149); (333, 37); (37, 2)|];;
assert(test a 12);;

let a = [|(5, 0); (50, 25); (500, 200); (2500, 1500)|];;
assert(test a 23);;

let a = [|(1, 0); (1000000, 500002)|];;
assert(test a 999997);;
	 
let a = [|(1, 0); (1000000, 499991)|];;
assert(test a 999982);;

let a = [|(831, 132); (17, 3); (81, 54); (9, 9)|];;
assert(test a 22);;

let a = [|(4, 0); (6, 4); (20, 12); (50, 20); (101, 98)|];;
assert(test a 13);;

let a = [|(3, 3); (11, 8); (22222, 12321)|];;
assert(test a 1807);;

let a = [|(999, 1); (100, 1); (1000, 1); (0, 0)|];;
assert(test a (-1));;

let a = [|(999,1); (100, 1); (1000, 1); (2, 0); (8, 1)|];;
assert(test a 13);;

Printf.printf "\027[33m\
=============================================================================
Following tests are from III Polish Olympiad in\
Informatics, task \"Mokra robota\".\
The task's author is Piotr Chrzastowski-Wachtel.
=============================================================================";;
print_newline ();;

let a = [|(20, 10); (10, 16)|];;
assert(test a (-1));;

let a = [|(3, 0); (5, 0); (5, 4)|];;
assert(test a 7);;

let a = [|(5, 0); (7, 0); (7, 6)|];;
assert(test a 11);;

let a = [|(5, 0); (7, 0); (7, 2)|];;
assert(test a 3);;

let a = [|(15, 14); (18, 0); (22, 0)|];;
assert(test a 6);;

let a = [|(1, 1); (2, 2); (3, 3); (4, 4)|];;
assert(test a 4);;

let a = [|(30, 0); (17, 17)|];;
assert(test a 1);;

let a = [|(6, 3)|];;
assert(test a (-1));;

let a = [|(26, 3); (28, 1); (30, 1); (32, 1)|];;
assert(test a (-1));;

let a = [|(21, 1); (22, 20); (23, 22); (24, 0)|];;
assert(test a 9);;

let a = [|(31, 0); (33, 17); (35, 14); (37, 35)|];;
assert(test a 16);;

Printf.printf "\027[33m\
=============================================================================
All tests \027[32mOK!
\027[33m\
=============================================================================";;
print_newline ();; 
*)
(* Mikołaj Grzebieluch *)
(* Review - Michał Wiśniewski *)


(** wyjatek rzucany przez [topol] gdy zaleznosci sa cykliczne *)
exception Cykliczne

(* Dodanie wierzchołka do mapy. Jeśli już w niej jest, 
  to łączy jego listę krawędzi z poprzednią, która już tam była  *)
(* 'a -> 'b list -> ('a, 'b) PMap.t -> ('a, 'b) PMap.t *)
let add_node node edges map =
  try (
    let _edges = PMap.find node map
    in PMap.add node (edges @ _edges) map
    )
  with Not_found -> PMap.add node edges map


(* Stworzenie mapy z listy wierzchołków i ich krawędzi*)
(* ('a * 'a list) list -> ('a, 'a) PMap.t *)
let create_map lst =
  List.fold_left (fun map (node, edges) ->
      List.fold_left (fun map node -> 
          add_node node [] map
        ) (add_node node edges map) edges
      ) PMap.empty lst


(** Dla danej listy [(a_1,[a_11;...;a_1n]); ...; (a_m,[a_m1;...;a_mk])] 
    zwraca liste, na ktorej kazdy z elementow a_i oraz a_ij wystepuje
    dokladnie raz i ktora jest uporzadkowana w taki sposob, ze kazdy
    element a_i jest przed kazdym z elementow a_i1 ... a_il *)
(* ('a * 'a list) -> 'a list *)
let topol lst =
  (* struktura reprezentująca graf *)
  let map = create_map lst
  and stack = ref []
  (* zbiór wierzchołków, które należą do ścieżki 
    od początkowego wierzchołka do obecnego *)
  and on_path = ref PMap.empty
  and visited = ref PMap.empty
  (* Funkcja przeszukująca graf w głąb, która sprawdza, 
    czy graf jest cykliczny oraz 
    dodaje wierzchołki do zmiennej stack w kolejności odwrotnej do przeszukiwania *)
  (* 'a -> unit *)
  in let rec dfs node =
    if PMap.mem node !visited then 
      if PMap.mem node !on_path then raise Cykliczne
      else ()
    else
      begin
        visited := PMap.add node () !visited;
        on_path := PMap.add node () !on_path;
        List.iter dfs (PMap.find node map);
        on_path := PMap.remove node !on_path;
        stack := node :: !stack
      end
  in PMap.iter (fun node _ -> dfs node) map; !stack


(* Testy pochodzą ze wspólnej puli testów *)
(* 
(******** SPRAWDZACZKA *********)
(**** POZYCJA a W LIŚCIE l *****)
let pos a l = 
	let poz = ref 0 and use = ref false in
	List.iter (fun x -> if (x <> a && (!use) = false) then poz := (!poz) + 1
	           else use := true ) l;
	(!poz);;	
(***FUNKCJA SPRAWDZAJĄCA POPRAWNOŚĆ ROZWIĄZANIA, GDY NIE MA CYKLU***)
let is_valid l ans =
	let rec pom_valid acc li ans =
		match li with 
			|[] -> acc
			|(f,s)::t -> 
				if (List.fold_left (fun a x -> (if (pos f ans) > (pos x ans) then false else a)) 
				(true) s) then (pom_valid acc t ans)
				else (pom_valid false t ans)
	in pom_valid true l ans	
			 
(*******TESTY*******)
(*******CYKLICZNE*******)

let l = (1,[1])::[];;
try(let _ = topol l in assert(false))
with Cykliczne -> ();;
let l =(1,[2])::(2,[3])::(3,[2])::[];;
try(let _ = topol l in assert(false))
with Cykliczne -> ();;
let l = (1,[2])::(2,[3])::(3,[4;5])::(4,[5])::[];;
assert(is_valid l (topol l));;
let l = (1,[2])::(2,[3])::(3,[4;5])::(4,[2;5])::[];;
try(let _ = topol l in assert(false))
with Cykliczne -> ();;

(******NIEISTNIEJĄCE_W_LIŚCIE_GŁÓWNEJ_WIERZCHOŁKI******)
let l = [];;
let l = (1,[0])::l;;
assert (is_valid l (topol l));;
let l = (2,[0])::l;;
assert (is_valid l (topol l));;

(*******SPRAWDZENIE_CZY_DZIAŁA_NA_INNYCH_TYPACH_NIŻ_INT******)
let l = [];;
let l = ('a',['b';'d'])::l;;
let l = ('b',['c';'d'])::l;;
let l = ('c',['d'])::l;;
assert (is_valid l (topol l));;
let l = [];;
let l = ("fst",["snd";"thr"])::l;;
let l = ("xyz",["abc";"snd"])::l;;
let l = ("cos",["fst";"xyz"])::l;;
assert (is_valid l (topol l));;
let l = [];;
let l = (true,[false])::l;;
let l = (false,[true])::l;;
try(let _ = topol l in assert(false))
with Cykliczne -> ();;

(******TESTY_RÓŻNE******)
let l = [];;
let l = (1,[0])::l;;
assert (is_valid l (topol l));;
let l = (0,[2])::l;;
assert (is_valid l (topol l));;
let l = (2,[3])::l;;
assert (is_valid l (topol l));;
let l = (4,[2;3])::l;;
assert (is_valid l (topol l));;
let l = (6,[2;3])::l;;
assert (is_valid l (topol l));;
let l = (9,[10;11])::l;;
assert (is_valid l (topol l));;
let l = (10,[9])::l;;
try(let _ = topol l in assert(false))
with Cykliczne -> ();; *)

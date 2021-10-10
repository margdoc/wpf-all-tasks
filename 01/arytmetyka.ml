(* Mikołaj Grzebieluch *)
(* Review - Aleksandra Martyniuk *)

type przedzial = float * float (* oznaczane w kodzie jako a, b, c ... *)
type wartosc = przedzial * bool (* oznaczane w kodzie jako x, y, z ... *) 

(* Wartosc zawsze jest postaci [x, y] (1) lub [neg_infinity, x] U [y, infinity] (2) 
   (gdzie x, y są liczbami rzeczywistymi, takimi, że x < y) *)
(* fst wartosc --> przedzial, ktory ona obejmuje *)
  (* fst przedzial --> x*)
  (* snd przedzial --> y*)
(* snd wartosc --> false jeśli (1), true jeśli (2) *)

(* Funkcje pomocnicze *)

(* is_nan: float -> bool *)
let is_nan x = (classify_float x = FP_nan)

(* wartosc_is_nan: wartosc -> bool *)
let wartosc_is_nan x = is_nan (fst (fst x)) || is_nan (snd (fst x))

(* zero_if_nan: float -> float *)
let zero_if_nan x =  if is_nan x then 0. else x
 
(* Używane do zamiany -0. na 0. *)
(* zero_if_zero: float -> float *)
let zero_if_zero x = if x = 0. then 0. else x

(* Określa, czy przedział zawiera liczby ujemne i/lub zero i/lub liczby dodatnie *)
type znak =
  | Zero
  | Ujemny
  | Dodatni
  | Nieujemny
  | Niedodatni
  | Oba

(* znak_przedzialu: przedzial -> znak *)
let znak_przedzialu a =
  match a with
  | (0., 0.) -> Zero
  | (0., _) -> Nieujemny
  | (_, 0.) -> Niedodatni
  | _ ->
    if snd a < 0. then Ujemny
    else if fst a > 0. then Dodatni
      else Oba

(* znak_wartosci: wartosc -> znak *)
let znak_wartosci x = znak_przedzialu (fst x)

(* wartosc_od_przedzialu: przedzial -> wartosc *)
let wartosc_od_przedzialu a = (a, false)

(* wartosc_od_do: float -> float -> wartosc *)
let wartosc_od_do x y = wartosc_od_przedzialu (x, y)

(* wartosc_dokladna: float -> wartosc *)
let wartosc_dokladna x = wartosc_od_do x x

(* wartosc_dokladnosc: float -> float -> wartosc *)
let wartosc_dokladnosc x p = 
  if( x >= 0. ) then 
    wartosc_od_do
      (x *. (1. -. p /. 100.))
      (x *. (1. +. p /. 100.))
  else 
    wartosc_od_do
      (x *. (1. +. p /. 100.))
      (x *. (1. -. p /. 100.))

let wszystkie_rzeczywiste = wartosc_od_do neg_infinity infinity

let wartosc_nan = wartosc_dokladna nan

(* in_wartosc: wartosc -> float -> bool *)
let in_wartosc x y = 
  if (snd x) then 
    not (fst (fst x) < y && y < snd (fst x))
  else 
    (fst (fst x) <= y && y <= snd (fst x))

(* min_wartosc: wartosc -> float *)
let min_wartosc x = 
  if (snd x) then neg_infinity
  else fst (fst x)

(* max_wartosc: wartosc -> float *)
let max_wartosc x = 
  if (snd x) then infinity
  else snd (fst x)

(* sr_wartosc: wartosc -> float *)
let sr_wartosc x = 
  if (snd x) then nan
  else (fst (fst x) +. snd (fst x)) /. 2.

(* plus: wartosc -> wartosc -> wartosc *)
let rec plus x y = 
  if (wartosc_is_nan x || wartosc_is_nan y) then 
    wartosc_nan
  else
    match (x, y) with
    | ((a, true), (b, false)) ->
      if (snd a -. fst a < snd b -. fst b) then 
        wszystkie_rzeczywiste
      else 
        ((fst a +. snd b, snd a +. fst b), true)
    | ((a, false), (b, true)) -> plus y x
    | ((a, false), (b, false)) -> 
      wartosc_od_przedzialu (fst a +. fst b, snd a +. snd b)
    | ((_, true), (_, true)) -> wszystkie_rzeczywiste

(* B' = { -b : b \in B}. minus A B = {a - b | a \in A, b \in B} =
   = {a + b' | a  \in A, b' \in B'} = plus A B' *)
(* Linijka wyżej pozwala na zmniejszenie liczby przypadków *)

(* ujemny_przedzial: przedzial -> przedzial *)
let ujemny_przedzial a = ( -. snd a, -. fst a)

(* ujemny: wartosc -> wartosc *)
let ujemny x = (ujemny_przedzial (fst x), snd x)

(* minus: wartosc -> wartosc -> wartosc *)
let minus x y = plus x (ujemny y)

(* generuj_liste: przedzial -> przedzial -> float list *)
let generuj_liste a b = 
  [ 
    fst a /. fst b; fst a /. snd b; 
    snd a /. fst b; snd a /. snd b
  ]

(* Funkcja dostaje dwa przedziały i zwraca minimalny, 
   pojedyńczy przedzial, który je obejmuje *)
(* min_max: przedzial -> przedzial -> przedzial *)
let min_max a b =
  (* Funkcja dla danej liczby rzeczywistej, sprawdza czy znajduje się w przedziale, 
     jeśli nie, to go rozszerza *)
  (* porownaj_przedzial: float -> przedzial -> przedzial *)
  let porownaj_przedzial element przedzial =
    if is_nan element then przedzial
    else
      if przedzial = (infinity, neg_infinity) then 
        (element, element)
      else
        if element < fst przedzial then 
          (element, snd przedzial)
        else 
          if element > snd przedzial then
            (fst przedzial, element)
          else przedzial
  (* Funkcja dla danej listy liczb rzeczywistych, 
     tworzy przedział, który obejmuje je wszystkie *)
  (* _min_max: float list -> przedzial -> przedzial *)
  in let rec _min_max lst przedzial = 
    match lst with
    | [] ->
      if przedzial = (infinity, neg_infinity) then (nan, nan)
      else przedzial
    | head :: tail -> _min_max tail (porownaj_przedzial head przedzial)
  in _min_max (List.map zero_if_zero (generuj_liste a b)) (infinity, neg_infinity)


(* Dostajemy przedziały [neg_infinity, x] oraz [y, infinity] lub [x, y] oraz [z, t]. 
   Wynikiem jest wartosc, która jest ich sumą *)
(* polacz: przedzial -> przedzial -> wartosc *)
let polacz a b =
  if (snd a >= fst b) then 
    ((fst a, snd b), false)
  else 
    ((snd a, fst b), true)

(* odwrotnosc A = {1 / a | a \in A} => podzielic A B = odwrotnosc (podzielic B A) *)
(* Linijka wyżej pozwala na zmniejszenie liczby przypadków *)

(* odwrotnosc: wartosc -> wartosc *)
let odwrotnosc x =
  match (znak_wartosci x) with
  | Zero -> wartosc_nan
  | Dodatni | Ujemny | Nieujemny -> ((1. /. snd (fst x), 1. /. fst (fst x)), (snd x))
  | Niedodatni -> ((neg_infinity, 1. /. fst (fst x)), (snd x))
  | Oba -> 
    if (x = wszystkie_rzeczywiste) then 
      wszystkie_rzeczywiste
    else 
      ((1. /. fst (fst x), 1. /. snd (fst x)), not (snd x))


(* [x, y] / [z, t] *)
(* podzielic_jeden_jeden: przedzial -> przedzial -> wartosc *)
let rec podzielic_jeden_jeden a b =
  match (znak_przedzialu b) with
  | Oba -> 
    (
      match (znak_przedzialu a) with
      | Dodatni -> ((fst a /. fst b, fst a /. snd b), true)
      | Ujemny -> ujemny (podzielic_jeden_jeden (ujemny_przedzial a) b)
      | _ -> wszystkie_rzeczywiste
    )
  (* Oddzielnie rozważam ten przypadek, 
     ponieważ dzielę przez liczby dążące do -0. *)
  | Niedodatni -> wartosc_od_przedzialu (min_max a (fst b, -0.)) 
  | _ -> wartosc_od_przedzialu (min_max a b)


(* [x, y] / ([neg_infinity, z] U [t, infinity]) *)
(* podzielic_jeden_dwa: przedzial -> przedzial -> wartosc *)
let rec podzielic_jeden_dwa a b =
  match (znak_przedzialu a) with
  | Oba -> 
    (
      match (znak_przedzialu b) with
      | Oba -> wartosc_od_przedzialu (min_max a b)
      | _ -> wszystkie_rzeczywiste
    )
  | Dodatni | Nieujemny -> 
    (
      match (znak_przedzialu b) with
      | Oba -> wartosc_od_przedzialu (min_max a b)
      | Dodatni -> polacz 
        (neg_infinity, snd a /. snd b) 
        (fst a /. fst b, infinity)
      | Nieujemny -> wartosc_od_przedzialu (neg_infinity, snd a /. snd b)
      | Niedodatni | Ujemny -> ujemny (podzielic_jeden_dwa a (ujemny_przedzial b))
      | Zero -> wartosc_nan
    )
  | Ujemny | Niedodatni -> ujemny (podzielic_jeden_dwa (ujemny_przedzial a) b)
  | Zero -> wartosc_nan


(* podzielic: wartosc -> wartosc -> wartosc *)
let rec podzielic x y = 
  if (wartosc_is_nan x || wartosc_is_nan y) then 
    wartosc_nan
  else
    if (y = wartosc_dokladna 0.) then 
      wartosc_nan
    else
      if (x = wartosc_dokladna 0.) then 
        wartosc_dokladna 0.
      else 
        if (y = wszystkie_rzeczywiste) then 
          wszystkie_rzeczywiste
        else
          match (x, y) with
          | ((a, false), (b, false)) -> podzielic_jeden_jeden a b
          | ((a, false), (b, true)) -> podzielic_jeden_dwa a b
          | ((a, true), (b, false)) -> odwrotnosc (podzielic y x)
          | ((a, true), (b, true)) -> wszystkie_rzeczywiste
          
          
(* razy: wartosc -> wartosc -> wartosc *)
let razy x y =
  if (wartosc_is_nan x || wartosc_is_nan y) then 
    wartosc_nan
  else
    if (y = wartosc_dokladna 0. || x = wartosc_dokladna 0.) then 
      wartosc_dokladna 0.
    else
      if (y = wszystkie_rzeczywiste) then 
        wszystkie_rzeczywiste
      else 
        podzielic x (odwrotnosc y)

        
(* Testy pochodzą ze wspólnej puli testów *)

let a = wartosc_od_do (-1.) 1.            (* <-1, 1> *)
let b = wartosc_dokladna (-1.)            (* <-1, -1> *)
let c = podzielic b a                     (* (-inf -1> U <1 inf) *)
let d = plus c a                          (* (-inf, inf) *)
let e = wartosc_dokladna 0.               (* <0, 0> *)
let f = razy c e                          (* <0, 0> *)
let g = razy d e                          (* <0, 0> *)
let h = wartosc_dokladnosc (-10.) 50.     (* <-15, -5> *)
let i = podzielic h e                     (* nan, przedzial pusty*)
let j = wartosc_od_do (-6.) 5.            (* <-6, 5> *)
let k = razy j j                          (* <-30, 36> *)
let l = plus a b                          (* <-2, 0> *)
let m = razy b l                          (* <0, 2> *)
let n = podzielic l l                     (* <0, inf) *)
let o = podzielic l m                     (* (-inf, 0) *)
let p = razy o a                          (* (-inf, inf) *)
let q = plus n o                          (* (-inf, inf) *)
let r = minus n n                         (* (-inf, inf) *)
let s = wartosc_dokladnosc (-0.0001) 100. (* <-0.0002, 0> *)
let t = razy n s                          (* (-inf, 0) *)

assert ((min_wartosc c, max_wartosc c) = (neg_infinity, infinity));
assert (is_nan (sr_wartosc c) );
assert (not (in_wartosc c 0.));
assert ((in_wartosc c (-1.)) && (in_wartosc c (-100000.)) && (in_wartosc c 1.) && (in_wartosc c 100000.));
assert ((in_wartosc d 0.) && (in_wartosc d (-1.)) && (in_wartosc d (-100000.)) && (in_wartosc d 1.) && (in_wartosc d 100000.));
assert ((min_wartosc f, max_wartosc f, sr_wartosc f) = (0., 0., 0.));
assert ((min_wartosc g, max_wartosc g, sr_wartosc g) = (0., 0., 0.));
assert ((min_wartosc h, max_wartosc h, sr_wartosc h) = (-15., -5., -10.));
assert (is_nan (min_wartosc i) && is_nan (sr_wartosc i) && is_nan (max_wartosc i));
assert ((min_wartosc k, max_wartosc k, sr_wartosc k) = (-30., 36., 3.));
assert ((min_wartosc n, max_wartosc n, sr_wartosc n) = (0., infinity, infinity));
assert ((min_wartosc o, max_wartosc o, sr_wartosc o) = (neg_infinity, 0., neg_infinity));
assert ((min_wartosc p, max_wartosc p, is_nan (sr_wartosc p)) = (neg_infinity, infinity, true));
assert ((min_wartosc q, max_wartosc q, is_nan (sr_wartosc q)) = (neg_infinity, infinity, true));
assert ((min_wartosc r, max_wartosc r, is_nan (sr_wartosc r)) = (neg_infinity, infinity, true));
assert ((min_wartosc t, max_wartosc t, sr_wartosc t) = (neg_infinity, 0., neg_infinity))


let a = min_wartosc ( minus ( wartosc_dokladnosc (7.) (8.) ) ( podzielic ( minus ( wartosc_od_do (0.) (0.) ) ( plus ( wartosc_od_do (0.) (0.) ) ( wartosc_dokladna (3.) ) ) ) ( wartosc_od_do (-4.) (4.) ) ) ) ;;
assert (a = neg_infinity);;
let a = in_wartosc ( razy ( wartosc_od_do (-4.) (0.) ) ( razy ( wartosc_dokladna (-8.) ) ( wartosc_dokladna (-1.) ) ) ) (0.);;
assert (a = true);;
let a = sr_wartosc ( razy ( wartosc_dokladnosc (4.) (0.) ) ( wartosc_od_do (-1.) (0.) ) ) ;;
assert (a =. -2.);;
let a = max_wartosc ( plus ( wartosc_dokladnosc (-3.) (5.) ) ( wartosc_od_do (-9.) (9.) ) ) ;;
assert (a =. 6.15);;
let a = in_wartosc ( podzielic ( wartosc_dokladna (9.) ) ( wartosc_dokladnosc (-5.) (9.) ) ) (-10.);;
assert (a = false);;
let a = sr_wartosc ( plus ( minus ( wartosc_dokladnosc (-2.) (2.) ) ( wartosc_dokladna (6.) ) ) ( wartosc_dokladna (3.) ) ) ;;
assert (a =. -5.);;
let a = min_wartosc ( podzielic ( wartosc_dokladnosc (4.) (6.) ) ( wartosc_dokladnosc (7.) (2.) ) ) ;;
assert (a =. 0.526610644257703098);;
let a = in_wartosc ( razy ( wartosc_od_do (0.) (0.) ) ( wartosc_od_do (4.) (4.) ) ) (0.);;
assert (a = true);;
let a = min_wartosc ( podzielic ( wartosc_dokladna (3.) ) ( wartosc_od_do (-1.) (0.) ) ) ;;
assert (a = neg_infinity);;
let a = min_wartosc ( plus ( wartosc_dokladnosc (9.) (5.) ) ( wartosc_dokladna (9.) ) ) ;;
assert (a =. 17.55);;
let a = sr_wartosc ( podzielic ( wartosc_dokladna (0.) ) ( wartosc_dokladna (9.) ) ) ;;
assert (a =. 0.);;
let a = max_wartosc ( podzielic ( wartosc_od_do (0.) (6.) ) ( wartosc_dokladna (4.) ) ) ;;
assert (a =. 1.5);;
let a = in_wartosc ( plus ( minus ( wartosc_od_do (-1.) (4.) ) ( wartosc_dokladna (0.) ) ) ( podzielic ( wartosc_dokladna (0.) ) ( wartosc_od_do (0.) (5.) ) ) ) (9.);;
assert (a = false);;
let a = min_wartosc ( podzielic ( wartosc_od_do (-1.) (7.) ) ( wartosc_dokladnosc (0.) (1.) ) ) ;;
assert ((classify_float a) == FP_nan);;
let a = sr_wartosc ( plus ( wartosc_od_do (1.) (2.) ) ( wartosc_dokladna (5.) ) ) ;;
assert (a =. 6.5);;
let a = in_wartosc ( razy ( wartosc_od_do (-8.) (-1.) ) ( podzielic ( wartosc_dokladna (0.) ) ( wartosc_dokladnosc (-10.) (6.) ) ) ) (-8.);;
assert (a = false);;
let a = sr_wartosc ( podzielic ( wartosc_od_do (0.) (0.) ) ( podzielic ( wartosc_od_do (-6.) (2.) ) ( plus ( podzielic ( wartosc_dokladnosc (-2.) (0.) ) ( wartosc_dokladna (0.) ) ) ( plus ( wartosc_dokladna (-10.) ) ( plus ( wartosc_od_do (-7.) (7.) ) ( wartosc_od_do (-8.) (-1.) ) ) ) ) ) ) ;;
assert ((classify_float a) == FP_nan);;
let a = sr_wartosc ( razy ( podzielic ( plus ( wartosc_dokladnosc (-9.) (4.) ) ( wartosc_od_do (-9.) (3.) ) ) ( wartosc_od_do (-10.) (0.) ) ) ( wartosc_dokladnosc (0.) (5.) ) ) ;;
assert (a =. 0.);;
let a = max_wartosc ( podzielic ( wartosc_od_do (0.) (6.) ) ( minus ( wartosc_dokladna (-1.) ) ( wartosc_dokladna (-1.) ) ) ) ;;
assert ((classify_float a) == FP_nan);;
let a = max_wartosc ( plus ( wartosc_dokladna (-8.) ) ( wartosc_od_do (3.) (8.) ) ) ;;
assert (a =. 0.);;
let a = in_wartosc ( plus ( wartosc_dokladna (-4.) ) ( plus ( podzielic ( wartosc_od_do (-4.) (-1.) ) ( minus ( plus ( minus ( wartosc_dokladnosc (-4.) (6.) ) ( wartosc_dokladna (4.) ) ) ( wartosc_od_do (-3.) (7.) ) ) ( minus ( wartosc_dokladnosc (-3.) (4.) ) ( wartosc_od_do (0.) (1.) ) ) ) ) ( wartosc_dokladna (-5.) ) ) ) (1.);;
assert (a = true);;


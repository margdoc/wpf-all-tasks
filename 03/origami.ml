(* Mikołaj Grzebieluch *)
(* Review - Mateusz Masłowski *)


(* stała służąca do porównywania liczb *)
let eps = 1e-12


(* Punkt na płaszczyźnie, interpretowany jako wektor *)
type point = float * float


(* Poskładana kartka: ile razy kartkę przebije szpilka wbita w danym punkcie *)
type kartka = point -> int


(* operator porównójący, czy liczby są w przybliżeniu równe *)
(* float -> float -> bool *)
let (===) x y = abs_float(x -. y) <= eps


(* operator dodawania wektorów *)
(* point -> point -> point *)
let (#+) (x1, y1) (x2, y2) = (x1 +. x2, y1 +. y2)


(* operator odejmowania wektorów *)
(* point -> point -> point *)
let (#-) (x1, y1) (x2, y2) = (x1 -. x2, y1 -. y2)


(* [prostokat p1 p2] zwraca kartkę, reprezentującą domknięty prostokąt
    o bokach równoległych do osi układu współrzędnych i lewym dolnym rogu [p1]
    a prawym górnym [p2]. Punkt [p1] musi więc być nieostro na lewo i w dół
    od punktu [p2]. Gdy w kartkę tę wbije się szpilkę wewnątrz
    (lub na krawędziach) prostokąta, kartka zostanie przebita 1 raz,
    w pozostałych przypadkach 0 razy *)
(* point -> point -> point -> int *)
let prostokat (x1, y1) (x2, y2) (x, y) = 
  if x1 <= x && x <= x2 && y1 <= y && y <= y2 then 1 else 0


(* funkcja zwraca długość wektora podniesioną do kwadratu *)
(* point -> float *)
let lensqr (x, y) = x *. x +. y *. y  


(* [kolko p r] zwraca kartkę, reprezentującą kółko domknięte 
  o środku w punkcie [p] i promieniu [r] *)
(* point -> point -> point -> int *)
let kolko c r p =
  if r *. r >= lensqr (c #- p) then 1 else 0 


(* [odbij p1 p2] funkcja zwraca punkt (x',y') będący odbiciem punktu p2 względem 
    prostej przechodzącej przez punkt (0,0) oraz p1 zgodnie ze wzorem 
    (gdzie a, to współczynnik kierunkowej tej prostej oraz p2 = (x,y)):
    x' = (1-a^2)/(1+a^2)x + 2a/(1+a^2)y
    y' = (2a/(1+a^2)x - (1-a^2)/(1+a^2)y *)
(* point -> point -> point *)
let odbij (x1, y1) (x2, y2) =
  if x1 === 0. then (-.x2, y2)
  else let a = y1 /. x1 
  in let w = 1. +. a *. a
  and w1 = (1. -. a *. a) 
  and w2 = 2. *. a
  in ((w1 *. x2 +. w2 *. y2) /. w, (w2 *. x2 -. w1 *. y2) /. w)


(* funkcja zwraca wyznacznik macierzy
  |x1 y1|
  |x2 y2| *)
(* point -> point -> float *)
let det (x1, y1) (x2, y2) = x1 *. y2 -. x2 *. y1


(* [zloz p1 p2 k] składa kartkę [k] wzdłuż prostej przechodzącej przez
    punkty [p1] i [p2] (muszą to być różne punkty). Papier jest składany
    w ten sposób, że z prawej strony prostej (patrząc w kierunku od [p1] do [p2])
    jest przekładany na lewą. Wynikiem funkcji jest złożona kartka. Jej
    przebicie po prawej stronie prostej powinno więc zwrócić 0.
    Przebicie dokładnie na prostej powinno zwrócić tyle samo,
    co przebicie kartki przed złożeniem. Po stronie lewej -
    tyle co przed złożeniem plus przebicie rozłożonej kartki w punkcie,
    który nałożył się na punkt przebicia. *)
(* point -> point -> kartka -> kartka *)
let zloz p1 p2 f p = 
  match det (p #- p1) (p2 #- p1) with
  | d when d === 0. -> f p
  | d when d > 0. -> 0
  | _ -> f p + f (p1 #+ (odbij (p2 #- p1) (p #- p1)))


(* [skladaj [(p1_1,p2_1);...;(p1_n,p2_n)] k = 
    zloz p1_n p2_n (zloz ... (zloz p1_1 p2_1 k)...)]
    czyli wynikiem jest złożenie kartki [k] kolejno wzdłuż wszystkich prostych
    z listy *) 
(* (point * point) list -> kartka -> kartka *)
let skladaj lst f = 
  List.fold_left (fun acc (p1, p2) -> zloz p1 p2 acc) f lst


(* Testy pochodzą ze wspólnej puli testów *)
(*
let test a b msg = if a<>b then (print_int a; print_string "<>"; print_int b; print_string " test: "; print_endline msg);;

let p1 = prostokat (0., 0.) (10., 10.)
let k1 = kolko (5., 5.) 5.
let l1 = [((0., 0.), (10., 10.));
	  ((5., 0.), (10., 5.));
	  ((10., 0.), (0., 10.));
	  ((2.5, 0.), (2.5, 10.))];;
let l2 = [((8., 0.), (10., 2.));
	  ((6., 0.), (10., 4.));
	  ((4., 0.), (10., 6.));
	  ((2., 0.), (10., 8.));
	  ((0., 0.), (10., 10.));
	  ((0., 2.), (8., 10.));
	  ((0., 4.), (6., 10.));
	  ((0., 6.), (4., 10.));
	  ((0., 8.), (2., 10.))];;

let p2 = skladaj l1 p1
let p3 = skladaj l2 p1
let k2 = skladaj l1 k1;;

test (p2 (7., 3.)) 0 "0.1: p2";;
test (p2 (5., 8.)) 0 "0.2: p2";;
test (p2 (3., 5.)) 0 "0.3: p2";;
test (p2 (5., 5.)) 0 "0.4: p2";;
test (p2 (0., 0.)) 2 "1: p2";;
test (p2 (0., 10.)) 2  "2: p2";;
test (p2 (2.5, 2.5)) 2 "3: p2";;
test (p2 (2.5, 7.5)) 2 "4: p2";;
test (p2 (2.5, 5.)) 4 "5: p2";;
test (p2 (0., 5.)) 5 "6: p2";;
test (p2 (1., 2.)) 4 "7: p2";;
test (p2 (1., 5.)) 8 "8: p2";;
test (p2 (1., 8.)) 4 "9: p2";;

test (k2 (7., 3.)) 0 "0.1: k2";;
test (k2 (5., 8.)) 0 "0.2: k2";;
test (k2 (3., 5.)) 0 "0.3: k2";;
test (k2 (5., 5.)) 0 "0.4: k2";;
test (k2 (2.5, 2.5)) 2 "1: k2";;
test (k2 (2.5, 7.5)) 2 "2: k2";;
test (k2 (2.5, 5.)) 4 "3: k2";;
test (k2 (0., 5.)) 5 "4: k2";;
test (k2 (1., 3.)) 4 "5: k2";;
test (k2 (1., 5.)) 8 "6: k2";;
test (k2 (1., 7.)) 4 "7: k2";;

test (p3 ((-4.), 6.)) 2 "1: p3";;
test (p3 ((-3.), 5.)) 1 "2: p3";;
test (p3 ((-3.), 7.)) 2 "3: p3";;
test (p3 ((-2.), 6.)) 3 "4: p3";;
test (p3 ((-2.5), 6.5)) 4 "5: p3";;
test (p3 ((-2.), 8.)) 4 "6: p3";;
test (p3 ((-1.), 7.)) 3 "7: p3";;
test (p3 ((-1.5), 7.5)) 6 "8: p3";;
test (p3 (0., 8.)) 5 "9: p3";;
test (p3 ((-1.), 9.)) 4 "10: p3";;
test (p3 ((-0.5), 8.5)) 8 "11: p3";;
test (p3 (0., 10.)) 6 "12: p3";;
test (p3 (1., 9.)) 5 "13: p3";;
test (p3 (0.5, 9.5)) 10 "14: p3";;
*)

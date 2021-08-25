(** useful list primitives **)
(* remove an element *)
let remove x = List.filter (fun y-> y<>x)

(* union of sorted lists *)
let rec fus l1 l2 = match (l1,l2) with
    [],l2 -> List.length l2
   |l1,[] -> List.length l1
   |t::q,t2::q2 when t=t2 -> 1+fus q q2
   |t::q,t2::q2 when t>t2 -> 1+fus l1 q2
   |t::q,t2::q2 -> 1+fus q l2

(* duplicate removal in a sorted list *)                
let rec doublons = function
    []->[]
   |t::t2::q when t=t2 -> doublons (t2::q)
   |t::q -> t::doublons q

(* testing if k can be obtained from l by removing some elements *)
let rec sublist k l= match k,l with
    |([],_)->true
    |(_::_,[])->false
    |(h1::t1,h2::t2)->if h1=h2 then sublist t1 t2 else sublist (h1::t1) t2

(* ?? *)
let rec clean=function
  |[]->[]
  |h::[]->h::[]
  |h::t->aux h [] t
and aux k acc =function
  |[]->k::(clean acc)
  |h::t-> if sublist k h then clean(acc@(h::t)) else if sublist h k then (aux k acc t) else (aux k (h::acc) t)

(* compl i n l returns [i,n]\l *)
let rec compl i n = function
    []-> if i <n then i::compl (i+1) n [] else []
   |t::q when i<t -> i::compl (i+1) n (t::q)
   |t::q -> compl (i+1) n q

(* splits a list according to a predicate p *)
let rec partition acc1 acc2 p = function
    [] -> (acc1,acc2)
   |t::q when p t -> partition (t::acc1) acc2 p q
   |t::q  -> partition acc1 (t::acc2) p q


(** tournaments are given by list of successors or list of arcs **)
(* printing tournament as list of arcs *)
let print v =
  Array.iteri (fun i x -> List.iter (fun y-> Printf.printf "(%d,%d)," i y) x) v;
  Printf.printf "\n%!"

(* conversion functions *)
let convert l n=
  let v= Array.init n (fun x->[]) in
  List.iter (fun (a,b) -> v.(a) <- b::v.(a)) l;
  Array.map (List.sort compare) v

let deconvert v =
  Array.mapi (fun i x -> List.map (fun y -> (i,y)) x) v |> Array.to_list |> List.concat

(* extracting from integer representation *)
let from_int k =
  let res = ref [] in
  for i = 0 to 7 do
    for j = i+1 to 7 do
      if (k lsr (i*8+j)) mod 2 = 1 then 
        res := (i,j)::!res
      else
        res := (j,i)::!res
    done;
  done;
  !res


(** chromatic number **)
(* generating all inclusion maximal transitive subtournaments with v as source *)
let ttmax tournoi v =
  let rec aux tournoi acc prov ismaxprov outn=function
    |[]->if ismaxprov then prov::acc else acc
    |h::t->let newoutn=List.filter (function k->List.mem k (tournoi.(h))) outn in (aux tournoi acc (h::prov) true newoutn newoutn)@(aux tournoi acc prov false outn t) in
  clean(aux tournoi [] [v] true tournoi.(v) tournoi.(v)) |> List.map List.rev

(* generating all inclusion maximal transitive subtournaments *)
let allttmax g = let res = ref [] in
                 for i = 0 to Array.length g-1 do
                   res := !res @ ttmax g i
                 done;
                 clean !res

(* updates a list of inclusion maximal transitive subtournaments when removing vertices *)
let removett vlist ttlist =
  clean (List.map (List.filter (fun x -> not (List.mem x vlist))) ttlist)

(* testing whether there are two disjoint TT5 *)
let rec has2tt5 = function
    []-> false
  |t::q -> List.exists (fun t' -> fus t t' >= 10) q || has2tt5 q


(* color l k = true iff the graph whose ttmax are l is k-colorable *)
let rec color ltt depth =
  if depth = 0 then ltt = [[]]
  else ltt = [[]] || List.exists (fun tt -> color (removett tt ltt) (depth-1)) ltt

(* nice hack to speed up when testing multiple graphs *)
let color_malin n all_tt new_tt=
  List.exists (fun tt -> color (removett tt all_tt) (n-1)) new_tt 

(** generation of 13-completions **)
(* importing the 3-chromatic TT5-free tournaments on 8 vertices *)
let t8_3col =
  let chan = open_in "tournoisX8not3col" in
  let l = ref [] in 
  for i = 0 to 257 do
    let t = from_int (int_of_string (input_line chan)) in
    if not (List.exists (fun tt -> List.length tt = 5) (allttmax (convert t 8))) then
      l:= t :: !l
  done;
  close_in chan;
  !l

(* auxiliary functions *)  
let rec f1ijx tournoi j=function
  |[]->[[]]
  |h::t->if List.mem h tournoi.(j) then List.map (function l -> h::l) (f1ijx tournoi j t) else f1ijx tournoi j t;;

let rec f1ixj tournoi j=function
  |[]->[[j]]
  |h::t when List.mem j tournoi.(h) -> List.map (function l -> h::l) (f1ixj tournoi j t)
  |h::t when List.mem h tournoi.(j) -> (f1ixj tournoi j t)@(List.map (function l -> j::h::l) (f1ijx tournoi j t))
  |_::t->f1ixj tournoi j t

let rec f1xij tournoi i j=function
  |[]->failwith "wtf"
  |h::t when h=i->List.map (function l-> i::l) (f1ixj tournoi j t)
  |h::t->if List.mem j tournoi.(h) then List.map (function l -> h::l) (f1xij tournoi i j t) else f1ixj tournoi j t;;


let rec f2ixj tournoi i j=function
  |[]->failwith "wtf"
  |h::t when h=j -> List.map (function l -> j::l) (f1ijx tournoi i t)
  |h::t -> if List.mem h tournoi.(i) then List.map (function l -> h::l) (f2ixj tournoi i j t) else f2ixj tournoi i j t;;

let rec f2xyij tournoi i j=function
  |[]->failwith "wtf"
  |h::t when h=j -> []
  |h::t when List.mem i tournoi.(h) -> List.map (function l -> h::l) (f2xij tournoi i j t)
  |_::t->f2xyij tournoi i j t
and f2xij tournoi i j=function
  |[]->failwith "wtf"
  |h::t when h=j -> List.map (function l -> i::j::l)(f1ijx tournoi i t)
  |h::t when List.mem i tournoi.(h) -> List.map (function l -> h::l) (f2xij tournoi i j t)
  |h::t when List.mem h tournoi.(i) -> (f2xyij tournoi i j t)@(List.map (function l -> i::h::l)(f2ixj tournoi i j t))
  |_::t->f2xij tournoi i j t

(* updating the list of maxtt when removing an arc *)
let rec newttmax tournoi i j = function
  |[]->[]
  |h::t when List.mem i h -> (f1xij tournoi i j h)@(newttmax tournoi i j t)
  |h::t when List.mem j h -> (f2xij tournoi i j h)@(newttmax tournoi i j t)
  |_::t->newttmax tournoi i j t

(* main routine for computing 4-chromatic 2TT5-free 13-completions *)
let completions t8 =
  let res = ref [] in
  let rec test_compl digons t13 tt = match digons with
  |[]-> print t13; res:= Array.copy t13 :: !res
  |(a,b)::q ->
    t13.(a) <- b::t13.(a);
    let l = clean(newttmax t13 a b tt) in
    let all_tt = clean(l@tt) in (* 2TT5 free ?*)
    if List.for_all (fun x-> List.length x<7) l && not (has2tt5 (List.map (List.sort compare) (List.filter (fun x-> List.length x>=5) all_tt))) && not (color_malin 3 all_tt l) then
      test_compl q t13 all_tt;
    t13.(a) <- List.tl t13.(a);
    if a < b then
      test_compl ((b,a)::q) t13 tt
  in
  test_compl
    (List.map (fun x-> List.map (fun y-> (y,x)) [0;1;2;3;4]) [5;6;7;8;9;10;11;12] |> List.concat)
    (convert ([(0,1);(0,2);(0,3);(0,4);(1,2);(1,3);(1,4);(2,3);(2,4);(3,4)]@(List.map (fun (a,b) -> (a+5,b+5)) t8)) 13)
    ([0;1;2;3;4]::(List.map (List.map (fun a-> a+5)) (allttmax (convert t8 8))));
  !res


(** type primitives **)
(* testing if the subgraph induced by the vertices in lv induce a transitive subtournament of g *)
exception Break

let is_subtrans g lv =
  let v = Array.of_list lv in
  try
    for i = 0 to Array.length v-1 do
      for j = i+1 to Array.length v-1 do
        for k = j+1 to Array.length v-1 do
          if (List.mem v.(j) g.(v.(i)) && List.mem v.(k) g.(v.(j)) && List.mem v.(i) g.(v.(k))) || (List.mem v.(i) g.(v.(j)) && List.mem v.(j) g.(v.(k)) && List.mem v.(k) g.(v.(i))) then
            raise Break
        done;
      done;
    done;
    true
  with Break -> false


(* generate all k-element subsets of [0,n-1] *)
let rec parts_aux taken nottaken i k n =
  if i > n then [[]]
  else if taken = k then [[]]
  else if nottaken = n-k then List.rev_map (fun x -> i::x) (parts_aux (taken+1) nottaken (i+1) k n)
  else List.rev_append (List.rev_map (fun x-> i::x) (parts_aux (taken+1) nottaken (i+1) k n)) (parts_aux taken (nottaken+1) (i+1) k n)

let parts k n =
  parts_aux 0 0 1 k n

(* generate all partitions of g into two non-transitive subtournaments *)
let splittings  g =
  (parts 3 8)@(parts 4 8)
  |> List.map (List.map (fun x-> x-1))
  |> List.map (fun x-> [x;compl 0 8 x])
  |> List.filter (List.for_all (fun p -> not (is_subtrans g p)))
  |> List.concat
  |> List.sort (compare)
  |> doublons 
  
(* relabeling outneighbors as an auxiliary function for restrict *)
let rec restrict_nbr nbr lv i = match (nbr,lv) with
    [],_-> []
   |_,[]-> []
   |t::q,t2::q2 when t=t2 -> i::restrict_nbr q q2 (i+1)
   |t::q,t2::q2 when t<t2 -> restrict_nbr q lv i
   |t::q,t2::q2 -> restrict_nbr nbr q2 (i+1)

(* computes the subtournament induced by l in g, and relabels from 0 to |l|-1 *)
let restrict g lv =
  let res = Array.make (List.length lv) [] in
  List.iteri (fun i x-> res.(i) <- restrict_nbr (List.sort compare g.(x)) lv 0) lv;
  res

(* computes the type of g *)
let typ g l =
  List.filter (fun x-> color (allttmax (restrict g ([0;1;2;3;4]@(List.map (fun y-> y+5) x)))) 2) l

(* given a list of types, computes the pairs of incompatible types in it *)
let rec incomp_aux l1 l2 = match l1 with
    [] -> []
   |[]::q -> List.rev_append (List.rev_map (fun x-> ([],x)) l2) (incomp_aux q l2)
   |(t::q)::q' ->
     let (hast,hasnott) = partition [][](List.mem t) l1 in 
     let tbar = compl 0 8 t in
     let (hastbar, hasnottbar) = partition [][] (List.mem tbar) l2 in
     List.rev_append(List.rev_append (List.rev_map (fun (a,b) -> (t::a,b)) (incomp_aux (List.rev_map (remove t) hast) hasnottbar))
     (List.rev_map (fun (a,b) -> (a,tbar::b)) (incomp_aux hasnott (List.rev_map (remove tbar) hastbar) )))
     (incomp_aux hasnott hasnottbar)

let incomp t =
  incomp_aux t t |> List.rev_map (fun (a,b) -> (min a b,max a b))
  |> List.sort compare |> doublons 

(* given a sorted list of tournaments with their type, and a sorted list of incompatible types, outputs the list of all pairs of completions with incompatible types *)
let rec recomb_aux ct ct2 itypes = match (ct,ct2,itypes) with
  | _,_,[] -> []
  | [],_,_ -> []
  |t1::q1,t2::q2,(a,b)::q when a > fst t1 -> assert (t1=t2); recomb_aux q1 q2 ((a,b)::q)
  |t1::q1,t2::q2,(a,b)::q when a = fst t1 && b > fst t2 -> recomb_aux (t1::q1) q2 ((a,b)::q)
  |t1::q1,t2::q2,(a,b)::q when a = fst t1 && b = fst t2 -> (snd t1,snd t2)::recomb_aux (t1::q1) q2 ((a,b)::q)
  |t1::q1,t2::q2,(a,b)::q when a = fst t1 && b < fst t2 -> recomb_aux (t1::q1) (t2::q2) q
  |t1::q1,_,(a,b)::q  -> recomb_aux q1 q1 ((a,b)::q)

let recomb ct itypes = recomb_aux ct ct itypes

(** generation of the 18-vertex graphs to test *)
(* identifies the vertices 5 to 12 in both graphs, relabels 0..4 into 13..17 in g2 *)
let glue g1 g2 =
  let g = g2
  |> List.filter (fun (a,b) -> a <5 || b<5)
  |> List.map (fun (a,b) -> (if a <5 then a+13 else a), (if b<5 then b+13 else b))
  in g1@g

(* updates the list of maximum transitive subtournaments in the gluing *)
let process g1 g2 =
  let g = glue g1 g2 in
  let tt1 = allttmax (convert g1 13) and tt2 = List.map (List.map (fun a -> if a <5 then a+13 else a)) (allttmax(convert g2 13)) in
  (convert g 18,tt1@(List.filter (fun x-> not (List.mem x tt1)) tt2)) 


(* trying all orientations of the 25 remaining arcs *)
(* assert is triggered when finding a 5-chromatic tournament *)
let rec aux digons t18 tt = match digons with
  |[]-> print t18; assert(false) 
  |(a,b)::q ->
    t18.(a) <- b::t18.(a);
    let l = clean(newttmax t18 a b tt) in
    let all_tt = clean(l@tt) in 
    if not (color_malin 4 all_tt l) then
      aux q t18 all_tt;
    t18.(a) <- List.tl t18.(a);
    if a < b then
      aux ((b,a)::q) t18 tt

let test_gluing g1 g2 =
  let (g,tt) = process g1 g2 in
  let digons = List.map (fun x-> List.map (fun y-> (y,x)) [0;1;2;3;4]) [13;14;15;16;17] |> List.concat in
  aux digons g tt

(* wrapping up *)
let test index g =
  Printf.printf "------ Graph %d/%d ------\n%!" index (List.length t8_3col);
  let cg = completions g in
  Printf.printf "%d completions\n\n%!" (List.length cg);
  List.iter print cg;  
  Printf.printf "------ Testing completions ------\n%!";
  let spl = splittings (convert g 8) in
  let ct = List.sort compare (List.rev_map (fun t-> (typ t spl,t)) cg) in
  let types = doublons (List.map fst ct) in
  Printf.printf "%d types\n%!" (List.length types);
  let itypes = incomp types in
  Printf.printf "%d pairs of incompatible types\n%!" (List.length itypes); 
  let ic = recomb ct itypes in
  Printf.printf "%d pairs of incompatible completions\n%!" (List.length ic);
  let i = ref 0 in
  List.iter (fun (a,b) -> incr i; Printf.printf "------ Gluing %d/%d ------\n%!" !i (List.length ic); test_gluing (deconvert a) (deconvert b)) ic;
  Printf.printf "------ DONE ------\n"


(* main *)
let _ = List.iteri test t8_3col

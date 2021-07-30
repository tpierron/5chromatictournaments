(* useful list primitives *)
(* intersection of sorted lists *)
let rec inter a b = match (a,b) with
    [],_ -> []
   |_,[] -> []
   |t1::q1, t2::q2 when t1 < t2 -> inter q1 (t2::q2)
   |t1::q1, t2::q2 when t1 > t2 -> inter (t1::q1) q2
   |t1::q1, t2::q2 -> t1::inter q1 q2

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


(* tournaments are given by list of successors or list of arcs *)
(* conversion functions *)

let convert l n=
  let v= Array.init n (fun x->[]) in
  List.iter (fun (a,b) -> v.(a) <- b::v.(a)) l;
  Array.map (List.sort compare) v

let deconvert v =
  Array.mapi (fun i x -> List.map (fun y -> (i,y)) x) v |> Array.to_list |> List.concat

(* the four 3-chromatic tournaments on 7 vertices *)
let p7 = Array.init 7 (fun i -> [1;2;3] |> List.map (fun x -> (x*x+i) mod 7))
let w1 = [|[1;2;3];[2;4;5;6];[3;4;6];[1;4;5];[0;5];[0;2;6];[0;3;4]|]
let w0 = [|[1;2;3];[2;5;6];[3;4;6];[1;4;5];[0;1;5];[0;2;6];[0;3;4]|]
let w =  [|[1;3;5];[2;4;5;6];[0;6];[1;2;4;6];[0;2];[2;3;4;6];[0;4]|]


(* generating all inclusion maximal transitive subtournaments with v as source *)
let ttmax tournoi v =
  let rec aux tournoi acc prov ismaxprov outn=function
    |[]->if ismaxprov then prov::acc else acc
    |h::t->let newoutn=List.filter (function k->List.mem k (tournoi.(h))) outn in (aux tournoi acc (h::prov) true newoutn newoutn)@(aux tournoi acc prov false outn t) in

  clean(aux tournoi [] [v] true tournoi.(v) tournoi.(v)) |> List.map List.rev;;

(* generating all inclusion maximal transitive subtournaments *)
let allttmax g = let res = ref [] in
                 for i = 0 to Array.length g-1 do
                   res := !res @ ttmax g i
                 done;
                 clean !res

(* updates a list of inclusion maximal transitive subtournaments when removing vertices *)
let removett vlist ttlist =
  clean (List.map (List.filter (fun x -> not (List.mem x vlist))) ttlist)

(* color l k = true iff the graph whose ttmax are l is k-colorable *)
let rec color ltt depth =
  if depth = 0 then ltt = [[]]
  else ltt = [[]] || List.exists (fun tt -> color (removett tt ltt) (depth-1)) ltt

(* nice hack to speed up when testing multiple graphs *)
let color3_malin all_tt new_tt=
  List.exists (fun tt -> color (removett tt all_tt) 2) new_tt 


(* testing if P11 is a subgraph of t *)
let containsP11 t =
  let tt = allttmax t in
  List.exists (fun x -> 
      not (color (clean (removett [x] tt)) 3)
    ) [0;1;2;3;4]


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

(* main routine for computing completions and testing them on the fly*)
let rec test_compl digons tournoi tt = match digons with
  |[]-> assert (containsP11 tournoi) 
  |(a,b)::q ->
    tournoi.(a) <- b::tournoi.(a);
    let l = clean(newttmax tournoi a b tt) in
    let all_tt = clean(l@tt) in
    if not (color3_malin all_tt l) then
      test_compl q tournoi all_tt;
    tournoi.(a) <- List.tl tournoi.(a);
    if a < b then
      test_compl ((b,a)::q) tournoi tt

(* wrapping up to test the 4-chromatic tournaments obtained by gluing t with a tt5 *)
let test t =
  test_compl
    (List.map (fun x-> List.map (fun y-> (y,x)) [0;1;2;3;4]) [5;6;7;8;9;10;11] |> List.concat)
    (convert ([(0,1);(0,2);(0,3);(0,4);(1,2);(1,3);(1,4);(2,3);(2,4);(3,4)]@(List.map (fun (a,b) -> (a+5,b+5)) (deconvert t))) 12)
    ([0;1;2;3;4]::(List.map (List.map (fun a-> a+5)) (allttmax t)))

(* if no assert is triggered, every completion contains P11 *)
let _ =
  test w0;
  test p7;
  test w;
  test w1

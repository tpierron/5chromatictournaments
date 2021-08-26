(* global variable *)
let progress = ref 0

(** useful list primitives **)
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

(* assoc [l1;...;ln] = [[x1;...;xn] for xi in li] *)
let rec assoc = function
    []-> [[]]
  |t::q-> let q' = assoc q in List.rev_map (fun x -> List.rev_map (fun y -> x::y) q') t |> List.concat

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

(* relabeling of vertices *)
let permute t v = List.map (fun (a,b) -> v.(a),v.(b)) t

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

(* color l k = true iff the graph whose ttmax are l is k-colorable *)
let rec color ltt depth =
  if depth = 0 then ltt = [[]]
  else ltt = [[]] || List.exists (fun tt -> color (removett tt ltt) (depth-1)) ltt

(* nice hack to speed up when testing multiple graphs *)
let color_malin n all_tt new_tt=
  List.exists (fun tt -> color (removett tt all_tt) (n-1)) new_tt 

(** generation of the 8-completions **)
(* importing the 3-chromatic tournaments on 8 vertices containing TT5 *)
let t8_3col =
  let chan = open_in "tournoisX8not3col" in
  let l = ref [] in 
  for i = 0 to 257 do
    let t = from_int (int_of_string (input_line chan)) in
    if List.exists (fun tt -> List.length tt = 5) (allttmax (convert t 8)) then
      l:= t :: !l
  done;
  close_in chan;
  !l
(* filtering out isomorphic completions *)
let rec filtr = function
    []->[]
   |t::q -> t::filtr (List.filter (fun t' -> t' <> t && t' <> List.sort compare (permute t [|0;1;2;3;4;6;7;5|]) && t' <> List.sort compare (permute t [|0;1;2;3;4;7;5;6|])) q)

(* generating the non isomorphic 8-completions *)
let x8fixedTT5 =
  t8_3col
  |> List.map (fun y-> List.map (fun t-> (y,t)) (List.filter (fun tt-> List.length tt = 5) (allttmax (convert y 8))))
  |> List.concat
  |> List.map (fun (t,tt) ->
         let l = tt@(compl 0 8 (List.sort compare tt)) in
         let v = Array.make 8 0 in
         List.iteri (fun i x -> v.(x) <- i) l;
         let a = permute t v in
         if List.mem (6,5) a then permute a [|0;1;2;3;4;6;5;7|] else a
       )
  |> List.map (List.sort compare)
  |> filtr
  |> Array.of_list

(* there are 256 8-completions *)
let n = Array.length x8fixedTT5


(** generation of the 13-completions **)
(* identifies the vertices 5,6,7 in both graphs, relabels 0..4 into 8..12 in g2 *)
let glue g1 g2 =
  let g = g2
          |> List.filter (fun (a,b) -> a <5 || b<5)
          |> List.map (fun (a,b) -> (if a <5 then a+8 else a), (if b<5 then b+8 else b))
  in g1@g

(* updates the list of maximum transitive subtournaments in the gluing *)
let process g1 g2 =
  let g = glue g1 g2 in
  let tt1 = allttmax (convert g1 8) and tt2 = List.map (List.map (fun a -> if a <5 then a+8 else a)) (allttmax(convert g2 8)) in
  (convert g 13,tt1@(List.filter (fun x-> not (List.mem x tt1)) tt2)) 

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
       
(* generation of the 13-completions *) 
let completions13 t8 t8' =
  let res = ref [] in
  let rec test_compl digons t13 tt = match digons with
  |[]-> print t13; res:= Array.copy t13 :: !res
  |(a,b)::q ->
    t13.(a) <- b::t13.(a);
    let l = clean(newttmax t13 a b tt) in
    let all_tt = clean(l@tt) in 
    if not (color_malin 3 all_tt l) then
      test_compl q t13 all_tt;
    t13.(a) <- List.tl t13.(a);
    if a < b then
      test_compl ((b,a)::q) t13 tt
  in
  let (g,tt) = process t8 t8' in
  let digons = List.map (fun x-> List.map (fun y-> (y,x)) [0;1;2;3;4]) [8;9;10;11;12] |> List.concat in
  test_compl digons g tt;
  !res

(* some memoization *)
(* all13compl.(i).(j) will store all 13-completions of the i-th and j-th 8-completions *)
let all13compl = Array.make_matrix n n []

(* filling all13compl.(i).(j) if non empty *)
(* note that there are three ways of identifying the directed triangles of the two 8-completions, hence all13compl.(i).(j), when non-empty, will contain three lists *)
let fill i j =
  if all13compl.(i).(j) = [] then begin
      Printf.printf "Start %d %d:\n%!" i j;
      let g1 = x8fixedTT5.(i) in
      let g2 = x8fixedTT5.(j) in
      let res = [completions13 g1 g2; completions13 g1 (permute g2 [|0;1;2;3;4;6;7;5|]) ;completions13 g1 (permute g2 [|0;1;2;3;4;7;5;6|])]  in
      Printf.printf "End %d %d: "i j;
      List.iter (fun x -> Printf.printf "%d," (List.length x)) res;
      let a = (List.fold_left (fun x y -> x+List.length y) 0 res) in
      Printf.printf "=%d completions found\n" a;
      List.iter (List.iter print) res;
      all13compl.(i).(j)<- res;
      incr progress;
      Printf.printf "Progression = %d cases/32896 = %f%%\n" !progress ((float_of_int !progress)*.100./.32896.)
    end

(** generating candidates for T **)
(* gluing three 13-completions *)   
let tripleglue tij tik tjk =
  tij
  @(tik |> List.filter (fun (a,b) -> a >7 || b>7)
    |> List.map (fun (a,b) -> (if a >7 then a+5 else a), (if b>7 then b+5 else b)))
  @(tjk |> List.filter (fun (a,b) -> (a <5 && b >7) || (a >7 && b<5))
    |> List.map (fun (a,b) -> (if a <5 then a+8 else if a>7 then a+5 else a), (if b<5 then b+8 else if b>7 then b+5 else b)))

(* testing all 18-vertex tournaments obtained from the i-th, j-th and k-th 8-completions *)
let test (i,j,k) =
  Printf.printf "Begin test (%d,%d,%d)\n%!" i j k;
  let ij = Array.of_list all13compl.(i).(j) in
  let ik = Array.of_list all13compl.(i).(k) in
  let jk = Array.of_list all13compl.(j).(k) in 
  let l = ((assoc [ij.(0);ik.(0);jk.(0)])
           @(assoc [ij.(0);ik.(1);jk.(1)])
           @(assoc [ij.(0);ik.(2);jk.(2)])
           @(assoc [ij.(1);ik.(0);jk.(2)])
           @(assoc [ij.(1);ik.(1);jk.(0)])
           @(assoc [ij.(1);ik.(2);jk.(1)])
           @(assoc [ij.(2);ik.(0);jk.(1)])
           @(assoc [ij.(2);ik.(1);jk.(2)])
           @(assoc [ij.(2);ik.(2);jk.(0)])
          )|> List.rev_map (function
                  | [a;b;c] -> convert (tripleglue (deconvert a) (deconvert b) (deconvert c)) 18
                  | _ -> failwith "test function")
          |> List.filter (fun t-> not(color (allttmax t) 4)) in
  List.iter print l;
  Printf.printf "End test (%d,%d,%d), %d graphs found\n%!" i j k (List.length l);
  List.length l

(* wrapping up *)
(* For each i, we compute all the 13-completions (i,j) for j>=i.
   Then for each j<=k such that there is an (i,j)-completion and an (i,k)-completion, 
   we compute the (j,k)-completions.
   Finally we test the 18-vertex tournaments obtained from i,j,k.
   When the i-th 8-completion is done, we free the memory. *)
let _ =
  for i = 0 to n-1 do 
    let nep = ref 0 and  compl = ref 0 and final = ref 0 in
    Printf.printf "Begin Graph %d\n%!" i;
    for j = i to n-1 do
      fill i j;
      if all13compl.(i).(j) <> [[];[];[]] then incr nep;
    done;
    Printf.printf "Testing completions of graph %d\n%!" i;
    for j=i to n-1 do
      if all13compl.(i).(j) <> [[];[];[]] then 
        for k=j to n-1 do
          if all13compl.(i).(k) <> [[];[];[]] then begin
              fill j k;
              if all13compl.(j).(k) <> [[];[];[]] then 
                final := !final + test (i,j,k)
            end;
        done;
    done;
    all13compl.(i) <- [||];
    Printf.printf "End Graph %d : %d non empty pairs, %d 13-completions, %d 18-completions\n%!" i !nep !compl !final;
  done
 

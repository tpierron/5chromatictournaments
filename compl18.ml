exception Break

let convert l n=
  let v= Array.init n (fun x->[]) in
  List.iter (fun (a,b) -> v.(a) <- b::v.(a)) l;
  Array.map (List.sort compare) v

let deconvert v =
  Array.mapi (fun i x -> List.map (fun y -> (i,y)) x) v |> Array.to_list |> List.concat

let from_entier k =
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

let rec parties_aux pris paspris i k n =
  if i > n then [[]]
  else if pris = k then [[]]
  else if paspris = n-k then List.rev_map (fun x -> i::x) (parties_aux (pris+1) paspris (i+1) k n)
  else List.rev_append (List.rev_map (fun x-> i::x) (parties_aux (pris+1) paspris (i+1) k n)) (parties_aux pris (paspris+1) (i+1) k n)

let parties k n =
  parties_aux 0 0 1 k n

let sublist k l=
  let rec aux l1 l2=match (l1,l2) with
    |([],_)->true
    |(_::_,[])->false
    |(h1::t1,h2::t2)->if h1=h2 then aux t1 t2 else aux (h1::t1) t2
  in aux k l;;


let rec clean=function
  |[]->[]
  |h::[]->h::[]
  |h::t->aux h [] t
and aux k acc =function
  |[]->k::(clean acc)
  |h::t-> if sublist k h then clean(acc@(h::t)) else if sublist h k then (aux k acc t) else (aux k (h::acc) t);;

let ttmax tournoi v =
  let rec aux tournoi acc prov ismaxprov outn=function
    |[]->if ismaxprov then prov::acc else acc
    |h::t->let newoutn=List.filter (function k->List.mem k (tournoi.(h))) outn in (aux tournoi acc (h::prov) true newoutn newoutn)@(aux tournoi acc prov false outn t) in

  clean(aux tournoi [] [v] true tournoi.(v) tournoi.(v)) |> List.map List.rev;;

let allttmax g = let res = ref [] in
                 for i = 0 to Array.length g-1 do
                   res := !res @ ttmax g i
                 done;
                 clean !res

let removett vlist ttlist =
  clean (List.map (List.filter (fun x -> not (List.mem x vlist))) ttlist)


let rec color ltt depth =
  if depth = 0 then ltt = [[]]
  else ltt = [[]] || List.exists (fun tt -> color (removett tt ltt) (depth-1)) ltt

let color4_malin all_tt new_tt=
  List.exists (fun tt -> color (removett tt all_tt) 3) new_tt 

let color3_malin all_tt new_tt=
  List.exists (fun tt -> color (removett tt all_tt) 2) new_tt 

let rec f1ijx tournoi j=function
  |[]->[[]]
  |h::t->if List.mem h tournoi.(j) then List.map (function l -> h::l) (f1ijx tournoi j t) else f1ijx tournoi j t;;

let rec f1ixj tournoi j=function
  |[]->[[j]]
  |h::t when List.mem j tournoi.(h) -> List.map (function l -> h::l) (f1ixj tournoi j t)
  |h::t when List.mem h tournoi.(j) -> (f1ixj tournoi j t)@(List.map (function l -> j::h::l) (f1ijx tournoi j t))
  |_::t->f1ixj tournoi j t;;

let rec f1xij tournoi i j=function
  |[]->failwith "wtf"
  |h::t when h=i->List.map (function l-> i::l) (f1ixj tournoi j t)
  |h::t->if List.mem j tournoi.(h) then List.map (function l -> h::l) (f1xij tournoi i j t) else f1ixj tournoi j t;;

(**)

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
  |_::t->f2xij tournoi i j t;;

(*

f1xij [|[];[3];[3];[0;4;8];[];[3];[3];[];[];[];[3]|] 2 3 [0;1;2;4;5;6;7;8;9;10];;

f2xij [|[2];[];[1;3;5;6;10];[];[2];[];[];[];[2];[];[]|] 2 3 [10;9;8;7;6;5;4;3;1;0];;
 *)

let rec newttmax tournoi i j = function
  |[]->[]
  |h::t when List.mem i h -> (f1xij tournoi i j h)@(newttmax tournoi i j t)
  |h::t when List.mem j h -> (f2xij tournoi i j h)@(newttmax tournoi i j t)
  |_::t->newttmax tournoi i j t;;


let av = ref 0

let print v =
  Array.iteri (fun i x -> List.iter (fun y-> Printf.printf "(%d,%d)," i y) x) v;
  Printf.printf "\n%!"

let res =ref []
let av = ref 0
       
let rec dsmash digons tournoi tt depth= match digons with
   |[]-> print tournoi; res:= Array.copy(tournoi) :: !res; 
   |(a,b)::q ->
     if depth=10 then begin
         Printf.printf "%d\n%!" !av;
         incr av
       end;
     tournoi.(a) <- b::tournoi.(a);
     let l = clean(newttmax tournoi a b tt) in
     let all_tt = clean(l@tt) in 
     if List.for_all (fun x-> List.length x < 7) l && not (color3_malin all_tt l) then
       dsmash q tournoi all_tt (depth+1)
          else
       if depth < 10 then
         av := !av + (1 lsl (10-depth));
     tournoi.(a) <- List.tl tournoi.(a);
     if a < b then
       dsmash ((b,a)::q) tournoi tt depth
          
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
              
let rec compl i n = function
    []-> if i <n then i::compl (i+1) n [] else []
   |t::q when i<t -> i::compl (i+1) n (t::q)
   |t::q -> compl (i+1) n q

          
let rec doublons = function
    []->[]
   |t::t2::q when t=t2 -> doublons (t2::q)
   |t::q -> t::doublons q
          
          
let splittings  g =
  (parties 3 8)@(parties 4 8)
  |> List.map (List.map (fun x-> x-1))
  |> List.map (fun x-> [x;compl 0 8 x])
  |> List.filter (List.for_all (fun p -> not (is_subtrans g p)))
  |> List.concat
  |> List.sort (compare)
  |> doublons 
  

let rec restrict_nbr nbr lv i = match (nbr,lv) with
    [],_-> []
   |_,[]-> []
   |t::q,t2::q2 when t=t2 -> i::restrict_nbr q q2 (i+1)
   |t::q,t2::q2 when t<t2 -> restrict_nbr q lv i
   |t::q,t2::q2 -> restrict_nbr nbr q2 (i+1)


let restrict g lv =
  let res = Array.make (List.length lv) [] in
  List.iteri (fun i x-> res.(i) <- restrict_nbr (List.sort compare g.(x)) lv 0) lv;
  res

let typ g l =
  List.filter (fun x-> color (allttmax (restrict g ([0;1;2;3;4]@(List.map (fun y-> y+5) x)))) 2) l

let compat t1 t2 = List.exists (fun t-> List.mem (compl 0 8 t) t2) t1
(*
let compat g1 g2 =
  let spl = (parties 1 8)@(parties 2 8)@(parties 3 8)@(parties 4 8)
            |> List.map (List.map (fun x-> x-1)) in
  List.exists (fun x -> let a = List.map (fun y->y+5) x in
                        let b = List.map (fun y-> y+5) (compl 0 8 x) in
                        not (is_subtrans g1 a) && not(is_subtrans g1 b)
                        && color (allttmax (restrict g1 ([0;1;2;3;4]@a))) 2
                        && color (allttmax (restrict g2 ([0;1;2;3;4]@b))) 2) spl
  || List.exists (fun x -> let a = List.map (fun y->y+5) x in
                           let b = List.map (fun y-> y+5) (compl 0 8 x) in
                           not (is_subtrans g1 a) && not (is_subtrans g1 b)
                           && color (allttmax (restrict g1 ([0;1;2;3;4]@b))) 2
                           && color (allttmax (restrict g2 ([0;1;2;3;4]@a))) 2) spl
  || List.exists (fun x -> let a = List.map (fun y->y+5) x in
                           let b = List.map (fun y-> y+5) (compl 0 8 x) in
                           is_subtrans g1 ([0;1;2;3;4]@a) 
                           && color (allttmax (restrict g2 ([0;1;2;3;4]@b))) 3) spl
  || List.exists (fun x -> let a = List.map (fun y->y+5) x in
                           let b = List.map (fun y-> y+5) (compl 0 8 x) in
                           is_subtrans g1 ([0;1;2;3;4]@b) 
                           && color (allttmax (restrict g2 ([0;1;2;3;4]@a))) 3) spl
 *)


let t8_3col =
  let chan = open_in "tournoisX8not3col" in
  let v = Array.make 258 0 in
  for i = 0 to 257 do
    v.(i) <- int_of_string (input_line chan)
  done;
  close_in chan;
  v
(*[216; 214; 212; 211; 206; 204; 203; 197; 196; 190; 187; 186; 176; 175; 173;
 171; 169; 168; 165; 159; 158; 157; 156; 153; 152; 151; 150; 144; 142; 140;
 138; 134; 133; 129; 128; 125; 117; 116; 115; 106; 103; 102; 101; 100; 99;
 98; 95; 94; 93; 92; 91; 90; 89; 82; 81; 79; 78; 77; 76; 75; 74; 73; 72; 69;
 68; 67; 65; 64; 60; 59; 58; 57; 56; 55; 54; 53; 52; 51; 48; 47; 46; 44; 43;
 42; 36; 35; 34; 31; 30; 22; 19; 11; 7; 4]

let res = ref [] in
    for i = 0 to 257 do
      let t = from_entier t8_3col.(i) in
      if not (List.exists (fun tt -> List.length tt = 5) (allttmax (convert t 8))) then res:= i::!res
    done;
    !res;;

*)
let completions t8 =
  res:=[];
  av:=0;
  dsmash (List.map (fun x-> List.map (fun y-> (y,x)) [0;1;2;3;4]) [5;6;7;8;9;10;11;12] |> List.concat) (convert ([(0,1);(0,2);(0,3);(0,4);(1,2);(1,3);(1,4);(2,3);(2,4);(3,4)]@(List.map (fun (a,b) -> (a+5,b+5)) t8)) 13) ([0;1;2;3;4]::(List.map (List.map (fun a-> a+5)) (allttmax (convert t8 8)))) 0;
  !res


let rec check =function
    []-> true
  | t::q ->  List.for_all (fun t2 -> compat t t2) (t::q) && check q

let rec incomp =function
    []->[]
  |t::q -> (List.find_all (fun t2-> not (compat t t2)) (t::q) |> List.map (fun x-> (t,x)))@incomp q

let glue g1 g2 =
  let g = g2
  |> List.filter (fun (a,b) -> a <5 || b<5)
  |> List.map (fun (a,b) -> (if a <5 then a+13 else a), (if b<5 then b+13 else b))
  in g1@g

let process g1 g2 =
  let g = glue g1 g2 in
  let tt1 = allttmax (convert g1 13) and tt2 = List.map (List.map (fun a -> if a <5 then a+13 else a)) (allttmax(convert g2 13)) in
  (convert g 18,tt1@(List.filter (fun x-> not (List.mem x tt1)) tt2)) 

let rec dsmash2 digons tournoi tt depth= match digons with
   |[]-> print tournoi; res:= Array.copy(tournoi) :: !res; 
   |(a,b)::q ->
     if depth=10 then begin
         Printf.printf "%d\n%!" !av;
         incr av
       end;
     tournoi.(a) <- b::tournoi.(a);
     let l = clean(newttmax tournoi a b tt) in
     let all_tt = clean(l@tt) in 
     if not (color4_malin all_tt l) then
       dsmash2 q tournoi all_tt (depth+1)
          else
       if depth < 10 then
         av := !av + (1 lsl (10-depth));
     tournoi.(a) <- List.tl tournoi.(a);
     if a < b then
       dsmash2 ((b,a)::q) tournoi tt depth



let doubleglue g1 g2 =
  res:=[];
  av:=0;
  let (g,tt) = process g1 g2 in
  let digons = List.map (fun x-> List.map (fun y-> (y,x)) [0;1;2;3;4]) [13;14;15;16;17] |> List.concat in
  dsmash2 digons g tt 0;
  !res

let rec assoc l1 l2 = List.map (fun x-> List.map (fun y-> (y,x)) l1) l2 |> List.concat 
(*
let completions _ =
  let res = ref [] in 
  try
    while true do
      let t = ref [] in 
      for i = 1 to 78 do
        Scanf.scanf "(%d,%d)," (fun a b -> t:= (a,b):: !t)
      done;
      res:= (convert !t 13):: !res;
      Scanf.scanf "\n"()
    done;
    !res
  with _ -> !res
            *)      

let _ =
  for i = int_of_string (Sys.argv.(1)) to int_of_string(Sys.argv.(2)) do
    Printf.printf "------ Graph %d -----\n%!" i ;
    let g = from_entier (t8_3col.(i)) in
    let cg = completions g in
    Printf.printf "%d completions\n\n%!" (List.length cg);
    List.iter print cg;  
    Printf.printf "------ Testing completions -----\n%!";
    let spl = splittings (convert g 8) in
    let j = ref 0 in 
    let ct = List.sort compare (List.rev_map (fun t-> if !j mod 200=0 then Printf.printf "%d\n%!" !j; incr j; (typ t spl,t)) cg) in
    Printf.printf "ct %d\n%!" (List.length ct);
    let types = doublons (List.map fst ct) in
    Printf.printf "types %d\n%!" (List.length types);
    let itypes = incomp types in
    Printf.printf "itypes %d\n%!" (List.length itypes); 
    let ic = List.rev_map (fun (a,b) -> (List.rev_map snd (List.filter (fun t-> fst t = a) ct),List.rev_map snd (List.filter (fun t-> fst t = b) ct))) itypes |> List.map (fun (a,b)-> assoc a b) |> List.concat in
    Printf.printf "%d paires à tester\n%!" (List.length ic);
    let i = ref 0 in
    List.map (fun (a,b) -> incr i; Printf.printf "----- Collage %d ----\n%!" !i; doubleglue (deconvert a) (deconvert b)) ic |> List.concat  |> List.iter print;
    Printf.printf "--- FINI ---------\n"
  done;




(*exception B2 of int list list
          
let rec find_color ltt depth =
  if depth = 0 then
    (ltt = [[]],[])
  else
    if ltt = [[]] then (true,[])
  else
    try
      List.iter (fun tt -> let (a,b) = find_color (removett tt ltt) (depth-1) in if a then raise (B2 (tt::b))) ltt;
      (false, [])
    with B2 b -> (true,b)

 *)

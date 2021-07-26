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
  Array.iteri (fun i x -> List.iter (fun y-> Printf.printf "(%d,%d);" i y) x) v;
  Printf.printf "\n%!"

let res =ref []
let av = ref 0
       
let rec dsmash digons tournoi tt depth= match digons with
  |[]-> (*print tournoi;*) res:= Array.copy(tournoi) :: !res; 
   |(a,b)::q ->
(*     if depth=6 then begin
         Printf.printf "%d\n%!" !av;
         incr av
       end;*)
     tournoi.(a) <- b::tournoi.(a);
     let l = clean(newttmax tournoi a b tt) in
     let all_tt = clean(l@tt) in 
     if not (color3_malin all_tt l) then
       dsmash q tournoi all_tt (depth+1)
     (*else
       if depth < 6 then
         av := !av + (1 lsl (6-depth))*);
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
  
let t8_3col =
  let chan = open_in "tournoisX8not3col" in
  let v = Array.make 258 0 in
  for i = 0 to 257 do
    v.(i) <- int_of_string (input_line chan)
  done;
  close_in chan;
  v

let completions t8 =
  res:=[];
  av:=0;
  dsmash (List.map (fun x-> List.map (fun y-> (y,x)) [0;1;2;3;4]) [5;6;7;8;9;10;11;12] |> List.concat) (convert ([(0,1);(0,2);(0,3);(0,4);(1,2);(1,3);(1,4);(2,3);(2,4);(3,4)]@(List.map (fun (a,b) -> (a+5,b+5)) t8)) 13) ([0;1;2;3;4]::(List.map (List.map (fun a-> a+5)) (allttmax (convert t8 8)))) 0;
  !res
  
let rec check =function
    []-> true
  | t::q ->  List.for_all (fun t2 -> compat t t2) (t::q) && check q

(*  
  
let _  =
  for i = int_of_string (Sys.argv.(1)) to int_of_string(Sys.argv.(2)) do
    Printf.printf "------ Graph %d -----\n%!" i ;
    let g = from_entier (t8_3col.(i)) in
    let cg = completions g in
    Printf.printf "%d completions" (List.length cg);
    if check cg then
      Printf.printf ", all of them pairwise compatible\n"
    else
      Printf.printf ", some of them incompatible\n";
    List.iter print cg;
    Printf.printf "---------\n"
  done;
  Printf.printf "------ FINI -----\n%!"

let t19 =Array.map (List.sort compare) [|[1;7;8;2;9;10;4;13;14];
            [7;2;9;10;3;11;12;5;15;16];
            [9;4;13;14;3;11;12;6;17;18];
            [11;0;4;13;14;5;15;16];
            [13;1;7;8;5;15;16;6;17;18];
            [15;0;2;9;10;6;17;18];
            [17;0;1;7;8;3;11;12];
            [8;2;9;10;3;11;12;5;15;16];
            [1;2;9;10;3;11;12;5;15;16];
            [10;4;13;14;3;11;12;6;17;18];
            [2;4;13;14;3;11;12;6;17;18];
            [12;0;4;13;14;5;15;16];
            [3;0;4;13;14;5;15;16];
            [14;1;7;8;5;15;16;6;17;18];
            [4;1;7;8;5;15;16;6;17;18];
            [16;0;2;9;10;6;17;18];
            [5;0;2;9;10;6;17;18];
            [18;0;1;7;8;3;11;12];
            [6;0;1;7;8;3;11;12]|]



       
let _ = if color (allttmax t19) 4 then Printf.printf "Il est 4 colorable\n%!" else Printf.printf "Il est 5 chromatique!\n"
            
 *)
      
let glue g1 g2 =
  let g = g2
  |> List.filter (fun (a,b) -> a <5 || b<5)
  |> List.map (fun (a,b) -> (if a <5 then a+13 else a), (if b<5 then b+13 else b))
  in g1@g

let process g1 g2 =
  let g = glue g1 g2 in
  let tt1 = allttmax (convert g1 13) and tt2 = List.map (List.map (fun a -> if a <5 then a+13 else a)) (allttmax(convert g2 13)) in
  (convert g 18,tt1@(List.filter (fun x-> not (List.mem x tt1)) tt2)) 

let doubleglue g1 g2 =
  res:=[];
  av:=0;
  let (g,tt) = process g1 g2 in
  let digons = List.map (fun x-> List.map (fun y-> (y,x)) [0;1;2;3;4]) [13;14;15;16;17] |> List.concat in
  dsmash digons g tt 0;
  !res

(*
exception B2 of int list list
          
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

let permute t v = List.map (fun (a,b) -> v.(a),v.(b)) t

let rec doublonsaux = function
    []->[]
   |(t,_)::(t2,b2)::q when t=t2 -> doublonsaux ((t2,b2)::q)
   |t::q -> t::doublonsaux q

let rec number (a,b) = function
  |[]-> [(a,b)]
  |t::q when t = a -> number (a,b+1) q
  |t::q -> (a,b)::number(t,1) q

let rec filtr = function
    []->[]
   |t::q -> t::filtr (List.filter (fun t' -> t' <> t && t' <> List.sort compare (permute t [|0;1;2;3;4;6;7;5|]) && t' <> List.sort compare (permute t [|0;1;2;3;4;7;5;6|])) q)
          
let x8fixedTT5 =
  (Array.to_list t8_3col)
  |> List.map (fun y-> List.map (fun t-> (from_entier y,t)) (List.filter (fun tt-> List.length tt = 5) (allttmax (convert (from_entier y) 8))))
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

let rec find t i = function
    []->raise Not_found
   |a::b when a=t -> i
   |_::b -> find t (i+1) b
          
let n = Array.length x8fixedTT5
   
let collages = Array.make_matrix n n []
let scan () =
  let res = ref [] in 
  for i = 1 to 78 do
    Scanf.scanf "(%d,%d);" (fun  a b -> Printf.printf "%d %d\n" a b; res:=(a,b):: !res)
  done;
  Scanf.scanf "\n" ();
  List.rev !res


let progress = ref 0

let fill_scan i j = 
  if collages.(i).(j) = [] then begin
      let t0 = ref 0 and t1 = ref 0 and t2 = ref 0 in 
      Scanf.scanf "Start %d %d:\n" (fun a b -> assert (a=i && b=j));
      Scanf.scanf "End %d %d: %d,%d,%d,=%d completions found\n" (fun _ _ e f g _ -> t0:=e; t1:=f; t2:=g);
      let c0 = ref [] in 
      for k = 1 to !t0 do
        c0 := (convert (scan ()) 13) :: !c0
      done;
      let c1 = ref [] in 
      for k = 1 to !t1 do
        c1 := (convert (scan ()) 13) :: !c1
      done;
      let c2 = ref [] in 
      for k = 1 to !t2 do
        c2 := (convert (scan ()) 13) :: !c2
      done;
      collages.(i).(j) <- [!c0;!c1;!c2];
      Scanf.scanf "Progression = %d cases/32896 = %s\n" (fun _ _ -> incr progress);
    end

let test_scan () =
  Scanf.scanf "Begin test (%d,%d,%d)\n" (fun _ _ _ -> ());
  Scanf.scanf "End test (%d,%d,%d), %d graphs found\n" (fun _ _ _ _ -> ())

let start = ref 0

let restore () =
  try
    for i = 0 to n-1 do 
      Scanf.scanf "Begin Graph %d\n" (fun x -> assert (i=x));
      for j = i to n-1 do
        fill_scan i j;
      done;
      Scanf.scanf "Testing completions of graph %d\n" (fun _ -> ());
      for j= i to n-1 do
        if collages.(i).(j) <> [[];[];[]] then 
          for k=j to n-1 do
            if collages.(i).(k) <> [[];[];[]] then begin
                fill_scan j k;
                test_scan ()
              end
          done;
      done;
      collages.(i) <- [||];
      Scanf.scanf "End Graph %d : %d non empty pairs, %d 13-completions, %d 18-completions\n" (fun _ _ _ _ -> incr start);
      
    done;
    ()
  with _ -> ()

let _ = restore()
        

let collij g1 g2 = 
  let g = g1@(g2
              |> List.filter (fun (a,b) -> a <5 || b<5)
              |> List.map (fun (a,b) -> (if a <5 then a+8 else a), (if b<5 then b+8 else b))) in
  let tt1 = allttmax (convert g1 8) and tt2 = List.map (List.map (fun a -> if a <5 then a+8 else a)) (allttmax(convert g2 8)) in
  res:=[];
  let digons = List.map (fun x-> List.map (fun y-> (y,x)) [0;1;2;3;4]) [8;9;10;11;12] |> List.concat in
  dsmash digons (convert g 13) (tt1@(List.filter (fun x-> not (List.mem x tt1)) tt2)) 0;
  !res

let fill i j =
  if collages.(i).(j) = [] then begin
      Printf.printf "Start %d %d:\n%!" i j;
      let g1 = x8fixedTT5.(i) in
      let g2 = x8fixedTT5.(j) in
      let res = [collij g1 g2; collij g1 (permute g2 [|0;1;2;3;4;6;7;5|]) ;collij g1 (permute g2 [|0;1;2;3;4;7;5;6|])]  in
      Printf.printf "End %d %d: "i j;
      List.iter (fun x -> Printf.printf "%d," (List.length x)) res;
      let a = (List.fold_left (fun x y -> x+List.length y) 0 res) in
      Printf.printf "=%d completions found\n" a;
      List.iter (List.iter print) res;
      collages.(i).(j)<- res;
      incr progress;
      Printf.printf "Progression = %d cases/32896 = %f%%\n" !progress ((float_of_int !progress)*.100./.32896.)
    end;
  List.fold_left (fun x y -> x+List.length y) 0 collages.(i).(j)

let tripleglue tij tik tjk =
  tij
  @(tik |> List.filter (fun (a,b) -> a >7 || b>7)
    |> List.map (fun (a,b) -> (if a >7 then a+5 else a), (if b>7 then b+5 else b)))
  @(tjk |> List.filter (fun (a,b) -> (a <5 && b >7) || (a >7 && b<5))
    |> List.map (fun (a,b) -> (if a <5 then a+8 else if a>7 then a+5 else a), (if b<5 then b+8 else if b>7 then b+5 else b)))

let rec assoc = function
    []-> [[]]
  |t::q-> let q' = assoc q in List.rev_map (fun x -> List.rev_map (fun y -> x::y) q') t |> List.concat
(*
let ij = [|[];[];[]|]

let ik = [|[];[];[]|]

let jk = [|[];[];[]|]

let scan f =
  let res = ref [] in 
  for i = 1 to 78 do
    Scanf.bscanf f "(%d,%d);" (fun  a b -> res:=(a,b):: !res)
  done;
  Scanf.bscanf f "\n" ();
  List.rev !res


let _ =
  let f = Scanf.Scanning.open_in "tmp1" in
  for j = 1 to 512 do
    ij.(1) <- (scan f)::ij.(1)
  done;
  for j = 1 to 512 do
    ik.(1) <- (scan f)::ik.(1)
  done;
  jk.(0) <- [scan f];
  Scanf.Scanning.close_in f

let i = ref 0

let l = List.rev_map (fun [a;b;c] -> if !i mod 10000=0 then Printf.printf "%d\n%!" !i; incr i; convert (tripleglue a b c) 18) (assoc [ij.(1);ik.(1);jk.(0)])

let _ = List.filter (fun t -> if !i mod 100=0 then Printf.printf "%d\n%!" !i; incr i; not(color (allttmax t) 4)) l
 *)
let test (i,j,k) =
  Printf.printf "Begin test (%d,%d,%d)\n%!" i j k;
  let ij = Array.of_list collages.(i).(j) in
  let ik = Array.of_list collages.(i).(k) in
  let jk = Array.of_list collages.(j).(k) in 
  let l = ((assoc [ij.(0);ik.(0);jk.(0)])
           @(assoc [ij.(0);ik.(1);jk.(1)])
           @(assoc [ij.(0);ik.(2);jk.(2)])
           @(assoc [ij.(1);ik.(0);jk.(2)])
           @(assoc [ij.(1);ik.(1);jk.(0)])
           @(assoc [ij.(1);ik.(2);jk.(1)])
           @(assoc [ij.(2);ik.(0);jk.(1)])
           @(assoc [ij.(2);ik.(1);jk.(2)])
           @(assoc [ij.(2);ik.(2);jk.(0)])
          )|> List.rev_map (fun [a;b;c] -> convert (tripleglue (deconvert a) (deconvert b) (deconvert c)) 18)
          |> List.filter (fun t-> not(color (allttmax t) 4)) in
  List.iter print l;
  Printf.printf "End test (%d,%d,%d), %d graphs found\n%!" i j k (List.length l);
  List.length l
  
let _ =
  for i = !start to n-1 do 
    let nep = ref 0 and  compl = ref 0 and final = ref 0 in
    if i > !start then Printf.printf "Begin Graph %d\n%!" i;
    for j = i to n-1 do
      compl := !compl + fill i j;
      if collages.(i).(j) <> [[];[];[]] then incr nep;
    done;
    Printf.printf "Testing completions of graph %d\n%!" i;
    for j=i to n-1 do
      if collages.(i).(j) <> [[];[];[]] then 
        for k=j to n-1 do
          if collages.(i).(k) <> [[];[];[]] then 
            let _ = fill j k in
            (*if collages.(j).(k) <> [[];[];[]] then *)
              final := !final + test (i,j,k)
        done;
    done;
    collages.(i) <- [||];
    Printf.printf "End Graph %d : %d non empty pairs, %d 13-completions, %d 18-completions\n%!" i !nep !compl !final;
  done
 
(*   
let triplets =
  let res= ref[] in
  for i = 0 to n-1 do
    for j = i to n-1 do
      if collages.(i).(j) <> [[];[];[]] then
        for k = j to n-1 do
          if collages.(i).(k) <> [[];[];[]] && collages.(j).(k) <> [[];[];[]] then
            res:= (i,j,k):: !res
        done;
    done;
  done;
  !res

let _ = Printf.printf "\n%d triplets à tester\n\n%!" (List.length triplets)
 *)
 



let rec printlst (l: int list) =
	match l with 
	[] -> print_string " ~\n"; ()
	| h::t -> print_int h; print_string ";"; printlst t;
;;

let rec printllst (l: int list list) =
	match l with 
	[] -> print_string " ~\n"; ()
	| h::t -> printlst h; print_string ";"; printllst t;
;;


let revf f x y = if f x y then false else true;;

let partition (f: 'a -> 'a -> bool) (v: 'a) (l: 'a list) : ('a list * 'a list) = 
List.fold_left (fun (l1, l2) x -> if f x v then (x::l1, l2) else (l1, x::l2)) ([],[]) l;;

let lstpass (f: 'a -> 'a -> bool) (l: 'a list) (acc: 'a list list) : ('a list list) = match l with
	[] -> acc
	| h::[] -> [h]::acc
	| h::t -> let p = partition f h t in snd(p)::([h]::(fst(p)::acc));;
	(*acc @ [fst(p);[h];snd(p)];;*)

let rec qpass (f: 'a -> 'a -> bool) (pl: 'a list list) : ('a list list) = 
	let l = List.fold_left (fun acc x -> lstpass f x acc) [] pl in
	printllst l ; l;;

let issorted (f: 'a -> 'a -> bool) (v: 'a) (pl: 'a list) = match pl with
	[] -> (v, true)
	| h::[] -> if f v h then (h, true) else (h, false)
	| h::t -> List.fold_left (fun (prev, acc) x ->
	 if acc && f prev x then (x, true) else (prev, false)) (v, true) t;;

exception IndexFault

let rec first (pl: 'a list list) = match pl with
	[] -> raise IndexFault
	| []::t -> first t
	| (h::t1)::t2 -> h;;

let issortedbig (f: 'a -> 'a -> bool) (pl: 'a list list) = 
List.fold_left (fun (prev, acc) x -> if acc then (issorted f prev x) else (prev, false)) (first pl, true) pl;;

let rec qsort (f: 'a -> 'a -> bool) (pl: 'a list list) = 
	if snd(issortedbig f pl) then List.flatten(pl) else
	qsort (revf f) (qpass f pl);;


let printres (l1, l2) =
	printlst l1 ; print_string("\n"); printlst l2;;


let f x y = if x<=y then true else false;;

printlst (qsort f [[77;84;1;12;6;10;134;3;7;-9;33;-74]]);;

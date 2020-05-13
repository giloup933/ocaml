open List;;

let print_sq (x, y) = 
	print_string (match x with
	1 -> "a";
	| 2 -> "b";
	| 3 -> "c";
	| 4 -> "d";
	| 5 -> "e";
	| 6 -> "f";
	| 7 -> "g";
	| 8 -> "h";
	| _ -> "xxxxxxx");
	print_int y; print_string " "; ();;

let rec printlst (l: (int * int) list) =
	match l with 
	[] -> print_string "~~\n"; ()
	| h::t -> print_sq h; printlst t;
;;

let moves (x, y) n = 
	let m = [(x+1,y+2);(x+1,y-2);(x-1,y+2);(x-1,y-2);
	(x+2,y-1);(x+2,y+1);(x-2,y-1);(x-2,y+1)] in
	let filtersq (x, y) = if x<1 || x>n || y<1 || y>n then false else true in
	List.filter filtersq m;;

let heuristics n (x, y) visited = 
	let m = List.filter (fun x -> not(List.mem x visited)) (moves (x,y) n) in
	List.length m;;

let warnsdorff n (x, y) visited = 
	let m = List.filter (fun x -> not(List.mem x visited)) (moves (x,y) n) in
	List.fast_sort(fun m1 m2 -> (heuristics n m1 visited) - (heuristics n m2 visited)) m;;

let knighttour n x y =
	let path = n*n in
	let rec tour (n: int) (sq: int * int) (visited: (int * int) list) = if List.length (sq::visited) = path then sq::visited else
		let succ = (warnsdorff n sq visited) in
		fst(List.fold_left (fun (acc, cond) e -> if cond then (acc, cond) else let t = tour n e (sq::visited) in 
if List.length t = path then (t, true) else ([], false)) ([], false) succ)
	in
	List.rev(tour n (x, y) []);;

printlst (knighttour 8 1 1);;

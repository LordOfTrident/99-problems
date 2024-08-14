(* Option printing helper function for results *)
let print_option f o =
	print_endline (
		match o with
		| None   -> "None"
		| Some x -> f x
	)

(* Convert to string helper functions *)
let str_str    (x: string) = x
let str_int    (x: int)    = Printf.sprintf "%d" x
let str_bool   (x: bool)   = Printf.sprintf "%b" x
let str_tuple2 (x, y)      = Printf.sprintf "%s, %s" x y
let rec str_list (l: string list) =
	match l with
	| []     -> ""
	| [x]    -> x
	| h :: t -> h ^ ", " ^ str_list t


(* Problem 01 *)

let rec my_last l =
	match l with
	| []     -> None
	| [x]    -> Some x
	| _ :: t -> my_last t

let () =
	print_endline "-- 01 --";
	print_option (str_str) (my_last ["a"; "b"; "c"; "d"]);
	print_option (str_str) (my_last [])


(* Problem 02 *)

let rec my_last_two l =
	match l with
	| [] | [_] -> None
	| [x; y]   -> Some (x, y)
	| _ :: t   -> my_last_two t

let () =
	print_endline "-- 02 --";
	print_option (str_tuple2) (my_last_two ["a"; "b"; "c"; "d"]);
	print_option (str_tuple2) (my_last_two ["a"])


(* Problem 03 *)

let rec my_at l k =
	match l with
	| []     -> None
	| h :: t -> if k = 1 then Some h else my_at t (k - 1)

let () =
	print_endline "-- 03 --";
	print_option (str_str) (my_at ["a"; "b"; "c"; "d"; "e"] 3);
	print_option (str_str) (my_at ["a"] 3)


(* Problem 04 *)

let my_len l =
	let rec loop l acc =
		match l with
		| []     -> acc
		| _ :: t -> loop t (acc + 1)
	in loop l 0

let () =
	print_endline "-- 04 --";
	print_endline (str_int (my_len ['a'; 'b'; 'c']));
	print_endline (str_int (my_len []))


(* Problem 05 *)

let my_reverse l =
	let rec loop l acc =
		match l with
		| []     -> acc
		| h :: t -> loop t (h :: acc)
	in loop l []

let () =
	print_endline "-- 05 --";
	print_endline (str_list (my_reverse ["a"; "b"; "c"]))


(* Problem 06 *)

let my_palindrome l = l = my_reverse l

let () =
	print_endline "-- 06 --";
	print_endline (str_bool (my_palindrome ["x"; "a"; "m"; "a"; "x"]));
	print_endline (str_bool (my_palindrome ["a"; "b"]))


(* Problem 07 *)

type 'a node =
	| One of 'a
	| Many of 'a node list

let my_flatten l =
	let rec loop l acc =
		match l with
		| []           -> acc
		| One  x  :: t -> loop t (x :: acc)
		| Many xs :: t -> loop t (loop xs acc)
	in my_reverse (loop l [])

let () =
	print_endline "-- 07 --";
	print_endline (str_list (my_flatten [One "a"; Many [One "b"; Many [One "c"; One "d"]; One "e"]]))


(* Problem 08 *)

(*
let my_compress l =
	let rec loop l v acc =
		match l with
		| [] -> acc
		| h :: t -> if Some h = v then loop t v acc else loop t (Some h) (h :: acc)
	in my_reverse (loop l None [])
*)

let my_compress l =
	let rec loop l acc =
		match l with
		| h :: (next :: _ as t) when h = next -> loop t acc
		| h :: t -> loop t (h :: acc)
		| [] -> acc
	in my_reverse (loop l [])

let () =
	print_endline "-- 08 --";
	print_endline (str_list (my_compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]))

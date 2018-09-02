let slice_tail ls a b =
	let rec drop n = function
		| [] -> []
		| hd :: tl as l ->
			if n > 0 then
				drop (n - 1) tl
			else
				l
	in
	let rec rev acc = function
		| [] -> acc
		| hd :: tl ->
			rev (hd :: acc) tl
	in
	let rec extract acc n = function
		| [] -> acc
		| hd :: tl ->
			if n > 0 then
				extract (hd :: acc) (n - 1) tl
			else
				acc
	in
	let chopped = drop a ls
	in
	rev [] (extract [] (b - a + 1) chopped);;

let slice_fold ls a b =
	let rec fold_until f acc n = function
		| [] -> (acc, [])
		| hd :: tl as ll -> 
			if n > 0 then
				fold_until f (f acc hd) (n - 1) tl
			else
				(acc, ll)
	in
	let rec rev acc = function
		| [] -> acc
		| hd :: tl ->
			rev (hd :: acc) tl
	in
	let _, dropped = fold_until (fun acc v -> []) [] a ls
	in
	let result, _ = fold_until (fun acc v -> v :: acc) [] (b - a + 1) dropped
	in
	rev [] result;;

let slice = slice_fold;;

assert (slice ['a'; 'b'; 'c'] 1 1 = ['b']);;
assert (slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6 = ["c"; "d"; "e"; "f"; "g"]);;

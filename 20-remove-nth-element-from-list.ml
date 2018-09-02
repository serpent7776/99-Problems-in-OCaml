let remove_at_join n ls =
	let rec split acc n = function
		| [] -> ([], [])
		| hd :: tl ->
			if n > 0 then
				split (hd :: acc) (n - 1) tl
			else
				(acc, tl)
	in
	let hd, tl = split [] n ls
	in
	let rec rev acc = function
		| [] -> acc
		| hd :: tl -> rev (hd :: acc) tl
	in
	(rev [] hd) @ tl;;

let remove_at_filter n ls =
	let rec filter_n acc n = function
		| [] -> (acc, [])
		| hd :: tl ->
			if n > 0 then
				filter_n (hd :: acc) (n - 1) tl
			else
				(acc, tl)
	in
	let hd, tl = filter_n [] n ls
	in
	let rec rev acc = function
		| [] -> acc
		| hd :: tl -> rev (hd :: acc) tl
	in
	(rev [] hd) @ tl;;

let remove_at = remove_at_join;;

assert (remove_at 0 ["a";"b";"c";"d"] = ["b"; "c"; "d"]);;
assert (remove_at 1 ["a";"b";"c";"d"] = ["a"; "c"; "d"]);;
assert (remove_at 2 ["a";"b";"c";"d"] = ["a"; "b"; "d"]);;
assert (remove_at 3 ["a";"b";"c";"d"] = ["a"; "b"; "c"]);;

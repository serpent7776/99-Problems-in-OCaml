let insert_at_tail v idx ls =
	let rec rev acc = function
		| [] -> acc
		| hd :: tl -> rev (hd :: acc) tl
	in
	let rec split acc n = function
		| [] -> ((rev [] acc), [])
		| hd :: tl as ll ->
			if n > 0 then
				split (hd :: acc) (n - 1) tl
			else
				((rev [] acc), ll)
	in
	let pre, post = split [] idx ls
	in
	pre @ (v :: post);;

let rec insert_at_notail v idx = function
	| [] -> [v]
	| hd :: tl as ll ->
		if idx > 0 then
			hd :: insert_at_notail v (idx - 1) tl
		else
			v :: ll;;

let insert_at = insert_at_notail;;

assert (insert_at "alfa" 1 ["a";"b";"c";"d"] = ["a"; "alfa"; "b"; "c"; "d"]);;
assert (insert_at "alfa" 3 ["a";"b";"c";"d"] = ["a"; "b"; "c"; "alfa"; "d"]);;
assert (insert_at "alfa" 4 ["a";"b";"c";"d"] = ["a"; "b"; "c"; "d"; "alfa"]);;

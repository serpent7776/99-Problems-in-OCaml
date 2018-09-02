let rotate ls n =
	let rec split acc n = function
		| [] -> (acc, [])
		| (hd :: tl) as ll ->
			if n > 0 then
				split (hd :: acc) (n - 1) tl
			else
				(acc, ll)
	in
	let rec size n = function
		| [] -> n
		| hd :: tl -> size (n + 1) tl
	in
	let k =
		if n > 0 then
			n
		else
			(size 0 ls) - (- n)
	in
	let chunk, ls = split [] k ls
	in
	let rec rev acc = function
		| [] -> acc
		| hd :: tl -> rev (hd :: acc) tl
	in
	ls @ (rev [] chunk);;

assert (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3 = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]);;
assert (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2) = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]);;

let rand_select (ls : string list) n =
	let rec nth_indices acc n max =
		if n > 0 then
			let idx = Random.int max in
			nth_indices (idx :: acc) (n - 1) (max - 1)
		else
			acc
	in
	let rec remove acc n = function
		| [] -> raise Not_found
		| hd :: tl ->
			if n = 0 then
				(hd, ((List.rev acc) @ tl))
			else
				remove (hd :: acc) (n - 1) tl
	in
	let rec select acc ls = function
		| [] -> acc
		| hd :: tl ->
			let v, ls2 = remove [] hd ls in
			select (v :: acc) ls2 tl
	in
	let len = List.length ls in
	let idx = List.rev (nth_indices [] n len) in
	select [] ls idx
	;;

assert (rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 3 = ["g"; "d"; "a"]);;
assert (rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 1 = ["b"]);;

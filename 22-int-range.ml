let range a b =
	let min, max = b, a in
	let delta = 
		if min > max then -1
		else 1
	in
	let lim = max in
	let rec insert acc v =
		if v = lim then v :: acc
		else insert (v :: acc) (v + delta)
	in
	insert [] min;;

assert (range 4 9 = [4; 5; 6; 7; 8; 9]);;
assert (range 9 4 = [9; 8; 7; 6; 5; 4]);;

let split ls n =
	let rec rev acc = function
		| [] -> acc
		| hd :: tl ->
			rev (hd :: acc) tl
	in
	let rec proc acc k = function
		| [] -> ls, []
		| hd :: tl as l ->
			if k > 0 then
				proc (hd :: acc) (k - 1) tl
			else
				(rev [] acc), l
	in
	proc [] n ls;;

assert (split [] 1 = ([], []));;
assert (split ['a'] 1 = (['a'], []));;
assert (split ['a'; 'b'] 1 = (['a'], ['b']));;
assert (split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 = (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]));;
assert (split ["a";"b";"c";"d"] 5 = (["a"; "b"; "c"; "d"], []));;

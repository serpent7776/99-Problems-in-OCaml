let rec last_two = function
    | [] -> None
    | [f; s] -> Some (f, s)
    | hd :: tl -> last_two tl;;

assert (last_two [ "a" ; "b" ; "c" ; "d" ] = Some ("c", "d"));;
assert (last_two [ "a" ] = None);;
assert (last_two [] = None);;
assert (last_two ["a"; "c"] = Some ("a", "c"));;

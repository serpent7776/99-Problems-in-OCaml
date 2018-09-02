let rec last = function
    | [] -> None
    | [v] -> Some v
    | hd :: tl -> last tl;;

assert (last [ "a" ; "b" ; "c" ; "d" ] = Some "d");;
assert (last [] = None);;
assert (last [12] = Some 12);;

let duplicate_tail ls =
    let rec dup acc = function
        | [] -> acc
        | hd :: tl -> dup (hd :: hd :: acc) tl
    in
    let rec rev acc = function
        | [] -> acc
        | hd :: tl -> rev (hd :: acc) tl
    in
    rev [] (dup [] ls);;

let rec duplicate_notail = function
    | [] -> []
    | hd :: tl -> hd :: hd :: duplicate_notail tl

let duplicate = duplicate_notail;;

assert (duplicate ["a";"b";"c";"c";"d"] = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]);;

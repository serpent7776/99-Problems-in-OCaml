let replicate ls n =
    let rec put acc v = function
        | 0 -> acc
        | n -> put (v :: acc) v (n - 1)
    in
    let rec proc acc n = function
        | [] -> acc
        | hd :: tl ->
            let buf = put acc hd n
            in
            proc buf n tl
    in
    let rec rev acc = function
        | [] -> acc
        | hd :: tl -> rev (hd :: acc) tl
    in
    proc [] n (rev [] ls);;

assert (replicate ["a";"b";"c"] 3 = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]);;

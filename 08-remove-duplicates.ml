let compress_tail = function
    | [] -> []
    | hd :: tl ->
        let head = function
            | [] -> raise (Failure "hd")
            | hd :: tl -> hd
        in
        let rec proc acc = function
            | [] -> acc
            | hd :: tl ->
                let prev = head acc in
                if prev = hd then
                    proc acc tl
                else
                    proc (hd :: acc) tl
        in
        let rec rev acc = function
            | [] -> acc
            | hd :: tl -> rev (hd :: acc) tl
        in
        rev [] (proc [hd] tl);;

let rec compress_notail = function
    | [] -> []
    | hd :: tl ->
        let check_head value = function
            | [] -> false
            | hd :: tl -> hd = value
        in
        let ret = compress_notail tl in
        if check_head hd ret then
            ret
        else
            hd :: ret;;

let rec compress_notail2 = function
    | hd :: (md :: _ as tl) ->
        if hd = md then
            compress_notail2 tl
        else
            hd :: compress_notail2 tl
    | other -> other;;

let compress = compress_notail2;;

assert (compress [] = []);;
assert (compress [1] = [1]);;
assert (compress [1; 2; 3; 3; 2; 1] = [1; 2; 3; 2; 1]);;
assert (compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = ["a"; "b"; "c"; "a"; "d"; "e"]);;

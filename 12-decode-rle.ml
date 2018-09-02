type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let decode lst =
    let rec put buf v = function
        | 0 -> buf
        | n -> put (v :: buf) v (n - 1)
    in
    let rec proc acc = function
        | [] -> acc
        | One v :: tl -> proc (v :: acc) tl
        | Many (c, v) :: tl ->
            let buf = put acc v c
            in
            proc buf tl
    in
    let rec rev acc = function
        | [] -> acc
        | hd :: tl -> rev (hd :: acc) tl
    in
    rev [] (proc [] lst)
;;

assert (decode [] = []);;
assert (decode [One 'a'] = ['a']);;
assert (decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")] = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]);;

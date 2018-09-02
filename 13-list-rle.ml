type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let encode ls =
    let put acc v = function
        | 1 -> One v :: acc
        | n -> Many (n, v) :: acc
    in
    let rec proc acc n = function
        | [] -> acc
        | [v] -> put acc v n
        | hd :: (md :: _ as tl) ->
            if hd = md then
                proc acc (n + 1) tl
            else
                let buf = put acc hd n
                in
                proc buf 1 tl
    in
    let rec rev acc = function
        | [] -> acc
        | hd :: tl -> rev (hd :: acc) tl
    in
    rev [] (proc [] 1 ls);;

assert (encode [] = []);;
assert (encode ['a'] = [One 'a']);;
assert (encode ['a'; 'a'] = [Many (2, 'a')]);;
assert (encode ['b'; 'a'; 'a'] = [One 'b'; Many (2, 'a')]);;
assert (encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]);;

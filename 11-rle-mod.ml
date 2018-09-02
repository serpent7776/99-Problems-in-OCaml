type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let encode lst =
    let make_rle cnt v =
        if cnt > 1 then
            Many (cnt, v)
        else
            One v
    in
    let rec proc cnt acc = function
        | [] -> []
        | [v] ->
            let r = make_rle cnt v
            in r :: acc
        | hd :: (md :: _ as tl) ->
            if hd = md then
                proc (cnt + 1) acc tl
            else
                proc 1 ((make_rle cnt hd) :: acc) tl
    in
    let rec rev acc = function
        | [] -> acc
        | hd :: tl -> rev (hd :: acc) tl
    in
    rev [] (proc 1 [] lst);;

assert (encode [] = []);;
assert (encode ['a'] = [One 'a']);;
assert (encode ['a'; 'a'] = [Many (2, 'a')]);;
assert (encode ['a'; 'b'] = [One 'a'; One 'b']);;
assert (encode ['a'; 'a'; 'c'] = [Many (2, 'a'); One 'c']);;
assert (encode ['a'; 'a'; 'c'; 'c'] = [Many (2, 'a'); Many (2, 'c')]);;
assert (encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]);;

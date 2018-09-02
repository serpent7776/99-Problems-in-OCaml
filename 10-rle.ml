let encode lst =
    let rec proc count acc = function
        | [] -> []
        | [v] -> (count + 1, v) :: acc
        | hd :: (md :: _ as tl) ->
            if hd = md then
                proc (count + 1) acc tl
            else
                proc 0 ((count + 1, hd) :: acc) tl
    in
    let rec rev acc = function
        | [] -> acc
        | hd :: tl -> rev (hd :: acc) tl
    in
    rev [] (proc 0 [] lst);;

assert (encode [] = []);;
assert (encode ['a'] = [(1, 'a')]);;
assert (encode ['a'; 'a'] = [(2, 'a')]);;
assert (encode ['a'; 'b'] = [(1, 'a'); (1, 'b')]);;
assert (encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]);;

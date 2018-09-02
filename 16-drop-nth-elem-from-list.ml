let drop ls n =
    let rec proc acc idx = function
        | [] -> acc
        | hd :: tl ->
            if idx = n then
                proc acc 1 tl
            else
                proc (hd :: acc) (idx + 1) tl
    in
    let rec rev acc = function
        | [] -> acc
        | hd :: tl -> rev (hd :: acc) tl
    in
    rev [] (proc [] 1 ls);;

assert (drop ['a'] 1 = []);;
assert (drop ['a'; 'b'] 2 = ['a']);;
assert (drop ['a'; 'b'; 'c'] 2 = ['a'; 'c']);;
assert (drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]);;

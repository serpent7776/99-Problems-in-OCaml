type 'a node =
    | One of 'a
    | Many of 'a node list;;

let flatten lst =
    let rec proc acc = function
        | [] -> acc
        | One value :: tl -> proc (value :: acc) tl
        | Many ls :: tl ->
            proc (proc acc ls) tl
    in
    let rec rev acc = function
        | [] -> acc
        | hd :: tl -> rev (hd :: acc) tl
    in
    rev [] (proc [] lst);;

(* #trace flatten;; *)

assert (flatten [One 12] = [12]);;
assert (flatten [One 12; One 1] = [12; 1]);;
assert (flatten [Many [One 0]] = [0]);;
assert (flatten [One 12; Many [One 0]; One 1] = [12; 0; 1]);;
assert (flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ] = ["a"; "b"; "c"; "d"; "e"]);;

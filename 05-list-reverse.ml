let rev ls =
    let rec f lst = function
        | [] -> lst
        | hd :: tl -> f (hd :: lst) tl
    in
    f [] ls;;

assert (rev ["a" ; "b" ; "c"] = ["c"; "b"; "a"]);;
assert (rev [12; 34] = [34; 12]);;
assert (rev [2] = [2]);;

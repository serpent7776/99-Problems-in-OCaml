let length ls =
    let rec sz n = function
        | [] -> n
        | hd :: tl -> sz (n + 1) tl
    in
    sz 0 ls;;

assert (length [ "a" ; "b" ; "c"] = 3);;
assert (length [] = 0);;
assert (length [12] = 1);;

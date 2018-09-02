let is_palindrome lst =
    let rev ls =
        let rec f acc = function
            | [] -> acc
            | hd :: tl -> f (hd :: acc) tl
        in
        f [] ls
    in
    rev lst = lst;;

assert (is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ]);;
assert (not (is_palindrome [ "a" ; "b" ]));;
assert (is_palindrome [12]);;
assert (is_palindrome []);;

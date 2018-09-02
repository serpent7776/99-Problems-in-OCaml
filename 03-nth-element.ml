let rec at n = function
    | [] -> None
    | hd :: tl ->
            if n = 1 then
                Some hd
            else
                at (n - 1) tl;;

assert (at 3 [ "a" ; "b"; "c"; "d"; "e" ] = Some "c");;
assert (at 3 [ "a" ] = None);;
assert (at 1 [3; 2; 1] = Some 3);;

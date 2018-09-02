let pack_tail = function
    | [] -> []
    | hd :: tl ->
        let checkhead value = function
            | [] -> false
            | hd :: tl -> hd = value
        in
        let rec proc acc buf = function
            | [] -> buf :: acc
            | hd :: tl ->
                if checkhead hd buf then
                    proc acc (hd :: buf) tl
                else
                    proc (buf :: acc) [hd] tl
        in
        let rec rev acc = function
            | [] -> acc
            | hd :: tl -> rev (hd :: acc) tl
        in
        rev [] (proc [] [hd] tl);;

let pack_tail2 lst =
    let rec proc acc buf = function
        | [] -> []
        | [v] -> (v :: buf) :: acc
        | hd :: (md :: _ as tl) ->
            if hd = md then
                proc acc (hd :: buf) tl
            else
                proc ((hd :: buf) :: acc) [] tl
    in
    let rec rev acc = function
        | [] -> acc
        | hd :: tl -> rev (hd :: acc) tl
    in
    rev [] (proc [] [] lst);;

(*
 * let pack_notail = function
 *     | [] -> []
 *     | hd :: tl ->
 *         let proc buf acc = function
 *             | [] -> _
 *             | hd :: tl -> _
 *         in
 *         proc [] [hd] tl;;
 *)

let pack = pack_tail2;;

assert (pack [] = []);;
assert (pack [12] = [[12]]);;
assert (pack [12; 12] = [[12; 12]]);;
assert (pack [12; 14] = [[12]; [14]]);;
assert (pack [12; 12; 14] = [[12; 12]; [14]]);;
assert (pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"] = [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]]);;

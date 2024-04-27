let compress (l: string list): string list =
        let rec aux previous_letter = function
                | [] -> []
                | x :: tail when compare x previous_letter = 0 -> aux x tail
                | x :: tail -> x :: (aux x tail)
                in aux "fake" l;;

let rec compress_solution = function
        | a :: (b :: _ as t) -> if a = b then compress_solution t else a :: compress_solution t
        | smaller -> smaller;;

let compressed = compress_solution ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
print_endline (String.concat ", " compressed)

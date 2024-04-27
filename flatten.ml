type 'a node = 
        | One of 'a
        | Many of 'a node list;;

let node_easy = [One "a"; One "b"];;
let node_example = [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]];;

let flatten (list: 'a node list): 'a list =
        let rec aux acc = function
                | [] -> acc
                | One x :: t -> aux (x :: acc) t 
                | Many l :: t -> aux (aux acc l) t 
        in List.rev (aux [] list);;

let easy = flatten node_easy;;
print_endline (String.concat ", " easy)

let example = flatten node_example;;
print_endline (String.concat ", " example)

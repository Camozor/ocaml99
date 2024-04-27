let rec last (xs: 'a list): 'a option =
        match xs with
        | [] -> None
        | [x] -> Some x
        | _ :: tail -> last tail;;

let rec last_two (xs: 'a list): ('a * 'a) option = 
        match xs with
        | [] | [_] -> None
        | [x;y] -> Some (x, y)
        | _ :: tail -> last_two tail

let rec at (nth: int) (xs: 'a list): 'a option = 
        match (nth, xs) with
        | (_, []) -> None
        | (1, x :: tail) -> Some x
        | (n, x :: tail) -> at (n - 1) tail

let length (xs: 'a list): int =
        let rec aux xs n =
        match xs with
        | [] -> n
        | x :: tail -> aux tail (n + 1)
        in aux xs 0;;

let rev (xs: 'a list): 'a list =
        let rec aux (l: 'a list) (acc: 'a list) =
                match l with
                | [] -> acc
                | x :: tail -> aux tail (x :: acc)
        in aux xs [];;

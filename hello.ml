(* main *)

let average a b =
        (a +. b) /. 2.0;;

let () = print_endline (string_of_float (average 1. 2.))

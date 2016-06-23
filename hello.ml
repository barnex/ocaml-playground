(* main *)

let average a b =
        (a +. b) /. 2.0;;

let avg a b =
        let sum = a +. b in
        sum /. 2.0;;

let () = 
        print_endline (string_of_float (average 1. 2.));
        print_endline (string_of_float (avg 1. 2.));
        let i = ref 0;;
        print_endline (string_of_int i);

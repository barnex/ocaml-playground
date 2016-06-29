open Core.Std
open Phys

type test = {e1: edge; e2: edge; want: vector}

let test_intersect () =
    let tests = [
            {e1 = (edge 2. 1. 6. 3.);
             e2 = (edge 5. 1. 2. 4.);
             want = (vector 4. 2.)} 
    ] in

    let fn t =
        let (i, j) = intersect t.e1 t.e2 in
        printf "%s %s %s\n" (vec_str i) (vec_str j) (vec_str t.want);
    in
        List.iter tests ~f:fn;
        
;;

let main () =
        test_intersect();
;;

main()

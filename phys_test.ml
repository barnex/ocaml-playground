open Core.Std
open Phys

let test_intersect () =
    let e1 = edge 2. 1. 6. 3. in
    let e2 = edge 5. 1. 2. 4. in
    let (i, j) = intersect e1 e2 in
    printf "%s %s\n" (vec_str i) (vec_str j);
;;

let main () =
        test_intersect();
;;

main()

open Core.Std
open Phys

type test = {e1: edge; e2: edge; inter: vector; ok: bool}

let test_intersect () =
    let tests = [
            {e1 = (edge 2. 1. 6. 3.);
             e2 = (edge 5. 1. 2. 4.);
             inter = (vector 4. 3.);
             ok = true } 
    ] in

    let fn failed t =
        let i, ok = intersect t.e1 t.e2 in
        printf "%s %B = %s %B\n" (vec_str i) ok (vec_str t.inter) t.ok;
        failed;
    in
    let failed = (List.fold_left tests ~init:false ~f:fn) in
        assert (failed = false);
        
;;

let main () =
        test_intersect();
;;

main()

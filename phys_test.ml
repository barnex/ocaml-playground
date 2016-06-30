open Core.Std
open Phys

type intersect_test = {e1: edge; e2: edge; inter: vector; ok: bool}

let test_intersect () =
    let tests = [
            {e1 = (edge 2. 1. 6. 3.);
             e2 = (edge 5. 1. 2. 4.);
             inter = (vector 4. 2.);
             ok = true
            };
            {e1 = (edge 2. 1. 6. 3.);
             e2 = (edge 3. 3. 2. 4.);
             inter = (vector 4. 2.);
             ok = false
            };
    ] in

    let run_test n_failed t =
        let i, ok = intersect t.e1 t.e2 in
        let pass = (i = t.inter && ok = t.ok) in
        if pass then (
            n_failed + 0;
        ) else (
            printf "intersect %s %s: have %s,%B, want: %s,%B\n" 
            (edge_str t.e1) (edge_str t.e2)
            (vec_str i) ok 
            (vec_str t.inter) t.ok;
            n_failed + 1;
        )
    in
    let n_failed = (List.fold_left tests ~init:0 ~f:run_test) in
        printf "%d tests failed\n" n_failed;
        assert (n_failed = 0);
        
;;


let main () =
        test_intersect();
;;

main()

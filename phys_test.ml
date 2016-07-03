open Core.Std
open Phys


let run_tests tests test_one =
    let run_test n_failed t =
            let pass = test_one t in
            if pass then (n_failed + 0) else (n_failed + 1)
    in
    let n_failed = (List.fold_left tests ~init:0 ~f:run_test) in
        printf "%d tests failed\n" n_failed;
        assert (n_failed = 0);
;;


type intersect_test = {e1: edge; e2: edge; inter: vector; ok: bool}

let test_intersect () =
    let tests = [
            {e1 = (edge 2. 1. 6. 3.);
             e2 = (edge 5. 1. 2. 4.);
             inter = (vector 4. 2.);
             ok = true;
            };
            {e1 = (edge 2. 1. 6. 3.);
             e2 = (edge 3. 3. 2. 4.);
             inter = (vector 4. 2.);
             ok = false;
            };
    ] in

    let test_one t =
        let i, ok = intersect t.e1 t.e2 in
        let pass = (i = t.inter && ok = t.ok) in
        if not pass then
            printf "intersect %s %s: have %s,%B, want: %s,%B\n" 
            (edge_str t.e1) (edge_str t.e2)
            (vec_str i) ok 
            (vec_str t.inter) t.ok;
        pass
    in

    run_tests tests test_one;
;;


type inside_test = {tr: triangle; pt: vector; want: bool}

let test_inside () =
    let tests = [
            {tr = (tr 1. 2. 4. 5. 6. 1.);
             pt = (vector 3. 2.);
             want = true;
            };
            {tr = (tr 1. 2. 4. 5. 6. 1.);
             pt = (vector 4. 2.);
             want = true;
            };
            {tr = (tr 1. 2. 4. 5. 6. 1.);
             pt = (vector 4. 3.);
             want = true;
            };
            {tr = (tr 1. 2. 4. 5. 6. 1.);
             pt = (vector 2. 2.);
             want = true;
            };
            {tr = (tr 1. 2. 4. 5. 6. 1.);
             pt = (vector 6. 2.);
             want = false;
            };
            {tr = (tr 1. 2. 4. 5. 6. 1.);
             pt = (vector 2. 1.);
             want = false;
            };
            {tr = (tr 1. 2. 4. 5. 6. 1.);
             pt = (vector 2. 4.);
             want = false;
            };
            {tr = (tr 1. 2. 4. 5. 6. 1.);
             pt = (vector 4. 6.);
             want = false;
            };
    ] in

    let test_one t =
        let got = inside t.tr t.pt in
        let pass = (got = t.want) in
        if not pass then
            printf "inside %s %s: have %B, want: %B\n" 
            (triangle_str t.tr) (vec_str t.pt)
            got t.want;
        pass
    in

    run_tests tests test_one;
;;


type convex_area_test = {points: (vector Array.t); want: float }

let test_convex_area () = 
    let tests = [
            {points= [| (vector 1. 1.); (vector 1. 2.); (vector 3. 1.) |]; want = 1.};
            {points= [| (vector 3. 2.); (vector 5. 0.); (vector 5. 2.) |]; want = 2.};
    ] in

    let test_one t =
        let got = convex_area t.points in
        let pass = (got = t.want) in
        if not pass then (
            printf "convex_area ";
            Array.iter t.points (fun x -> printf "%s" (vec_str x));
            printf ": got: %g, want: %g\n" got t.want;
        );
        pass;
    in

    run_tests tests test_one;
;;


type center_test = {points: (vector Array.t); want: vector }

let test_center () = 
    let tests = [
            {points= [| (vector 1. 1.); (vector 2. 1.); (vector 3. 1.) |]; want = (vector 2. 1.) };
            {points= [| (vector 2. 1.); (vector 5. 0.); (vector 5. 2.) |]; want = (vector 4. 1.) };
    ] in

    let test_one t =
        let got = center t.points in
        let pass = (got = t.want) in
        if not pass then (
            printf "center ";
            Array.iter t.points (fun x -> printf "%s" (vec_str x));
            printf ": got: %s, want: %s\n" (vec_str got) (vec_str t.want);
        );
        pass;
    in

    run_tests tests test_one;
;;


let () =
        test_intersect();
        test_inside();
        test_convex_area();
        test_center();
;;

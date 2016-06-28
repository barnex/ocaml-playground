open Graphics
open Core.Std


(* 2D point. *)
type point = {
        mutable x: float; 
        mutable y: float;
}

(* Converts coordinates to int tuple, for rendering. *)
let ptint p =
        (int_of_float p.x, int_of_float p.y);
;;


(* 2D triangle *)
type triangle = {
    a: point;
    b: point;
    c: point;
}


(* Constructs a triangle from vertex coordinates. *)
let tr ax ay bx by cx cy = {
        a = {x=ax; y=ay}; 
        b = {x=bx; y=by}; 
        c = {x=cx; y=cy}; 
    };
;;


(* Draws triangle t. *)
let trdraw t = 
        draw_poly [| ptint t.a; ptint t.b; ptint t.c |];
;;


let main () =
    open_graph " 820x620";
    auto_synchronize false;

    let t1 = tr 0. 0. 100. 100. 0. 100. in
    trdraw t1;

    let start = Unix.gettimeofday() in
    Util.sleep_until (start +. 100000.);
;;

main()

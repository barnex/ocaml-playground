open Graphics
open Core.Std


(* 2D vector. *)
type vector = {
        mutable x: float; 
        mutable y: float;
}

(* Converts coordinates to int tuple, for rendering. *)
let vec_int p =
        (int_of_float p.x, int_of_float p.y);
;;


let vec_add v delta =
        v.x <- v.x +. delta.x;
        v.y <- v.y +. delta.y;
;;



(* 2D triangle *)
type triangle = {
    a: vector;
    b: vector;
    c: vector;
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
        draw_poly [| vec_int t.a; vec_int t.b; vec_int t.c |];
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

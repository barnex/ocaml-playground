open Graphics
open Core.Std


(* 2D vector. *)
type vector = {
    x: float; 
    y: float;
}


let vec vx vy = {
        x = vx;
        y = vy
    };
;;


(* Converts coordinates to int tuple, for rendering. *)
let vec_int p =
    (int_of_float p.x, int_of_float p.y);
;;


let vec_add v delta = { 
        x = v.x +. delta.x;
        y = v.y +. delta.y;
    };
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


let tr_transl t delta = {
        a = vec_add t.a delta;
        b = vec_add t.b delta;
        c = vec_add t.c delta;
    };
;;

(* Draws triangle t. *)
let tr_draw t = 
        draw_poly [| vec_int t.a; vec_int t.b; vec_int t.c |];
;;


let main () =
    open_graph " 820x620";
    auto_synchronize false;

    let t1 = tr 0. 0. 100. 100. 0. 100. in
    tr_draw t1;
    let t1 = tr_transl t1 (vec 50. 100.) in
    tr_draw t1;

    let start = Unix.gettimeofday() in
    Util.sleep_until (start +. 100000.);
;;

main()

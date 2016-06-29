open Graphics
open Core.Std
open Vec

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


(* Translate triangle t by delta. *)
let tr_transl t delta = {
        a = vec_add t.a delta;
        b = vec_add t.b delta;
        c = vec_add t.c delta;
    };
;;


type edge = {
    p1: vector;
    p2: vector;
}


(* Constructs an edge from vertex coordinates. *)
let edge x1 y1 x2 y2 = {
        p1 = {x=x1; y=y1};
        p2 = {x=x2; y=y2};
    };
;;


let intersect e1 e2 =
        let u = vec_sub e1.p2 e1.p1 in
        let v = vec_sub e2.p2 e2.p1 in
        let w = vec_sub e2.p1 e1.p1 in

        let t = (v.x*.w.y -. w.x*.v.y) /. (v.x*.u.y -. u.x*.v.y) in
        let s = (u.x*.w.y -. w.x*.u.y) /. (v.x*.u.y -. u.x*.v.y) in

        let i = vec_madd e1.p1 t u in
        let j = vec_madd e2.p1 s v in

        (i, j);
;;



(* Draws triangle t. *)
let tr_draw t = 
        draw_poly [| vec_int t.a; vec_int t.b; vec_int t.c |];
;;



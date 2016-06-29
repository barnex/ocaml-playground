open Graphics
open Core.Std


(* 2D vector. *)
type vector = {
    x: float; 
    y: float;
}


(* Vector constructor. *)
let vector vx vy = {
        x = vx;
        y = vy
    };
;;


let vec_str v = 
    sprintf "(%f, %f)" v.x v.y;
;;


(* Converts coordinates to int tuple, for rendering. *)
let vec_int p =
    (int_of_float p.x, int_of_float p.y);
;;


(* Vector sum. *)
let vec_add a b = { 
        x = a.x +. b.x;
        y = a.y +. b.y;
    };
;;

(* Vector multiply-add: a + s*b. *)
let vec_madd a s b = { 
        x = a.x +. s *. b.x;
        y = a.y +. s *. b.y;
    };
;;


(* Vector subtraction. *)
let vec_sub a b = { 
        x = a.x -. b.x;
        y = a.y -. b.y;
    };
;;


let vec_norm v = 
        sqrt (v.x*.v.x +. v.y*.v.y);
;;


let vec_dist a b =
        (vec_norm (vec_sub a b));
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

        let intersection = vec_madd e1.p1 t u in
        let intersection2 = vec_madd e2.p1 s v in

        assert ( (vec_norm (vec_sub intersection intersection2)) < 1e-6 );

        let ok = t >= 0. && t <= 1. && s >= 0. && s <= 1. in
        (intersection, ok);
;;



(* Draws triangle t. *)
let tr_draw t = 
        draw_poly [| vec_int t.a; vec_int t.b; vec_int t.c |];
;;



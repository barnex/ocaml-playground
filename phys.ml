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


(* Vector to string. *)
let vec_str v = 
    sprintf "(%g, %g)" v.x v.y;
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


(* Vector dot product. *)
let vec_dot a b =
        a.x*.b.x +. a.y*.b.y;
;;


(* Scalar * vector product. *)
let vec_mul a v = {
        x = a *. v.x;
        y = a *. v.y;
    };
;;


(* Vector length. *)
let vec_norm v = 
        sqrt (vec_dot v v)
;;


(* Re-scale vector to unit length. *)
let vec_normalize v =
        vec_mul (1. /. vec_norm v) v;
;;


(* Norm of difference between vectors a and b. *)
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


(* Triangle to string. *)
let triangle_str tr =
        sprintf "%s-%s-%s" (vec_str tr.a) (vec_str tr.b) (vec_str tr.c)
;;


(* Does vector pt lie inside triangle tr? *)
let inside tr pt =
        let x1, y1 = tr.a.x, tr.a.y in
        let x2, y2 = tr.b.x, tr.b.y in
        let x3, y3 = tr.c.x, tr.c.y in

        let det = (y2-.y3)*.(x1-.x3) +.  (x3-.x2)*.(y1-.y3) in
        let l1 =  ((y2-.y3)*.(pt.x-.x3) +. (x3-.x2)*.(pt.y-.y3)) /. det in
        let l2 =  ((y3-.y1)*.(pt.x-.x3) +. (x1-.x3)*.(pt.y-.y3)) /. det in
        let l3 = 1. -. l1 -. l2 in

        l1 >= 0. && l1 <= 1. &&
        l2 >= 0. && l2 <= 1. && 
        l3 >= 0. && l3 <= 1.;
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


(* Edge to string. *)
let edge_str e =
        sprintf "%s-%s" (vec_str e.p1) (vec_str e.p2)
;;


(* Returns the intersection point of two edges,
 * and a boolean 'ok', true when they actually intersect. *)
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



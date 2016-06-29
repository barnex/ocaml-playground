open Core.Std

(* 2D vector. *)
type vector = {
    x: float; 
    y: float;
}


(* Vector constructor. *)
let vec vx vy = {
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

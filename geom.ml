open Graphics

type point = {
        mutable x: float;
        mutable y: float
}

let point x y: point = {x=x; y=y};
;;


type rect = {
        mutable x: float;
        mutable y: float;
        mutable rx: float;
        mutable ry: float;
}

let rect x1 y1 x2 y2: rect = {
        x  = (x1+.x2)/.2.0; 
        y  = (y1+.y2)/.2.0;
        rx = (x2-.x1)/.2.0; 
        ry = (y2-.y1)/.2.0;
    }
;;


let x1 (r:rect) = 
        r.x -. r.rx;
;;

let x2 (r:rect) = 
        r.x +. r.rx;
;;

let y1 (r:rect) = 
        r.y -. r.ry;
;;

let y2 (r:rect) = 
        r.y +. r.ry;
;;

let w (r:rect) =
        2.0 *. r.rx;
;;

let h (r:rect) =
        2.0 *. r.ry;
;;


let fill_rectf x y w h: unit =
        fill_rect (int_of_float x) (int_of_float y) (int_of_float w) (int_of_float h);
;;


let draw_rectf x y w h: unit =
        draw_rect (int_of_float x) (int_of_float y) (int_of_float w) (int_of_float h);
;;

let fill_circlef x y r: unit =
        fill_circle (int_of_float x) (int_of_float y) (int_of_float r);
;;

let draw_rect2 r: unit =
     draw_rectf (x1 r) (y1 r) (w r) (h r);
;;

class boxed =
        object (self)
                val mutable x = 0.0;
                val mutable y = 0.0;
                val mutable rx = 0.0;
                val mutable ry = 0.0;
                method x1 = x -. rx;
        end
;;


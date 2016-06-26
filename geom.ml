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

let rect_of_bounds x1 y1 x2 y2: rect = {
        x  = (x1+.x2)/.2.0; 
        y  = (y1+.y2)/.2.0;
        rx = (x2-.x1)/.2.0; 
        ry = (y2-.y1)/.2.0;
    }
;;

let rect_of_center_w_h x y w h: rect = {
        x = x;
        y = y;
        rx = w /. 2.0;
        ry = h /. 2.0;
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


class boxed (cx:float) (cy:float) (rrx:float) (rry:float) =
        object (self)
                val mutable x = cx;
                val mutable y = cy;
                val mutable rx = rrx;
                val mutable ry = rry;
                method x1 () = x -. rx;
                method x2 () = x +. rx;
                method y1 () = y -. ry;
                method y2 () = y +. ry;
                method w  () = 2.0 *. rx;
                method h  () = 2.0 *. ry;
                method set_bounds x1 y1 x2 y2 =
                    x  <- (x1+.x2)/.2.0; 
                    y  <- (y1+.y2)/.2.0;
                    rx <- (x2-.x1)/.2.0; 
                    ry <- (y2-.y1)/.2.0;
                    self;
               method set_center_w_h cx cy w h=
                    x <- cx;
                    y <- cy;
                    rx <- w /. 2.0;
                    ry <- h /. 2.0;
                    self;
        end
;;


let boxed_of_bounds x1 y1 x2 y2=
        let b = new boxed 0.0 0.0 0.0 0.0 in
        b#set_bounds x1 y1 x2 y2;
;;

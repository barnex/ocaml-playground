open Graphics

type point = {
        mutable x: float;
        mutable y: float
}

let point x y: point = {x=x; y=y};
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


class boxed =
        object (self)
                val mutable x = 0.0;
                val mutable y = 0.0;
                val mutable rx = 0.0;
                val mutable ry = 0.0;
                method x () = x;
                method y () = y;
                method rx () = rx;
                method ry () = ry;
                method x1 () = x -. rx;
                method x2 () = x +. rx;
                method y1 () = y -. ry;
                method y2 () = y +. ry;
                method w  () = 2.0 *. rx;
                method h  () = 2.0 *. ry;
                method setx cx = x <- cx;
                method sety cy = y <- cy;
                method set_bounds x1 y1 x2 y2 =
                    x  <- (x1+.x2)/.2.0; 
                    y  <- (y1+.y2)/.2.0;
                    rx <- (x2-.x1)/.2.0; 
                    ry <- (y2-.y1)/.2.0;
                    self;
               method set_center_w_h cx cy w h =
                    x <- cx;
                    y <- cy;
                    rx <- w /. 2.0;
                    ry <- h /. 2.0;
                    self;
               method transl dx dy =
                    x <- x +. dx;
                    y <- y +. dy;
        end
;;


let boxed_of_bounds x1 y1 x2 y2=
        let b = new boxed in
        b#set_bounds x1 y1 x2 y2;
;;


let boxed_of_center_w_h x y w h=
        let b = new boxed in
        b#set_center_w_h x y w h;
;;

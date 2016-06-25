open Graphics

type point = {
        mutable x: float;
        mutable y: float
}

let point x y: point = {x=x; y=y};
;;


type rect = {
        mutable x1: float;
        mutable y1: float;
        mutable x2: float;
        mutable y2: float;
}

let rect x1 y1 x2 y2: rect =
        {x1=x1; y1=y1; x2=x2; y2=y2};
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

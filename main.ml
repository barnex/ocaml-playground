open Geom
open Graphics
open Util

let q = 10.0

let boxw = 800.0
let boxh = 600.0
let box = rect_of_bounds q q (boxw+.q) (boxh+.q)

let pad_width  = q
let pad_height = 10.0 *. q
let pad_speed  = 8.0
let pad1 = rect_of_center_w_h ( 0.0      +.4.0*.q) (box#ry()) (pad_width) (pad_height)
let pad2 = rect_of_center_w_h ((box#x2()) -. 4.0*.q) (box#ry()) (pad_width) (pad_height)

let ball_r = 3.0 *. q
let ball   = rect_of_center_w_h (box#x()) (box#y()) ball_r ball_r
let v      = point 3.0 2.0

let bg_col = black
let fg_col = white
let pad1_col = red
let pad2_col = blue

let draw_ball () =
    set_color red;
    let r = 3.0*.ball_r/.3.0 in
    fill_circlef (ball#x()) (ball#y()) (r/.2.0);

    set_color 0xffa500;
    let r = 2.0*.ball_r/.3.0 in
    fill_circlef (ball#x()) (ball#y()) (r/.2.0);

    set_color yellow;
    let r = 1.0*.ball_r/.3.0 in
    fill_circlef (ball#x()) (ball#y()) (r/.2.0);
;;


let draw_rect b =
    draw_rectf (b#x1()) (b#y1()) (b#w()) (b#h()); 
;;


let fill_rect b =
    fill_rectf (b#x1()) (b#y1()) (b#w()) (b#h()); 
;;


let clear () =
        set_color bg_col;
        Graphics.fill_rect 0 0 (size_x()) (size_y());
;;

let draw (): unit =
    
    clear();

    set_color fg_col;
    draw_rect box;

    draw_ball ();
    set_color pad1_col;
    fill_rect pad1;

    set_color pad2_col;
    fill_rect pad2;
;;


let mouse_posf (): float * float =
      let x, y = mouse_pos () in
      ((float_of_int x), (float_of_int y));
;;


let debug_status (): unit =
    Debug.start ();
    Debug.point "v"         (v.x, v.y);
;;


let move_ball (): unit =
    ball#transl v.x v.y;
    if (ball#x1()) <= (box#x1()) || (ball#x2()) >= (box#x2()) then v.x <- -.v.x;
    if (ball#y1()) <= (box#y1()) || (ball#y2()) >= (box#y2()) then v.y <- -.v.y;

    let pad = pad1 in
    if v.x < 0.0 && (ball#x1()) <= (pad#x2()) && (ball#y()) <= (pad#y2()) && (ball#y()) >= (pad#y1()) then (
            v.x <- -.v.x;
    );

    let pad = pad2 in
    if v.x > 0.0 && (ball#x2()) >= (pad#x1()) && (ball#y()) <= (pad#y2()) && (ball#y()) >= (pad#y1()) then (
            v.x <- -.v.x;
    );
;;



let move_pad1 (): unit =
    let x,y = mouse_posf() in
    let pad = pad1 in
    pad#sety (limit (y) ((box#y1())+.pad#ry()) ((box#y2())-.pad#ry()));
    pad#setx (limit (x) (box#x1()) (box#x1()+.100.0));
;;


let aiv = ref 0.0

let move_pad2 (): unit =
        let pad = pad2 in

        if v.x > 0.0 then (
                if ball#y() > pad#y()+. pad#ry() then aiv := !aiv +. 1.0;
                if ball#y() < pad#y()-. pad#ry() then aiv := !aiv -. 1.0;
        ) else (
                aiv := !aiv /. 2.0;
        );

        aiv := limit (!aiv) (-.pad_speed) (pad_speed);
        pad#sety (limit (pad#y() +. !aiv) ((box#y1())+.pad#ry()) ((box#y2())-.pad#ry()));
;;


let main (): unit =
    open_graph " 820x620";
    auto_synchronize false; 

    for i = 1 to 10000000 do 
        let start = Unix.gettimeofday() in

        move_pad1();
        move_pad2();
        move_ball ();

        draw ();
        debug_status ();

        if key_pressed () then
                Debug.string (String.make 1 (read_key()));

        synchronize ();
        Util.sleep_until(start +. (1.0/.60.0));
    done;
;; 


main ()
    

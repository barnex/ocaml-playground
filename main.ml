open Graphics
open Geom
open Debug

let boxw = 500
let boxh = 300
let q = 10
let texth = 16
let padw = q
let padh = 10*q
let pad_speed = 4

let v = point 2 1
let ball = point (boxw/2) (boxh/2)

let pad1 = point (2*q) (boxh/2)
let pad2 = point (boxw-q) (boxh/2)


let draw_pad pad: unit =
    fill_rect (pad.x-padw/2) (pad.y-padh/2) padw padh
;;


let draw (): unit =
    clear_graph ();

    set_color black;
    draw_rect q q (boxw - q) (boxh - q);
    fill_circle ball.x ball.y q;

    set_color red;
    draw_pad pad1;

    set_color blue;
    draw_pad pad2;
;;


let debug_status (): unit =
    debug_start ();
    debug_point "mouse_pos" (mouse_pos ());
    debug_point "ball"      (ball.x, ball.y);
    debug_point "v"         (v.x, v.y);
;;


let rec sleep_until until: unit =
     let secs = (until -. Unix.gettimeofday()) in
     if secs > 0.0 then
         try ignore (Unix.select [] [] [] secs) with
         Unix.Unix_error(Unix.EINTR, "select", "") -> sleep_until until
;;


let move_ball (): unit =
        ball.x <- ball.x + v.x;
        ball.y <- ball.y + v.y;
        v.x <- if ball.x <= 2*q || ball.x >= boxw - q then -v.x else v.x;
        v.y <- if ball.y <= 2*q || ball.y >= boxh - q then -v.y else v.y;
;;


let limit x min max: int  =
        if x < min then min else
        if x > max then max else x;
;;

let move_pad1 (): unit =
        let x,y = mouse_pos() in
        if y > pad1.y then pad1.y <- pad1.y + pad_speed;
        if y < pad1.y then pad1.y <- pad1.y - pad_speed;
        pad1.y <- (limit (pad1.y) (padh/2+q) (boxh-padh/2));
;;

let aiv = ref 0

let move_pad2 (): unit =
        let y = ball.y in

        if v.x > 0 then
        (if y > pad2.y + padh/2 then aiv := !aiv + 1;
        if y < pad2.y - padh/2 then aiv := !aiv -1; )
        else aiv := !aiv / 2;

        aiv := if !aiv > pad_speed then pad_speed else !aiv;
        aiv := if !aiv < -pad_speed then -pad_speed else !aiv;

        pad2.y <- (limit (pad2.y + !aiv) (padh/2+q) (boxh-padh/2));
;;


let main (): unit =
    open_graph " 510x310";
    auto_synchronize false; 

    for i = 1 to 10000000 do 
        let start = Unix.gettimeofday() in

        move_pad1();
        move_pad2();
        move_ball ();

        draw ();
        debug_status ();

        if key_pressed () then
                debug_string (String.make 1 (read_key()));

        synchronize ();
        sleep_until(start +. (1.0/.60.0));
    done;
;; 


main ()
    

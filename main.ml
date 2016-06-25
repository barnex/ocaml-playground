open Geom
open Graphics
open Util

let boxw = 500.0
let boxh = 300.0
let q = 10.0
let padw = q
let padh = 10.0 *. q
let pad_speed = 4.0

let v = point 2.0 1.0
let ball = point (boxw /. 2.0) (boxh /. 2.0)

let pad1 = point (2.0 *. q) (boxh /. 2.0)
let pad2 = point (boxw -. q) (boxh /. 2.0)


let draw_pad pad: unit =
    fill_rectf (pad.x -. padw /. 2.0) (pad.y -. padh /. 2.0) padw padh
;;


let draw (): unit =
    clear_graph ();

    set_color black;
    draw_rectf q q (boxw -. q) (boxh -. q);
    fill_circlef ball.x ball.y q;

    set_color red;
    draw_pad pad1;

    set_color blue;
    draw_pad pad2;
;;


let mouse_posf (): float * float =
        let x, y = mouse_pos () in
        ((float_of_int x), (float_of_int y));
;;


let debug_status (): unit =
    Debug.start ();
    Debug.point "mouse_pos" (mouse_posf ());
    Debug.point "ball"      (ball.x, ball.y);
    Debug.point "v"         (v.x, v.y);
;;



let move_ball (): unit =
        ball.x <- ball.x +. v.x;
        ball.y <- ball.y +. v.y;
        v.x <- if ball.x <= 2.0*.q || ball.x >= boxw -. q then -.v.x else v.x;
        v.y <- if ball.y <= 2.0*.q || ball.y >= boxh -. q then -.v.y else v.y;
;;



let move_pad1 (): unit =
        let x,y = mouse_posf() in
        if y > pad1.y then pad1.y <- pad1.y +. pad_speed;
        if y < pad1.y then pad1.y <- pad1.y -. pad_speed;
        pad1.y <- (limit (pad1.y) (padh/.2.0+.q) (boxh-.padh/.2.0));
;;

let aiv = ref 0.0

let move_pad2 (): unit =
        let y = ball.y in

        if v.x > 0.0 then (
                if y > pad2.y +. padh/.2.0 then aiv := !aiv +. 1.0;
                if y < pad2.y -. padh/.2.0 then aiv := !aiv -. 1.0;
        ) else (
                aiv := !aiv /. 2.0;
        );

        aiv := if !aiv > pad_speed then pad_speed else !aiv;
        aiv := if !aiv < -.pad_speed then -.pad_speed else !aiv;

        pad2.y <- (limit (pad2.y +. !aiv) (padh/.2.0+.q) (boxh-.padh/.2.0));
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
                Debug.string (String.make 1 (read_key()));

        synchronize ();
        Util.sleep_until(start +. (1.0/.60.0));
    done;
;; 


main ()
    

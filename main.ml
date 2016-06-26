open Geom
open Graphics
open Util

let q = 10.0

let boxw = 500.0
let boxh = 300.0
let box = rect_of_bounds q q (boxw+.q) (boxh+.q)


let padw = q
let padh = 10.0 *. q
let padr = padh /. 2.0
let pad_speed = 4.0

let v    = point 3.0           2.0
let ball = point box.x box.y

let pad1 = point (4.0 *. q)           (box.ry)
let pad2 = point ((x2 box) -. 4.0*.q) (box.ry)


let draw_pad (pad: point): unit =
    fill_rectf (pad.x -. padw /. 2.0) (pad.y -. padr) padw padh
;;


let draw (): unit =
    clear_graph ();

    set_color black;
    draw_rect2 box;
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
    Debug.point "box x1, y1" ((x1 box), (y1 box));
    Debug.point "box x2, y2" ((x2 box), (y2 box));
    Debug.point "v"         (v.x, v.y);
;;



let move_ball (): unit =
    ball.x <- ball.x +. v.x;
    ball.y <- ball.y +. v.y;
    v.x <- if ball.x <= (x1 box) || ball.x >= (x2 box) then -.v.x else v.x;
    v.y <- if ball.y <= (y1 box) || ball.y >= (y2 box) then -.v.y else v.y;

    let top = pad1.y +. padr in
    let bottom = pad1.y -. padr in
    if v.x < 0.0 && ball.x <= pad1.x +. q && ball.y <= top && ball.y >= bottom then v.x <- -.v.x;

    let top = pad2.y +. padr in
    let bottom = pad2.y -. padr in
    if v.x > 0.0 && ball.x >= pad2.x -. q && ball.y <= top && ball.y >= bottom then v.x <- -.v.x;
;;



let move_pad1 (): unit =
    let x,y = mouse_posf() in
    if y > pad1.y then pad1.y <- pad1.y +. pad_speed;
    if y < pad1.y then pad1.y <- pad1.y -. pad_speed;
    pad1.y <- (limit (pad1.y) (padr+.q) ((y2 box) -.padr));
;;


let aiv = ref 0.0

let move_pad2 (): unit =
        let y = ball.y in

        if v.x > 0.0 then (
                if y > pad2.y +. padr then aiv := !aiv +. 1.0;
                if y < pad2.y -. padr then aiv := !aiv -. 1.0;
        ) else (
                aiv := !aiv /. 2.0;
        );

        aiv := if !aiv > pad_speed then pad_speed else !aiv;
        aiv := if !aiv < -.pad_speed then -.pad_speed else !aiv;

        pad2.y <- (limit (pad2.y +. !aiv) (padr+.q) (boxh-.padr));
;;


let main (): unit =
    open_graph " 520x320";
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
    

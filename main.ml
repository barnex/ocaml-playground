open Graphics

type point = {mutable x: int; mutable y: int}

let point x y: point =
        {x=x; y=y};
;;


let boxw = 500
let boxh = 300
let q = 10
let texth = 16

let v = point 2 1
let ball = point (boxw/2) (boxh/2)


let debug_point label xy: unit =
        let x, y = xy in
        draw_string label;
        draw_string " ";
        draw_string (string_of_int x);
        draw_string " ";
        draw_string (string_of_int y);
        moveto (2*q) (current_y() - texth);
;;


let debug_status (): unit =
        set_color blue;
        moveto (2*q) (boxh - 2*q);

        debug_point "mouse_pos" (mouse_pos ());
        debug_point "ball"      (ball.x, ball.y);
        debug_point "v"         (v.x, v.y);
;;


let draw (): unit =
    clear_graph ();

    set_color black;
    draw_rect q q (boxw - q) (boxh - q);
    fill_circle ball.x ball.y q;
    moveto (ball.x + q) ball.y;
    draw_string "Felix";

    debug_status ();

;;


let rec sleep_until until: unit =
     let secs = (until -. Unix.gettimeofday()) in
     if secs > 0.0 then
         try ignore (Unix.select [] [] [] secs) with
         Unix.Unix_error(Unix.EINTR, "select", "") -> sleep_until until
;;



let main (): unit =
    open_graph " 510x310";
    auto_synchronize false; 

    for i = 1 to 10000000 do 
        let start = Unix.gettimeofday() in
        ball.x <- ball.x + v.x;
        ball.y <- ball.y + v.y;
        v.x <- if ball.x <= 2*q || ball.x >= boxw - q then -v.x else v.x;
        v.y <- if ball.y <= 2*q || ball.y >= boxh - q then -v.y else v.y;
        draw ();
        sleep_until(start +. (1.0/.60.0));
        synchronize ();
    done;
;; 


main ()
    

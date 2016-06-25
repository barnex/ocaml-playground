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
let ballx = ref (boxw/2)
let bally = ref (boxh/2)


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
        debug_point "ball"      (!ballx, !bally);
        debug_point "v"         (v.x, v.y);
;;


let draw (): unit =
    clear_graph ();

    set_color black;
    draw_rect q q (boxw - q) (boxh - q);
    fill_circle !ballx !bally q;
    moveto (!ballx + q) !bally;
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
        ballx := !ballx + v.x;
        bally := !bally + v.y;
        v.x <- if !ballx <= 2*q || !ballx >= boxw - q then -v.x else v.x;
        v.y <- if !bally <= 2*q || !bally >= boxh - q then -v.y else v.y;
        draw ();
        sleep_until(start +. (1.0/.60.0));
        synchronize ();
    done;
;; 


main ()
    

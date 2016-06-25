open Graphics

let boxw = 500
let boxh = 300
let q = 10
let texth = 16

let ballx = ref (boxw/2)
let bally = ref (boxh/2)
let vx = ref 2
let vy = ref 1

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
        debug_point "v"         (!vx, !vy);
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
    open_graph "";
    auto_synchronize false; 

    for i = 1 to 10000000 do 
        let start = Unix.gettimeofday() in
        ballx := !ballx + !vx;
        bally := !bally + !vy;
        vx := if !ballx <= 2*q || !ballx >= boxw - q then -(!vx) else (!vx);
        vy := if !bally <= 2*q || !bally >= boxh - q then -(!vy) else (!vy);
        draw ();
        sleep_until(start +. (1.0/.60.0));
        synchronize ();
    done;
;; 


main ()
    

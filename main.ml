open Graphics

let boxw = 500
let boxh = 300
let q = 10

let ballx = ref (boxw/2)
let bally = ref (boxh/2)
let vx = ref 2
let vy = ref 1

let draw (): unit =
    clear_graph ();

    set_color black;
    draw_rect q q (boxw - q) (boxh - q);
    fill_circle !ballx !bally q;

    synchronize ();;

let rec sleep secs: unit =
        try ignore (Unix.select [] [] [] 0.01) with
        Unix.Unix_error(Unix.EINTR, "select", "") -> sleep secs;;

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
    done;
    ignore(wait_next_event [Mouse_motion; Button_down; Button_up; Key_pressed]);

;; main ()
    

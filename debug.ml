open Graphics

let q = 10
let texth = 10

let debugln (): unit =
    moveto (2*q) (current_y() - texth);
;;


let debug_string str: unit=
    draw_string str;
    debugln();
;;
    

let debug_point label xy: unit =
    let x, y = xy in
    draw_string label;
    draw_string " ";
    draw_string (string_of_int x);
    draw_string " ";
    draw_string (string_of_int y);
    debugln ();
;;


let debug_start (): unit=
    set_color green;
    moveto (2*q) ((size_y()) - 3*q);
;;




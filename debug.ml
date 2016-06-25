open Graphics

let q = 10
let texth = 10

let ln (): unit =
    moveto (2*q) (current_y() - texth);
;;


let string str: unit=
    draw_string str;
    ln();
;;
    

let point label xy: unit =
    let x, y = xy in
    draw_string label;
    draw_string " ";
    draw_string (string_of_int x);
    draw_string " ";
    draw_string (string_of_int y);
    ln ();
;;


let start (): unit=
    set_color green;
    moveto (2*q) ((size_y()) - 3*q);
;;




open Graphics

let main (): unit =
    open_graph "";
    auto_synchronize false; 
    set_color black;

    let q = 10 in
    fill_rect q q (2*q) (10*q);

    synchronize ();

    ignore(wait_next_event [Mouse_motion; Button_down; Button_up; Key_pressed]);

;; main ()
    

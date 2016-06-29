open Phys
open Graphics

let main () =
    open_graph " 820x620";
    auto_synchronize false;

    let t1 = tr 0. 0. 100. 100. 0. 100. in
    tr_draw t1;
    let t1 = tr_transl t1 (vector 50. 100.) in
    tr_draw t1;

    let start = Unix.gettimeofday() in
    Util.sleep_until (start +. 100000.);
;;

main()

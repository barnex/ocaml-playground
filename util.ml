
let rec sleep_until until: unit =
     let secs = (until -. Unix.gettimeofday()) in
     if secs > 0.0 then
         try ignore (Unix.select [] [] [] secs) with
         Unix.Unix_error(Unix.EINTR, "select", "") -> sleep_until until
;;


let limit x min max: int  =
        if x < min then min else
        if x > max then max else x;
;;

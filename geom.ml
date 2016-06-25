
type point = {mutable x: int; mutable y: int}

let point x y: point =
        {x=x; y=y};
;;


type rect = {mutable x1: int; mutable y1: int; mutable x2: int; mutable y2: int}

let rect x1 y1 x2 y2: rect =
        {x1=x1; y1=y1; x2=x2; y2=y2};
;;

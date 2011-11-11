open Braid;;

let trace b = 
    Braid_Print.display b;
    Graphics.read_key ();();;
    
Braid_Reverse.split ~trace:trace {word = [2;-2;2;-2;-3;1;-4;2]; size = 5}

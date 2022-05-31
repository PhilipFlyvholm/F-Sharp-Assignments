(* Load this file into the interacive envorinment
   (select all and Alt-Enter in VS).
   
   Cut-and pasting these lines will typically not work unless you provide
   the entire path in the #load command. 

   Some IDEs may still complain about the path, place the full path here if that is the case.
*)

#load "Exam.fs"
open Exam2020_2;;

let fib x =
         let rec aux acc1 acc2 =
             function
             | 0 -> acc1
             | x -> aux acc2 (acc1 + acc2) (x - 1)
         aux 0 1 x
let fibll1 = init fib
let fibll2 = unfold (fun (acc1, acc2) -> (acc1, (acc2, acc1 + acc2))) (0, 1)
let fibll3 = unfold (fun (acc1, acc2) -> (acc1, (acc2, add acc1 acc2))) ([0], [1])

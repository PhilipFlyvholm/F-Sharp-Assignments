open Assignment4.MultiSet

printf "%b\n" (isEmpty empty)
printf "%d\n" (size empty)
printf "%d\n" (size (add "a" 5u empty))
printf "%d\n" (size (add "b" 50u (add "a" 5u empty)))
printf "%d\n" (size (remove "a" 6u (add "b" 50u (add "a" 5u empty))))
printf "%d\n" (size (remove "b" 5u (remove "a" 6u (add "b" 50u (add "a" 5u empty)))))
//toList test
printf "tolist test: %A\n" (toList (add "b" 2u (add "a" 5u empty)))
//Union test
printf "Union 1 test: %A\n" (toList (union (add "b" 2u empty) (add "a" 5u empty)))
printf "Union 2 test: %A\n" (toList (union (add "a" 1u (add "b" 2u empty)) (add "a" 5u empty)))
//Sum test
printf "Sum test: %A\n" (toList (sum (add "a" 1u (add "b" 2u empty)) (add "a" 5u empty)))
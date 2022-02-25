open Assignment4.MultiSet

printf "%b\n" (isEmpty empty)
printf "%d\n" (size empty)
printf "%d\n" (size (add "a" 5u empty))
printf "%d\n" (size (add "b" 50u (add "a" 5u empty)))
printf "%d\n" (size (remove "a" 6u (add "b" 50u (add "a" 5u empty))))
printf "%d\n" (size (remove "b" 5u (remove "a" 6u (add "b" 50u (add "a" 5u empty)))))
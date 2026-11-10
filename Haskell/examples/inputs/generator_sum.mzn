array[1..3] of var 1..9: a;

constraint a[1] = 3;
constraint a[2] = 7;
constraint a[3] = 5;
constraint sum(i in 1..3)(a[i]) = 15;

solve satisfy;

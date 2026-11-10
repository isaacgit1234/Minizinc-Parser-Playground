array[1..3] of var 0..5: a;

constraint a[1] = 5;
constraint a[2] = 5;
constraint sum(a) = 10;

solve satisfy;

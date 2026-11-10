array[1..3] of var 1..9: a;

constraint a[1] = 4;
constraint a[2] = 9;
constraint a[3] = 2;
constraint max(a) = 9;
constraint min(a) = 2;

solve satisfy;

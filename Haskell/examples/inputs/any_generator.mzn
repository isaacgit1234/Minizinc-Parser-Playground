array[1..3] of var 1..3: b;

constraint b[1] = 1;
constraint b[2] = 1;
constraint any(i in 1..3)(b[i] = 3);

solve satisfy;

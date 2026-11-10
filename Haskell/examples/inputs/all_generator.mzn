array[1..4] of var 0..1: a;

constraint all(i in 1..4)(a[i] = 1);

solve satisfy;

int: n = 8;
array[1..n] of var 1..n: q;

constraint allDifferent(q);
constraint allDifferent(i in 1..n)(q[i] + i);
constraint allDifferent(i in 1..n)(q[i] - i);

solve minimize q[1];

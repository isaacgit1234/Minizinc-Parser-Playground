var 1..3: x;
int: n = 2;
var 1..n: y;

constraint x < y;

solve satisfy;

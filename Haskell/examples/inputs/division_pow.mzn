int: n = 2;
var 1..10: x;

constraint x^n = 9;
constraint x/2 = 1;

solve satisfy;

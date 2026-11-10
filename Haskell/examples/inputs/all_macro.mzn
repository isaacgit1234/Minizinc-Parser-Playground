var 1..5: x;
var 1..5: y;

constraint all([x = 2, y = 3]);

solve satisfy;

var 1..5: x;
var 1..5: y;

constraint all([x+1=2, y+2=4]);

solve satisfy;

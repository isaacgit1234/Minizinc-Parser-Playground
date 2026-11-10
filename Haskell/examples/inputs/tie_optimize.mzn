var 1..3: x;
var -3..3: y;

constraint y = -x;

solve maximize x + y;

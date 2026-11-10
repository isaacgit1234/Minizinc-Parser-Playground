var 1..5: x;
var 1..5: y;
var 1..5: z;

constraint x = 1;
constraint y = 2;
constraint z = 3;
constraint sum([x, y, z]) = 6;
constraint allDifferent([x, y, z]);

solve satisfy;

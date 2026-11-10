var 0..100: b;
var 0..100: c;

constraint 250*b + 200*c <= 8000;
constraint 2*b  <= 11;
constraint 75*b + 150*c <= 3000;
constraint 100*b + 150*c <= 1500;
constraint 75*c <= 800;

solve maximize 400*b + 450*c;
var x;
x = 10;
var y = 3 * x + 5;
while (y % x != 3)
  y = y + 1;
if (x * x > y)
  return x * x;
x = x + 1;
return x;
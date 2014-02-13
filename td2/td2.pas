program bonjour;
var
   x, y : integer;
   b : boolean;
begin
   x := 5;
   y := 3*x;
   b := y = 7;
   if b or (x <> 7) then
      writeln(x)
   else
   begin
      x := 3*y;
      writeln(y)
   end
end.


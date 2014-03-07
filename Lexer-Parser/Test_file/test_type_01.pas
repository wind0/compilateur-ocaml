program Test ;
var
	x,y : integer;
	b1 : boolean;
	b2 : boolean;
	z : integer;
begin
	x := 5 + 4;
	y := x + 8;
	
	b1 := x > y;
	b2 := b1;
	
	if b2 then 
		z := y
	else
		z := x;

	if z = x then
		b1 := b2
	else
		x := 7 + 9
			
end.
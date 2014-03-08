program Level2 ;

type
	int = integer;
	bool = boolean;

var
	x,y : integer;
	b1 : boolean;
	b2 : boolean;
	z : integer;
	maxi : integer;
	maxboole : boolean;

procedure Test (x : integer) ;
begin
	x := 10
end;

function Testfunc : integer;
var
	val : integer;
begin
	val := 10 + 4;
	Testfunc := val
end;

function Max (x : integer; y : integer) : integer;
var
	maxi : integer;
begin
	if x > y then
		maxi := x
	else
		maxi := y;
	Max := maxi
end;

function Maxboolx (x : integer; y : integer) : boolean;
var
	maxboole : boolean;
begin
	maxboole := x > y;
	Maxboolx := maxboole
end;

begin
	x := 5 + 4;
	y := x + 8;
	
	b1 := x > y;
	b2 := b1;

	Test(x);
	
	if b2 then 
		z := y
	else
		z := x;

	if z = x then
		b1 := b2
	else
		x := 7 + 9;

	x := Testfunc;

	maxi := Max(x,y);

	maxboole := Maxboolx(x,y)
			
end.
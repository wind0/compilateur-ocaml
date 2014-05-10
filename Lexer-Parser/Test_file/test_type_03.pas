program Level3 ;

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
	i,j : integer;

procedure Test (a : integer) ;
begin
	a := 10
end;

function Testfunc : integer;
var
	val : integer;
begin
	val := 10 + 4;
	Testfunc := val
end;

function Max (x1 : integer; y1 : integer) : integer;
var
	maxi : integer;
begin
	if x1 > y1 then
		maxi := x1
	else
		maxi := y1;
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

	maxboole := Maxboolx(x,y);

	i := 10;

	while i > 0 do
		i := i - 1;

	repeat 
		i := i + 1;
		x := x + 1;
		b1 := x > y
	until
		i > 10;

	for j := 0 to 10 do
		x := y;	

	b1 := x = y

end.

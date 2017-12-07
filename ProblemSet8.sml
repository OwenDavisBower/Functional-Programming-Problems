(* 4.10.3 *)

fun pow(b,n) =
	let fun helper(a, _, 0) = a
  		  | helper(a, b, n) = if (n mod 2 = 0) then helper(a, b*b, n div 2)
  								  			   else helper(a*b, b, n-1);
	in helper(1, b, n)
	end;

(* 4.10.4 *)

fun mul(b, c) =
	let fun helper(a, _, 0) = a
   		  | helper(a, b, c) = if c mod 2 = 0 then helper(a, b+b, c div 2)
  								  else helper(a + b, b, c-1);
  	in helper(0, b, c)
  	end;


(* 4.10.6 *)
(* Floor log and remainder log, this is a tricky one! *)

fun frlog(a, 0) = (0, 0)
  | frlog(a, b) =
	let fun helper(s, n) = if s > a then (n-1, a - s div b)
									   else helper(s*b, n+1)
	in helper(b, 1)
	end;

(* 5.3.12 *)

(* image(2, [(2,1), (3,6), (4,5), (2,4), (2,7)]); *)

fun image(_, []) = []
  | image(x, (a,b)::rest) = if a=x then b::image(x, rest) else image(x, rest);

(* 5.3.13 *)

fun addImage(_, []) = []
  | addImage(x, y::yrest) = (x, y)::addImage(x, yrest);

(* 5.3.14 *)
(* Another tough one *)

compose([(1,3),(2,1),(3,5),(6,3)], [(3,2), (1,3), (5,4), (3,4)]);

fun image(_, []) = []
  | image(x, (a,b)::rest) = if a=x then b::image(x, rest) else image(x, rest);

fun addImage(_, []) = []
  | addImage(x, y::yrest) = (x, y)::addImage(x, yrest);

fun compose([], _) = []
  | compose((a,b)::arest, c) = addImage(a, image(b, c))@compose(arest, c);






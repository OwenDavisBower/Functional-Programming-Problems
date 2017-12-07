fun map(f, []) = []
  | map(f, a::rest) = f(a)::map(f, rest);

fun filter(f, []) = []
  | filter(f, a::rest) = if f(a) then a::filter(f, rest) else filter(f, rest);

fun foldl(f, seed, []) = seed
  | foldl(f, seed, a::rest) = foldl(f, f(seed, a), rest);

fun foldr(f, seed, []) = seed
  | foldr(f, seed, a::rest) = f(a, foldr(f, seed, rest));

(* 7.3.3 *)

fun makeListExtractor(xx) =
	let fun findNth([], x) = ~1
		  | findNth(a::rest, 1) = a
		  | findNth(a::rest, x) = findNth(rest, x-1);
		fun extract(x) = findNth(xx, x);
	in
		extract
	end;

fun makeListExtractor(xx) =
	let fun findNth([], x) = ~1
		  | findNth(a::rest, 1) = a
		  | findNth(a::rest, x) = findNth(rest, x-1);
	in
		fn(x) => findNth(xx, x)
	end;

(* 7.3.4 *)

fun makeListMul(x) =
	let fun listMul([]) = []
		  | listMul(a::rest) = a*x::listMul(rest);
	in
		listMul
	end;

(* 7.3.8 *)

fun transformTree(Leaf(x), func) = Leaf(func(x))
  | transformTree(Internal(x, left, right), func) = Internal(func(x), transformTree(left, func), transformTree(right, func));

(* 7.5.a *)

fun sumPairs(xx) = map(fn(x,y) => x + y, xx);

(* 7.5.b *)

fun keepLessThan(x, xx) = filter(fn(y) => x > y, xx);

(* 7.5.c *)

fun halveEvens(xx) = map(fn(x) => x div 2, 
					 filter(fn(x) => x mod 2 = 0, xx));

(* 7.14.a *)

fun catAll(xx) = foldr(fn(x, y) => x @ y, [], xx);

(* 7.14.b *)

(* Just so you know, the documentation for "foldl" on the additional problems page does not match up with how foldl actually works on the online cheker. I believe the online checker uses the foldl function from the book, while the documentation shows the foldl function which we fixed in class *)

fun numEvens(xx) = foldl(fn(x, y) => if x mod 2 = 0 then y+1 else y, 0, xx);



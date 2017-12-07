(* 5.6.1 *)
fun isSymmetric(relation) = 
    let fun testOnePair((a,b), []) = false
          | testOnePair((a,b), (c,d)::rest) = (a = d andalso b = c) orelse testOnePair((a,b), rest);
        fun test([]) = true
          | test((a,b)::rest) = testOnePair((a,b), relation) andalso test(rest);
    in
    	test(relation)
	end;

(* 5.6.3 *)
fun counterSymmetric(relation) = 
    let fun testOnePair((a,b), []) = [(b,a)]
          | testOnePair((a,b), (c,d)::rest) = if (a = d andalso b = c) then [] 												 else testOnePair((a,b), rest);
        fun test([]) = []
          | test((a,b)::rest) = testOnePair((a,b), relation) @ test(rest);
    in
    	test(relation)
	end;

(* 5.7.5 *)
fun contains(x, []) = false
  | contains(x, y::rest) = x=y orelse contains(x, rest);

fun makeNoRepeats([]) = []
  | makeNoRepeats(x::rest) = if contains(x, rest) then makeNoRepeats(rest)
  												  else x::makeNoRepeats(rest);

fun symmetricClosure(relation) =
	if isSymmetric(relation) then relation
							 else symmetricClosure(makeNoRepeats(counterSymmetric(relation)) @ relation);
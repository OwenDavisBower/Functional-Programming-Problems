(* 3.3.5 *)

fun areEven([]) = []
  | areEven(x::rest) = (x mod 2 = 0)::areEven(rest);

(* 3.3.6 *)

fun containsOne([]) = false
  | containsOne(x::rest) = x = 1 orelse containsOne(rest);

(* 3.7.7 *)

fun contains(x, []) = false
  | contains(x, y::rest) = x=y orelse contains(x, rest);

fun intersection([], list2) = []
  | intersection(x::rest, list2) = if contains(x, list2) then x::intersection(rest, list2)
  														 else intersection(rest, list2);

(* 3.7.12 *)

fun listMin([x]) = x
  | listMin(x::rest) = if x < listMin(rest) then x
  											else listMin(rest);

fun contains(x, []) = false
  | contains(x, y::rest) = x=y orelse contains(x, rest);

(* 3.7.8 *)

fun difference([], yy) = []
  | difference(x::xrest, yy) = if contains(x, yy) then difference(xrest, yy)
  												  else x::difference(xrest, yy);

(* 3.7.9 *)

fun makeNoRepeats([]) = []
  | makeNoRepeats(x::rest) = if contains(x, rest) then makeNoRepeats(rest)
  												  else x::makeNoRepeats(rest);

(* 3.7.13 *)

fun sort([]) = []
  | sort(xx) = listMin(xx)::sort(removeFirst(listMin(xx), xx));

(* 3.13.2 *)

fun allHaveDouble([], yy) = true
  | allHaveDouble(x::xrest, yy) =
    let fun hasDouble([]) = false
          | hasDouble(y::yrest) = (x * 2 = y) orelse hasDouble(yrest);
    in
   	  hasDouble(yy) andalso allHaveDouble(xrest, yy)
   	end;

(* 3.13.4 *)

fun hasDivisorOfAll(xx) =
  let fun isDivisor(x, []) = true
  		| isDivisor(x, y::yrest) = (y mod x) = 0 andalso isDivisor(x, yrest);
  	  fun checkEach([]) = false
  		| checkEach(x::rest) = isDivisor(x, xx) orelse checkEach(rest);
  in
    checkEach(xx)
  end;

(* 3.13.5 *)

fun hasCommonElement([], yy) = false
  | hasCommonElement(x::rest, yy) =
  	let fun hasCommon([]) = false
		  | hasCommon(y::yrest) = (x = y) orelse hasCommon(yrest);
	in
	  hasCommon(yy) orelse hasCommonElement(rest, yy)
	end;


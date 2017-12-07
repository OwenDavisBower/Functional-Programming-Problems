(* 2.2.3 *)
fun doubler([]) = []
  | doubler(x::rest) = x*2::doubler(rest);

(* 2.2.8 *)
fun digify(0) = [0]
  | digify(1) = [1]
  | digify(2) = [2]
  | digify(3) = [3]
  | digify(4) = [4]
  | digify(5) = [5]
  | digify(6) = [6]
  | digify(7) = [7]
  | digify(8) = [8]
  | digify(9) = [9]
  | digify(x) = (x mod 10)::digify(x div 10);

(* 2.2.9 *)
fun cat([], b) = b
  | cat(a::rest, b) = a::cat(rest, b);

(* 2.4.14 *)
fun addToEach(a, []) = []
  | addToEach(a, list::rest) = (a::list)::addToEach(a, rest);

(* 2.4.15 *)
fun powerset([]) = [[]]
  | powerset(x::rest) = addToEach(x, powerset(rest))@powerset(rest);

(* Data Modeling Exercise *)
datatype topping = Pepperoni | Ham | Sausage | Chicken | Bacon | Salami 
                 | Mushroom | Onion | BellPepper | Garlic | Olives | ArtichokeHearts 
                 | SunDriedTomatoes | Jalapeno | Pineapple | Basil | Spinach 
                 | GreenOlives | KalamataOlives | RoastedRedPepper | Ricotta | Feta;
datatype customPizza = CustomPizza of topping list;

fun price(CustomPizza([])) = 19.0
  | price(CustomPizza([_])) = 19.0
  | price(CustomPizza(_::rest)) = 1.5 + price(CustomPizza(rest));
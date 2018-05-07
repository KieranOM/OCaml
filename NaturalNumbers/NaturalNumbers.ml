type nat = Zero | Suc of nat;;

let nat_int a =
	let rec nat_int_aux b c = match b with
	 | Zero -> c
	 | Suc x -> nat_int_aux x c+1
	in nat_int_aux a 0;;

let int_nat a =
	let rec int_nat_aux a b = match a with
	 | 0 -> b
	 | x -> int_nat_aux (a-1) (Suc b)
	in int_nat_aux a Zero;;

(* a+b *)
let rec add_nat a b = match a with
 | Zero -> b
 | Suc x -> add_nat x (Suc b);;

(* a-b *)
let rec sub_nat a b = match (a, b) with
 | (Zero, Suc y) -> failwith "Negative result"
 | (x, Zero) -> x
 | (Suc x, Suc y) -> sub_nat x y;;

(* a*b *)
let mul_nat a b =
	let m = a in
		let rec mul_nat_aux a' b = match b with
		 | Zero -> Zero
		 | Suc Zero -> a'
		 | Suc x -> mul_nat_aux (add_nat a' m) x
	in mul_nat_aux a b;;

(* a/b *)
let div_nat a b =
	if b = Zero then failwith "Divide by zero" else
		let rec div_nat_aux a' c = match a' with
		 | Zero -> c
		 | x -> try div_nat_aux (sub_nat x b) (Suc c) with _ -> c
		in div_nat_aux a Zero;;

(* a^b *)
let pow_nat a b =
	let m = a in
		let rec pow_nat_aux a' b = match b with
		 | Zero -> Suc Zero
		 | Suc Zero -> a'
		 | Suc x -> pow_nat_aux (mul_nat a' m) x
	in pow_nat_aux a b;;
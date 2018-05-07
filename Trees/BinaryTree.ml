type bin =
 | Empty
 | Node of bin * int * bin;;
let leaf x = Node(Empty, x, Empty);;

let rec bin_search t x = match t with
 | Empty -> false
 | Node(l, v, r) -> if v = x then true else if x < v then bin_search l x else bin_search r x;;

let rec bin_list t =
	let rec bin_list_aux t a = match t with
	 | Empty -> []
	 | Node(l, v, r) -> (bin_list_aux l a) @ [v] @ (bin_list_aux r a)
	in bin_list_aux t [];;
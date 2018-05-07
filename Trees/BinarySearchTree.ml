type bst =
 | Empty
 | Node of bst * int * bst;;
let leaf x = Node(Empty, x, Empty);;

let rec bst_search t x = match t with
 | Empty -> false
 | Node(l, v, r) -> if v = x then true else if x < v then bst_search l x else bst_search r x;;

let rec bst_list t = match t with
 | Empty -> []
 | Node(l, v, r) -> (bst_list l) @ [v] @ (bst_list r);;

let rec bst_valid t = match t with
 | Empty -> true
 | Node(l, v, r) -> match (l, r) with
 	 | (Empty, Empty) -> true
 	 | (Empty, Node(l', v', r')) -> v' > v
 	 | (Node(l', v', r'), Empty) -> v' < v
 	 | (Node(ll, lv, lr), Node(rl, rv, rr)) -> lv < v && rv > v && (bst_valid l) && (bst_valid r);;
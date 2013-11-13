module MergeSort : sig
	type 'a t = 'a list
	val sort : 'a t -> 'a t
end
=
struct
	type 'a t = 'a list

	let rec split xs left right =
		match xs with
		| [] -> (left,right)
		| x::xs -> split xs (right) (x::left) 

	let rec merge left right =
		match (left,right) with
		| ([],_) -> right
		| (_,[]) -> left
		| (x::xs,y::ys) -> if x < y 
						   then x::(merge xs right)
						   else y::(merge left ys)

	let rec sort l =
		match l with
		| ([]|_::[]) -> l
		| _ -> let (left,right) = split l [] [] in
				 merge (sort left) (sort right)


end

(* Sample Usage *)
let originalList = [21;42;661;-235;436;372;25;6;2;9;23]
let sortedList = MergeSort.sort originalList
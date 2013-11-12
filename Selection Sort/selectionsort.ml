module SelectionSort : sig
	type 'a t = 'a list
	val sort : 'a t -> 'a t
end
=
struct
	type 'a t = 'a list

	let rec min = function
		| [x] -> x
		| x::xs -> let left = min xs in
					if x < left then x else left
		| _ -> failwith "To ignore exhaustive checking"

	(* In order to obtain the new array without the smallest value,
	    I create an ignore function to filter it out and so it makes
	    the complexity worse than O(N^2), any help?
	*)
	let rec ignore s = function
		| [] -> []
		| x::xs -> if x = s then xs
				   		  else x::(ignore s xs)

	let rec sort = function
		| [] -> []
		| xs -> let smallest = min xs in
				  let xss = ignore smallest xs in
				  	 smallest::(sort xss)

end

(* Sample Usage *)
let originalList = [234;-124;43;64;568;1231;-23;0;3;2412]

let sortedList = SelectionSort.sort originalList
module Bubblesort : sig
	type 'a t = 'a list
	val sort : 'a t -> 'a t
end
=
struct
	type 'a t = 'a list

	let rec sortOnce l swaps = 
		match l with
		| [] -> []
		| x::[] -> x::[]
		| x1::x2::[] -> if (x1 > x2) 
						then begin incr swaps; [x2;x1] end
						else [x1;x2]
		| x1::x2::xs -> if (x1 > x2) 
						then begin incr swaps; x2::(sortOnce (x1::xs) swaps) end
						else x1::(sortOnce (x2::xs) swaps) 

	let sort l =
		let sortList = ref l in
			let swaps = ref 1 in 
				while (!swaps <> 0) do
					swaps := 0;
					sortList := (sortOnce !sortList swaps); 
				done;
		!sortList
end

(* Sample Usage *)
let originalArray = [123;24;13;654;234;23]

let resultArray = Bubblesort.sort originalArray  (* 5 loops required, total 8 swaps *)

let worstCase = [90;67;56;54;43;34;22;10;8;3;-12]

let worstResult = Bubblesort.sort worstCase (* 11 loops required, total 55 swaps *)





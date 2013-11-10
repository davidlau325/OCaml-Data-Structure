open Core.Std

module type ORDERED =
sig
	type t
	val compare : t -> t -> int  (* x less than y *)
end

module type SET = 
sig
	type elem
	type t
	val empty : t
	val insert : elem -> t -> t
	val member : elem -> t -> bool
end

(* Use unbalanced binary tree as the implementation *)
module Set (Element : ORDERED) : (SET with type elem := Element.t) =
struct
	type elem = Element.t
	type t = 
		| Empty
		| Tree of t * elem * t
	let empty = Empty
	let rec insert x t = 
		match t with
		| Empty -> Tree (Empty,x,Empty)
		| Tree (left,value,right) as s ->
			match Element.compare x value with
			| 0 -> s
			| n when n < 0 -> Tree (insert x left,value,right)
			| n when n > 0 -> Tree (left,value,insert x right)
			| _ -> failwith "ORDERED compare error"
			
	let rec member x t =
		match t with
		| Empty -> false
		| Tree (left,value,right) ->
			match Element.compare x value with
			| 0 -> true
			| n when n < 0 -> member x left
			| n when n > 0 -> member x right
			| _ -> failwith "ORDERED compare error"
end

(* Generate Set with suitable type, in this case, use Core module *)
module IntegerSet = Set(Int)
module StringSet = Set(String)
module FloatSet = Set(Float)

(* Example Usage *)
let iSet = IntegerSet.empty
let iSet2 = IntegerSet.insert 25 iSet
let iSet3 = IntegerSet.insert 10 iSet2
let isMember = IntegerSet.member 10 iSet3

module type ORDERED =
sig
	type t
	val lt : t -> t -> bool  (* x less than y *)
end

module INTEGER =
struct
	type t = int
	let lt x y = x < y
end

module STRING =
struct
	type t = string
	let lt x y = x < y
end

module FLOAT =
struct
	type t = float
	let lt x y = x < y
end

module type SET = 
sig
	type elem
	type t
	val empty : t
	val insert : elem -> t -> t
	val member : elem -> t -> bool
end

module UnbalancedTree (Element : ORDERED) : (SET with type elem = Element.t) =
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
			if Element.lt x value then Tree (insert x left,value,right)
			else if Element.lt value x then Tree (left,value,insert x right)
			else s
	let rec member x t =
		match t with
		| Empty -> false
		| Tree (left,value,right) ->
			if Element.lt x value then member x left
			else if Element.lt value x then member x right
			else true 
end

module IntegerTree = UnbalancedTree(INTEGER)
module StringTree = UnbalancedTree(STRING)
module FloatTree = UnbalancedTree(FLOAT)

(* Example Usage *)
let iTree = IntegerTree.empty
let iTree2 = IntegerTree.insert 25 iTree
let iTree3 = IntegerTree.insert 10 iTree2
let isMember = IntegerTree.member 10 iTree3

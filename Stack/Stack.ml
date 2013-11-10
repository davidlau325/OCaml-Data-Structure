module type STACK = 
sig
	type 'a stack
	exception EmptyStack
	val empty : 'a stack
	val isEmpty : 'a stack -> bool
	val push : ('a * 'a stack) -> 'a stack
	val pop : 'a stack -> 'a stack
	val top : 'a stack -> 'a
	val map : ('a -> 'b) -> 'a stack -> 'b stack
	val app : ('a -> unit) -> 'a stack -> unit
end

(* implemented with native list *)
module Stack : STACK =
struct
	type 'a stack = 'a list
	exception EmptyStack

	let empty : 'a stack = []
	let isEmpty l = l = []
	let push x l = x :: l
	let pop l =
		match l with
		| [] -> raise EmptyStack
		| x::xs -> xs
	let top l =
		match l with
		| [] -> raise EmptyStack
		| x::xs -> x
	let map f l = List.map f l
	let app f l = List.iter f l
end
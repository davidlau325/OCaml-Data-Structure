exception EmptyStack

module type STACK = 
sig
	type 'a t
	val empty : 'a t
	val isEmpty : 'a t -> bool
	val push : 'a -> 'a t -> 'a t
	val pop : 'a t -> ('a * 'a t)
	val top : 'a t -> 'a
end

(* implemented with native list *)
module Stack : STACK =
struct
	type 'a t = 'a list
	let empty = []
	let isEmpty = function
		| [] -> true
		| _ -> false
	let push x stack = x::stack
	let pop = function
		| [] -> raise EmptyStack
		| x::stack -> x,stack
	let top = function
		| [] -> raise EmptyStack
		| x::stack -> x
end
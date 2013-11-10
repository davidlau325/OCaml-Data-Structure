module type DICTIONARY =
sig
  type key = string
  type 'a dict
  exception NotFound

  val make : unit -> 'a dict
  val insert : 'a dict -> key -> 'a -> 'a dict
  val lookup : 'a dict -> key -> 'a
  val map : ('a -> 'b) -> 'a dict -> 'b dict
end

(* implemented with association list *)
module AssocList : DICTIONARY =
struct
  type key = string
  type 'a dict = (key * 'a) list
  exception NotFound
  
  let make () : 'a dict = []
  let insert (d : 'a dict) (k : key) (x : 'a) : 'a dict = (k, x) :: d
  let rec lookup (d : 'a dict) (k : key) : 'a =
    match d with
      | [] -> raise NotFound
      | (k', x) :: rest ->
              if k = k' then x
              else lookup rest k
  let map (f : 'a -> 'b) (d : 'a dict) =
        List.map (fun (k, a) -> (k, f a)) d
end

(* implemented with higher-order function *)
module FunctionDict : DICTIONARY =
struct
    type key = string
    type 'a dict = string -> 'a
    exception NotFound
   
    let make () = fun _ -> raise NotFound
    let lookup (d : 'a dict) (key : string) : 'a = d key
    let insert (d : 'a dict) (k : key) (x : 'a) : 'a dict = 
                fun k' -> if k = k' then x else d k'
    let map (f : 'a -> 'b) (d : 'a dict) = fun k -> f (d k)
end 

(* a better association list verion *)
module SortedAssocList : DICTIONARY =
struct
  type key = string
  type 'a dict = (key * 'a) list
  exception NotFound
        
  let make() : 'a dict = []
  let rec insert (d : 'a dict) (k : key) (x : 'a) : 'a dict =
          match d with
          | [] -> (k, x) :: []
          | (k', x') :: rest ->
              match String.compare k k' with
                1 -> (k', x') :: (insert rest k x)
              | 0 -> (k, x) :: rest
              | -1 -> (k, x) :: (k', x') :: rest
              | _ -> failwith "Impossible"

  let rec lookup (d : 'a dict) (k : key) : 'a =
          match d with
            [] -> raise NotFound
          | (k', x) :: rest ->
              match String.compare k k' with
                0 -> x
              | -1 -> raise NotFound
              | 1 -> lookup rest k
              | _ -> failwith "Impossible"

  let map (f : 'a -> 'b) (d : 'a dict) =
           List.map (fun (k,a) -> (k, f a)) d
end

(* implemented with binary tree, logn loopup *)
module AssocTree : DICTIONARY =
struct
  type key = string
  type 'a dict = Empty | Node of key * 'a * 'a dict * 'a dict
  exception NotFound


  let make() : 'a dict = Empty

  let rec insert (d : 'a dict) (k : key) (x : 'a) : 'a dict =
          match d with
            Empty -> Node (k, x, Empty, Empty)
          | Node (k', x', l, r) ->
              match String.compare k k' with
                0 -> Node(k, x, l, r)
              | -1 -> Node(k', x', insert l k x, r)
              | 1 -> Node(k', x', l, insert r k x)
              | _ -> failwith "Impossible"

  let rec lookup (d : 'a dict) (k : key) : 'a =
          match d with
            Empty -> raise NotFound
          | Node(k', x, l, r) ->
              match String.compare k k' with
                0 -> x
              | -1 -> lookup l k
              | 1 -> lookup r k
              | _ -> failwith "Impossible"

  let rec map (f : 'a -> 'b) (d : 'a dict) =
          match d with
            Empty -> Empty
          | Node (k, x, l, r) -> Node (k, f x, map f l, map f r)
end
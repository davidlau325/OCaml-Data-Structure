module type QUEUE =
sig
    type 'a queue
    exception EmptyQueue

    val empty : 'a queue
    val isEmpty : 'a queue -> bool

    val enqueue : ('a * 'a queue) -> 'a queue
    val dequeue : 'a queue -> 'a queue
    val front : 'a queue -> 'a

    val map : ('a -> 'b) -> 'a queue -> 'b queue
    val app : ('a -> unit) -> 'a queue -> unit      
end

module Queue : QUEUE = 
    struct

      module S = Stack (* use the Stack module as the helper *)

      type 'a queue = ('a S.stack * 'a S.stack)
      exception EmptyQueue

      let empty : 'a queue = (S.empty, S.empty)
      let isEmpty ((s1, s2) : 'a queue) = 
        S.isEmpty s1 && S.isEmpty s2 

      let enqueue ((x : 'a), ((s1, s2) : 'a queue)) : 'a queue = 
        (S.push (x, s1), s2)

      let rev (s : 'a S.stack) : 'a S.stack = 
        let rec loop ((prev : 'a S.stack), (curr : 'a S.stack)) : 'a S.stack = 
          if S.isEmpty prev
          then curr
          else loop (S.pop prev, S.push (S.top prev, curr))
      in
        loop (s, S.empty)

      let dequeue ((s1, s2) : 'a queue) : 'a queue = 
        if S.isEmpty s2
        then try (S.empty, S.pop (rev s1)) 
             with S.EmptyStack -> raise EmptyQueue
        else (s1, S.pop s2)

      let front ((s1, s2) : 'a queue) : 'a = 
        if (S.isEmpty s2)
        then try S.top (rev s1)
             with S.EmptyStack -> raise EmptyQueue
        else S.top s2

      let map (f : 'a -> 'b) ((s1, s2) : 'a queue) : 'b queue = 
        (S.map f s1, S.map f s2)

      let app (f : 'a -> unit) ((s1, s2) : 'a queue) : unit = 
        S.app f s2;
        S.app f (rev s1)
end
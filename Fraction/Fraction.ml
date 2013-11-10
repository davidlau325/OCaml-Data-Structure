module type FRACTION =
sig
    type fraction

    val make : int -> int -> fraction
    val numerator : fraction -> int
    val denominator : fraction -> int
    val toString : fraction -> string
    val toReal : fraction -> float
    val add : fraction -> fraction -> fraction
    val mul : fraction -> fraction -> fraction
end

module Fraction : FRACTION =
struct
    type fraction = { num:int; denom:int }
    exception BadDenominator

    let rec gcd (x : int) (y : int) : int =
                if x = 0 then y
                else if x < y then gcd (y - x) x
                else gcd y (x - y)

    let make (n : int) (d : int) : fraction =
            if d = 0 then raise BadDenominator
            else let g = gcd (abs n) (abs d) in
                 let n2 = n / g in
                 let d2 = d / g
            in
                 if (d2 < 0) then {num = -n2; denom = -d2}
                 else {num = n2; denom = d2}

    let numerator (x : fraction) : int = x.num

    let denominator (x : fraction) : int = x.denom

    let toString (x : fraction) : string =
            (string_of_int (numerator x)) ^ "/" ^
            (string_of_int (denominator x))

    let toReal (x : fraction) : float =
            (float_of_int (numerator x)) /. (float_of_int (denominator x))

    let mul (x : fraction) (y : fraction) : fraction =
            make ((numerator x) * (numerator y))
                 ((denominator x) * (denominator y))

    let add (x : fraction) (y : fraction) : fraction =
            make ((numerator x) * (denominator y) +
                  (numerator y) * (denominator x))
                 ((denominator x) * (denominator y))
end
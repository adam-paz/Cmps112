(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)

open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])




    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0'
                in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)



    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))







    let rec cmp' list1 list2 max = match (list1, list2, max) with
    | list1, [], max     -> 1
    | [], list2, max     -> -1
    | car1::cdr1, car2::cdr2 , max  -> 
    (*| car1::[], car2::[], max  -> *)
    if(cdr1 = [] && cdr2 = [])then
        if (car1 > car2) then
            1
        else if (car1 < car2) then
            -1
        else max
    else 
        if (car1> car2) then
            cmp' cdr1 cdr2 1
        else if (car1 < car2) then
            cmp' cdr1 cdr2 (-1)
        else 
            cmp' cdr1 cdr2 max


    let cmp list1 list2 = match (list1, list2) with
    | [], []  -> 0
    | [], [0]   -> 0
    | [0], []   -> 0
    | list1, list2   -> 
        cmp' list1 list2 0

    

    let increment number = number * 10

  

    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)
    let double number = (add' number number 0)

 let rec sub' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> sub' list1 [carry] 0
        | [], list2, carry   -> sub' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 - car2 - carry
            in if sum<0 
            then 
                begin
                    let sum = sum + radix
                    in sum mod radix :: sub' cdr1 cdr2 1
                end
            else 
                sum mod radix :: sub' cdr1 cdr2 0

    let trimzeros list =
        let rec trimzeros' list' = match list' with
        | []       -> []
        | [0]      -> []
        | car::cdr ->
             let cdr' = trimzeros' cdr
             in  match car, cdr' with
                 | 0, [] -> []
                 | car, cdr' -> car::cdr'
    in trimzeros' list



   let rec divrem' (dividend, powerof2, divisor') =
    if cmp divisor' dividend > 0
    then [0], dividend
    else let quotient, remainder =
             divrem' (dividend, double powerof2, double divisor')
         in  if cmp remainder divisor' < 0
             then quotient, remainder
             else add' quotient powerof2 0, trimzeros (sub' remainder
              divisor' 0)

    let divrem (dividend, divisor') = divrem' (dividend, [1], 
    divisor')

    let div (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        let quotient, _ = divrem (value1, value2) in
        (*in quotient*)
        if neg1 = neg2
            then Bigint (Pos, quotient)
            else Bigint (Neg, quotient)

    let rem (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        let _, remainder = divrem (value1, value2)
        in Bigint(neg1,remainder)


let even number = 
       let _, remainder = divrem (number, [2]) in
       let trimainder = trimzeros(remainder) in
       if cmp trimainder [] = 0 
            then true
            else false 


    let rec mul' (multiplier, powerof2, multiplicand) =
        
        if (cmp powerof2 multiplier) > 0
        then multiplier, []
        else let remainder, product =
             mul' (multiplier, double powerof2, double multiplicand)
        in  if (cmp powerof2 remainder) > 0
             then remainder, product
             else (trimzeros (sub' remainder powerof2 0)), (add' 
             product multiplicand 0)

    let mul (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        let _, product = mul' (value1, [1], value2) in
        if neg1 = neg2
            then Bigint (Pos, product)
            else Bigint (Neg, product)




    let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then 
            if (cmp value1 value2) > 0
            then
                Bigint (neg1, sub' value1 value2 0)
            else if (cmp value1 value2) < 0 then
                    if neg1 = Pos
                    then
                        Bigint (Neg, sub' value2 value1 0)
                    else
                        Bigint (Pos, sub' value2 value1 0)
            else zero
        else if (cmp value1 value2) > 0
            then
                Bigint (neg1 ,add' value1 value2 0)
            else
                Bigint (neg2 ,add' value1 value2 0)
         

    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then Bigint (neg1, add' value1 value2 0)
        else sub (Bigint (neg1, value1)) (Bigint (neg1, value2))


    let mul_to_1 (list1, list2, list3) =
         let _, product = mul' (list1, list2, list3) in
         product

    let div_to_1 (list1, list2) =
        let quotient,_ = divrem (list1, list2) in
         quotient

    let rec power' (base, expt, result) = match expt with
    | [0]                   -> result
    | expt when even expt   -> power'  (mul_to_1(base, [1], base), 
        div_to_1(expt, [2]), result)
    | expt                  -> power' (base, sub' expt [1] 0, 
        mul_to_1(base, [1], result))

    let pow (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
    if neg2 = Neg then zero
        else if even value2 then
            Bigint(Pos, power' (value1, value2, [1]))
        else
            Bigint(neg1, power' (value1, value2, [1]))
   


end


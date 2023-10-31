type entier = Zero | Successeur of entier
type rationel = {p : entier; q : entier}
exception Overflow of string

(* question 1 *)
let quatre = Successeur(Successeur(Successeur(Successeur(Zero))))

(* question 2 *)
let rec int_vers_entier n = match n with
        |0 -> Zero
        |n -> Successeur (int_vers_entier (n - 1))

let rec entier_vers_int a = match a with
    |Zero -> 0
    |Successeur a -> 1 + entier_vers_int a

(* question 3 *)
let rec add (a : entier) (b : entier) = match a with
    |Successeur a -> Successeur (add a b)
    |Zero -> match b with
        |Zero -> Zero
        |Successeur b -> Successeur (add b Zero)

(* question 4 *)
let rec mult (a : entier) (b : entier) = match b with
    |Zero -> Zero
    |Successeur b -> add a (mult a b)

let rec expo (a : entier) (b : entier) = match b with
    |Zero -> int_vers_entier 1
    |Successeur b -> mult a (expo a b)


(* let rationel_jsp a = *)
(*     entier_vers_int a.p , entier_vers_int a.q *)

(* question 6 *)
let ( * ) (a : rationel) (b : rationel) = {p = mult a.p b.p; q = mult a.q b.q }
let ( + ) (a : rationel) (b : rationel) = {p = add (mult a.p b.q) (mult b.p a.q); q = mult a.q b.q}

(*le but est de calculer le pgcd du nominateur avec le dénominateur. *)

let rec ( >= ) (a : entier) (b : entier) = match a, b with
    |Zero, Zero -> true
    |Zero, _ -> false
    |_, Zero -> true
    |Successeur a, Successeur b -> a >= b

let rec ( - ) (a : entier) (b : entier) = match a, b with
    |_, Zero -> a
    |Successeur a, Successeur b -> a - b
    |_, _ -> raise (Overflow "resultat négatif!")

let rec ( mod ) (a : entier) (b : entier) = match a >= b with
    |false -> a
    |true -> (a - b) mod b

let rec ( / ) (a : entier) (b : entier) = match a >= b with
    |false -> Zero
    |true -> Successeur((a - b) / b)

let rec pgcd (a : entier) (b : entier) = match a mod b with
        |Zero -> b
        |r -> pgcd b r

let irrecductible (frac : rationel) =
    let _pgcd = pgcd frac.p frac.q in
    {p = frac.p / _pgcd; q = frac.q / _pgcd}

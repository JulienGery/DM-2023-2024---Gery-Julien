(*??? question1*)
let question1 u v w x y z =
    float_of_int (u + v + w) +. x +. y +. z /. 6.

(* question 2 *)
let racines a b c =
    let delta = b *. b -. 4. *. a *. c in
    match delta > 0. with
        |true -> 2
        |false when delta = 0. -> 1
        |false -> 0
(* question 3 *)
let rec nombres_pas calories =
    match calories <= 0. with
        |true -> 0
        |false -> 1000 + nombres_pas(calories -. 36.)

(* question 4 *)
let difference_finies suite =
    let g n = suite (n + 1) - suite n in
    g

(* question 5 *)
let rec peter n =
    let rand = Random.int 7 in
    match rand + n >= 100 with
        |true -> "peter"
        |false -> tony (rand + n)
and tony n =
    let des = Random.int 7 + Random.int 7 in
    let anneaux = match des mod 2 with
        |0 -> n + des
        |_ -> max 0 (n - des) in
    match anneaux >= 100 with
        |true -> "tony"
        |false -> steve anneaux
and steve n =
    match n >= 100 with
        |true -> "steve"
        |false -> let pile = Random.int 2 = 1 in
                    match pile with
                        |true -> steve (n + 2)
                        |false -> peter n

(* question 6 *)
(* determine si n est premier avec les nombres dans la liste. *)
let rec est_premier l n = match l with
    |[] -> true
    |h :: t -> match n mod h with
        |0 -> false
        |_ -> est_premier t n

(* renvoie une liste décroissante des premiers jusqu'a n inclue *)
let liste_premier n =
    let rec _liste_premier l t = match t > n with
        |true -> l
        |false -> match est_premier l t with
            |true -> _liste_premier (t :: l) (t + 1)
            |false -> _liste_premier l (t + 1) in
    _liste_premier [] 2

(* limite est la limite de jusque où on veux tester.
   hamming 100 10^9 sont les nombres de hamming inferieur ou égaux a 10^9 *)
let hamming n limite =
    let premiers = liste_premier n in

    (* fonction oxilaire qui compte le nombre de nombre de hamming. branch désigne la brache actuelle. _n est le nombre actuelle de hamming *)
    let rec _hamming _n branch = match _n <= limite with
        |false -> 0
        |true -> let rec ox l = match l with (*fonction auxilaire qui parcour l'arbre des possibilité en fonction de la branche *)
            |[] -> 0
            |h :: t -> match h >= branch with
                |true -> _hamming (_n * h) h + ox t
                |false -> 0 in
        1 + ox premiers in

    _hamming 1 2

(*Puisque tout nombre de hamming (sauf 1) est un multiple d'un premier inférieur ou égale à n par un autre nombre de hamming,
 on peux voir cela comme étant un arbre de choix où avec un nombre de hamming (_n) on peux le multipler par un premier inférieur ou égale a n.
il suffit de limiter les premiers par les quels on peux le multipler en fonction de la branche.
Par exemple: 5 * 7 est un nombre de hamming (7+) mais il peut aussi etre obtenus par 7 * 5, cette optinion est ignoré car 7 > 5.*)

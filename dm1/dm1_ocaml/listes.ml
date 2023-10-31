exception Trop_cour of string

(* question 1 *)
let rec avant_dernier l = match l with
    |[] -> raise (Trop_cour "liste trop courte")
    |[x] -> raise (Trop_cour "liste trop courte")
    |h :: t -> match List.length t = 1 with
        |true -> h
        |false -> avant_dernier t

(* question 2. On suppose que la liste est indexé a partir de 1. Par exemple, n_eme [1; 2] 1 = 1 *)
let rec n_eme l n = match l with
    |[] -> None
    |h :: t -> match n - 1 = 0 with
        |true -> Some h
        |false -> n_eme t (n - 1)

let croissante s =
    let rec _croissante l n = match l with
        |[] -> true
        |h :: t -> match h >= n with
            |true -> _croissante t h
            |false -> false in
    match s with
    |[] -> true
    |h :: t -> _croissante t h

let decroissante s =
    let rec _decroissante l n = match l with
        |[] -> true
        |h :: t -> match h <= n with
            |true -> _decroissante t h
            |false -> false in
    match s with
    |[] -> true
    |h :: t -> _decroissante t h

(* question 3 *)
let monotone l =
    croissante l || decroissante l

(* question 4 *)
let somme_cumulee l =
    let rec _somme_cumulee _l s = match _l with
        |[] -> raise (Trop_cour "c'est pas normal") (* cette ligne n'est jamais executé mais sinon le patern n'est pas exaustif... *)
        |[x] -> [x + s]
        |h :: t -> h :: _somme_cumulee t (h + s) in
    match l with
    |[] -> raise (Trop_cour "liste trop courte") (* on pourait renvoyer [] techniquement... *)
    |[x] -> [x]
    |h :: t -> h :: _somme_cumulee t h

(* question 5 *)
let question5 p l =
    let rec _question5 _l l1 l2 = match _l with
        |[] -> (l1, l2)
        |h :: t -> match p h with
            |true -> _question5 t (h :: l1) l2
            |false -> _question5 t l1 (h :: l2) in

    _question5 l [] []
(* question 6 *)
let produit l1 l2 =
    (*calcule le produit cartésien entre le singleton e et une liste l*)
    let rec _produit e l = match l with
        |[] -> []
        |h :: t -> (e, h) :: _produit e t in
    (*fonction oxiliaire*)
    let rec ox _l1 _l2 = match _l1 with
        |[] -> []
        |h :: t -> _produit h _l2 @ ox t _l2 in
    ox l1 l2

(*équivalent @ pour la question 7*)
let rec combine l1 l2 = match l1 with
    |h :: t -> h :: combine t l2
    |[] -> match l2 with
        |[] -> []
        |h :: t -> h :: combine t []

(* question 7 *)
let rec question7 l = match l with
    |[] -> []
    |h :: t -> combine h (question7 t)

(* question ouverte *)
let encode l =
    (* _l est la liste, e est l'element juste avant et n est la taille actuelle de la suite du même element *)
    let rec _encode _l e n = match _l with
        |[] -> [(e, n)]
        |h :: t -> match h = e with
            |true -> _encode t e (n + 1)
            |false -> (e, n) :: _encode t h 1 in
    match l with
        |[] -> raise (Trop_cour "liste trop courte") (* on pourait renvoyer [] techniquement... *)
        |h :: t -> _encode t h 1

let rec decode l =
    (* fonction d'aide qui cree une liste de n element e *)
    let rec _decode e n = match n with
        |0 -> []
        |n -> e :: _decode e (n - 1) in

    match l with
        |[] -> []
        |h :: t -> let e, n = h in
            _decode e n @ decode t

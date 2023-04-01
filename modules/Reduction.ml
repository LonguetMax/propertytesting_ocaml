module Reduction :
  sig
    (** Type d'une stratégie de réduction des éléments de type 'a
      * Une stratégie associe à chaque valeur une liste de propositions plus "simples".
      * NB : Les propositions sont ordonnées dans l'ordre croissance de "simplicité"
      *      (i.e. les valeurs les plus "simples" sont en début de liste).
      * IMPORTANT : Les stratégies implémentées respectent les conditions des générateurs correspondants.
      *)
    type 'a t = 'a -> 'a list

    (** La stratégie vide : ne renvoie aucune proposition de réduction *)
    val empty : 'a t

    (* TYPES DE BASE *)

    (** Stratégie de réduction sur les entiers
      * @param n entier
      * @return  liste d'entiers plus "simples" entre `-|n|` et `|n|`
      *)
    val int : int t

    (** Stratégie de réduction sur les entiers positifs
      * @param n entier positif
      * @return  liste d'entiers naturels plus "simples" entre 0 et `n`
      *)
    val int_nonneg : int t

    (** Stratégie de réduction sur les flottants
      * @param x flottant
      * @return  liste de flottants plus "simples" entre `-|x|` et `|x|`
      *)
    val float : float t

    (** Stratégie de réduction sur les flottants positifs
      * @param x flottant positif
      * @return  liste de flottants positifs plus "simples" entre `0` et `x`
      *)
    val float_nonneg : float t

    (** Stratégie de réduction sur les caractères
      * @param c caractère
      * @return  liste de caractères plus "simples"
      *)
    val char : char t

    (** Stratégie de réduction sur les caractères alphanumériques
      * @param c caractère alphanumérique
      * @return  liste de caractères alphanumériques plus "simples"
      *)
    val alphanum : char t

    (* CHAINES DE CARACTERES *)

    (** Stratégie de réduction sur les chaînes de caractères
      * @param red stratégie de réduction à utiliser sur chaque caractère
      * @param s   chaîne de caractères
      * @return    liste de chaînes de caractères plus "simples" au pire aussi longues que `s`
      *)
    val string : char t -> string t (*val string : char t -> string t*)

    (* LISTES *)

    (** Stratégie de réduction sur les listes
      * @param red stratégie de réduction à utiliser sur chaque élément
      * @param l   liste
      * @return    liste de listes plus "simples" au pire aussi longues que `l`
      *)
    val list : 'a t -> ('a list) t

    (* TRANSFORMATIONS *)

    (** Stratégie de réduction sur les couples
      * @param fst_red stratégie de réduction de la première coordonnée
      * @param snd_red stratégie de réduction de la deuxième coordonnée
      * @return        stratégie de réduction sur les couples correspondants
      *)
    val combine : 'a t -> 'b t -> ('a * 'b) t

    (** Applique un filtre à une stratégie de réduction
      * @param p   filtre à appliquer à chaque réduction
      * @param red stratégie de réduction
      * @return    stratégie de réduction ne contenant que des propositions vérifiant `p`
      *)
    val filter : ('a -> bool) -> 'a t -> 'a t
  end = struct
    type 'a t = 'a -> 'a list ;;

    (* Implémentation de tous les éléments de la signature manquants *)
    let empty = fun _ -> []
    
    (* TYPES DE BASE *)
    (* int *)
    let int n = List.init (abs n * 2 + 1) (fun i -> i - abs n)    
    let int_nonneg n = 
      List.init (n + 1) (fun i -> i)

    (* float *)
    let float x =
      let open Float in
      let x = abs x in
      let step_size = x /. 4. in
      List.init 9 (fun i -> (float_of_int (i - 4)) *. step_size)

    let float_nonneg x =
      let open Float in
      let step_size = x /. 8. in
      List.init 9 (fun i -> (float_of_int i) *. step_size)

    (* char *)
    let char c =
      let code = int_of_char c in
      let min_code = 0 in
      let max_code = min 255 code in
      List.map char_of_int (List.init (max_code - min_code + 1) (fun i -> i + min_code))
    (*
    let alphanum c =
      List.filter (() || () || ()) char c
*)
    let alphanum c =
      if c >= '0' && c <= '9' then
        let n = int_of_char c - int_of_char '0' in
        if n = 0 then ['0'] else List.init n (fun i -> char_of_int (int_of_char '0' + i))
      else if c >= 'A' && c <= 'Z' then
        let n = int_of_char c - int_of_char 'A' + 1 in
        List.init n (fun i -> char_of_int (int_of_char 'A' + i - 1))
      else if c >= 'a' && c <= 'z' then
        let n = int_of_char c - int_of_char 'a' + 1 in
        List.init n (fun i -> char_of_int (int_of_char 'a' + i - 1))
      else
        [c]

    (* CHAINES DE CARACTERES *)

    let red c =
      match c with
      | 'a' | 'e' | 'i' | 'o' | 'u' | 'y' -> ['*']
      | _ -> [c]

      let rec string red s =
        match s with
        | "" -> [""]
        | c::cs -> List.concat (List.map red c) @ List.map (fun x -> String.make 1 x) cs |> List.map (fun x -> x :: string red cs) |> List.concat
    

    (* LISTES *)
    let list red l =
      let rec aux acc = function
        | [] -> [List.rev acc]
        | x::xs -> List.concat (List.map (fun y -> aux (y::acc) xs) (red x))
      in aux [] l

    (* TRANSFORMATIONS *)
    let combine fst_red snd_red =
      fun (x, y) ->
        List.concat
          (List.map (fun x' -> List.map (fun y' -> (x', y')) (snd_red y)) (fst_red x))

    let filter p red = fun x -> List.filter p (red x)
  end ;;
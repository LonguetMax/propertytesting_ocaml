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

    val alpha : char t

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

    let alphanum c =
      let is_alphanum c =
        let code = int_of_char c in
        (code >= 48 && code <= 57) || (code >= 65 && code <= 90) || (code >= 97 && code <= 122)
      in
      let code = int_of_char c in
      let min_code = 0 in
      let max_code = min 255 code in
      List.filter is_alphanum (List.map char_of_int (List.init (max_code - min_code + 1) (fun i -> i + min_code)))

    let alpha c =
      let code = int_of_char (Char.lowercase_ascii c) in
      if (code < 97 || code > 122) then
          []
      else
        let min_code = 97 in
        let max_code = min 122 code in
        List.map char_of_int (List.init (max_code - min_code + 1) (fun i -> i + min_code))

    (* CHAINES DE CARACTERES *)
    (* Version simple :*)
    (*
    let rec all_combinations acc = function
    | [] -> acc
    | hd::tl ->
        let new_acc = List.concat_map (fun x -> List.map (fun y -> x::y) acc) hd in
        all_combinations new_acc tl

    let string red s =
      let rec string_helper acc = function
        | [] -> all_combinations [[]] (List.rev acc)
        | c :: tl -> string_helper ((red c) :: acc) tl
      in
      List.map (fun l -> List.fold_left (fun acc c -> acc ^ (String.make 1 c)) "" (List.rev l)) (string_helper [] (String.to_seq s |> List.of_seq))
    *)
    let rec string_all_combinations acc = function
    | [] -> acc
    | hd::tl ->
        let new_acc = List.concat_map (fun x -> List.map (fun y -> x::y) acc) hd in
        let sub_combinations, longer_strings = List.partition (fun l -> List.length l <= 2) new_acc in
        string_all_combinations (sub_combinations @ longer_strings @ acc) tl
  
    let string red s =
      let rec string_helper acc = function
        | [] -> string_all_combinations [[]] (List.rev acc)
        | c :: tl -> string_helper ((red c) :: acc) tl
      in
      let result = List.map (fun l -> List.fold_left (fun acc c -> acc ^ (String.make 1 c)) "" (List.rev l)) (string_helper [] (String.to_seq s |> List.of_seq)) in
      List.sort_uniq compare result |> List.sort (fun s1 s2 -> compare (String.length s1) (String.length s2))

    (* LISTES *)
    let rec list_all_combinations acc = function
    | [] -> acc
    | hd::tl ->
        let new_acc = List.concat_map (fun x -> List.map (fun y -> x::y) acc) hd in
        let sub_combinations, longer_lists = List.partition (fun l -> List.length l <= 2) new_acc in
        list_all_combinations (sub_combinations @ longer_lists @ acc) tl
    
    let list red l =
      let rec list_helper acc = function
        | [] -> list_all_combinations [[]] (List.rev acc)
        | hd :: tl -> 
            let reduced_hd = red hd in
            list_helper (reduced_hd :: acc) tl
      in
      let result = List.map List.rev (list_helper [] l) in
      List.sort_uniq compare result |> List.sort (fun l1 l2 -> compare (List.length l1) (List.length l2))
  

    (* TRANSFORMATIONS *)
    let combine fst_red snd_red =
      fun (x, y) ->
        List.concat
          (List.map (fun x' -> List.map (fun y' -> (x', y')) (snd_red y)) (fst_red x))

    let filter p red = fun x -> List.filter p (red x)


      

  


  end ;;
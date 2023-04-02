#use  "Property.ml" ;;
#use "Generator.ml" ;;
#use "Reduction.ml" ;;

module Test :
  sig
    (** Type d'un test portant sur des éléments de type 'a *)
    type 'a t

    (** Construit un test
      * @param gen  générateur pseudo-aléatoire de valeurs de test
      * @param red  stratégie de réduction
      * @param prop propriété qui fait l'objet du test
      * @return     test créé
      *)
    val make_test : 'a Generator.t -> 'a Reduction.t -> 'a Property.t -> 'a t

    (** Effectue un test
      * @param n    nombre de valeurs à tester
      * @param test test à effectuer
      * @return     `true` si n > 0 et que toutes les valeurs à tester satisfont les conditions
      *)
    val check : int -> 'a t -> bool

    (** Cherche une valeur simple ne vérifiant pas la propriété
      * @param n nombre de valeurs à tester
      * @return  `None` si toutes les valeurs de test générées par `gen` vérifient `prop`,
                 une valeur ne vérifiant pas `prop` (éventuellement en appliquant `red`) sinon
      *)
    val fails_at : int -> 'a t -> 'a option

    (** Cherche une valeur simple ne vérifiant pas la propriété
      * @param n nombre de valeurs à tester
      * @return  `None` si toutes les valeurs de test générées par `gen` vérifient `prop`,
                 une valeur ne vérifiant pas `prop` (éventuellement en appliquant `red`) sinon
      *)
    val reduced_fails_at : int -> 'a t -> ('a * 'a list option) list

    (** Exécute plusieurs tests
      * @param n     nombre de valeurs testées par test
      * @param tests liste des tests à vérifier
      * @return      tableau associatif des résultats
      *)
    val execute : int -> ('a t) list -> ('a t * 'a option) list
  end =
  struct
    type 'a t = {
      gen: 'a Generator.t;
      red: 'a Reduction.t;
      prop: 'a Property.t;
    }

    let make_test gen red prop = {
      gen = gen;
      red = red;
      prop = prop;
    }

    let rec check n test =
      match n with
      | m when m > 1 -> test.prop (Generator.next test.gen) && (check (n-1) test)
      | 1 -> test.prop (Generator.next test.gen)
      | _ -> false

    let rec fails_at n test =
        match n with
        | m when m > 0 -> let value = Generator.next test.gen in if test.prop value then fails_at (n-1) test else Some(value)
        | _ -> None

    let reduced_fails_at n test =
      let rec aux i acc =
        if i = 0 then
          acc
        else
          let value = Generator.next test.gen in
          if test.prop value then aux (i-1) (acc @ [(value, None)])
          else aux (i-1) (acc @ [(value, Some((Reduction.filter (fun x -> not (test.prop x)) test.red) value))])
      in aux n [];;

    let execute n tests =
      List.map (fun test -> (test, fails_at n test)) tests

  end ;;

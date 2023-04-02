#use "modules/Reduction.ml"


let run_tests () =
  (* Tests pour la fonction empty *)
  assert (Reduction.empty 3 = []);
  assert (Reduction.empty true = []);
  assert (Reduction.empty "hello" = []);
  print_endline "empty passed !";

  (* Tests pour la fonction int *)
  let ints = Reduction.int 5 in
  assert (List.length ints = 11);
  assert (List.mem 0 ints);
  assert (List.mem (-5) ints);
  assert (List.mem 5 ints);
  print_endline "int passed !";

  (* Tests pour la fonction int_nonneg *)
  let ints_nonneg = Reduction.int_nonneg 5 in
  assert (List.length ints_nonneg = 6);
  assert (List.mem 0 ints_nonneg);
  assert (List.mem 5 ints_nonneg);
  print_endline "int_nonneg passed !";

  (* Tests pour la fonction float *)
  let floats = Reduction.float 3.0 in
  assert (List.length floats = 9);
  assert (List.mem 0.0 floats);
  assert (List.mem (-3.0) floats);
  assert (List.mem 3.0 floats);
  print_endline "float passed !";

  (* Tests pour la fonction float_nonneg *)
  let floats_nonneg = Reduction.float_nonneg 3.0 in
  assert (List.length floats_nonneg = 9);
  assert (List.mem 0.0 floats_nonneg);
  assert (List.mem 3.0 floats_nonneg);
  print_endline "float_nonneg passed !";

  (* Tests pour la fonction char *)
  let chars = Reduction.char '\007' in
  assert (List.length chars = 8);
  assert (List.hd chars = '\000');
  print_endline "char passed !";

  (* Tests pour la fonction alphanum *)
  let alphanums = Reduction.alphanum '[' in
  assert (List.length alphanums = 36);
  assert (List.hd alphanums = '0');
  print_endline "alphanum passed !";

  (* Tests pour la fonction string *)
  let strings = Reduction.string Reduction.alpha "aBc" in
  assert (List.length strings = 16); (* len0(1) + len1(3) + len2(6) + len3(6) *)
  assert (List.mem "" strings);
  assert (List.mem "a" strings);
  assert (List.mem "b" strings);
  assert (List.mem "c" strings);
  assert (List.mem "bc" strings);
  assert (List.mem "abc" strings);
  print_endline "string passed !";

  (* Tests pour la fonction list *)
  let list_red = Reduction.list Reduction.int_nonneg [1;2;3] in
  assert (List.length list_red = 41); (* len0(1 valeur) + len1(4valeurs) + len2(3 valeurs * 4valeurs) len3(2 valeurs * 3 valeurs * 4 valeurs) *)
  assert (List.mem [] list_red);
  assert (List.mem [2] list_red);
  assert (List.mem [2;0] list_red);
  assert (List.mem [0;1;3] list_red);
  print_endline "list passed !";

  (* Tests pour la fonction combine *)
  let combined_fn = Reduction.combine Reduction.int_nonneg Reduction.float in
  let pair_red = combined_fn (5, -1.5) in
  assert (List.length pair_red = 54); (* 6 valeurs a gauhe, 9 a droite *)
  assert (List.mem (0, 0.) pair_red);
  assert (List.mem (1, -1.5) pair_red);
  assert (List.mem (2, 1.125) pair_red);
  assert (List.mem (3, -0.375) pair_red);
  assert (List.mem (4, 0.75) pair_red);
  assert (List.mem (5, 1.5) pair_red);
  print_endline "combine passed !";

  (* Tests pour la fonction filter *)
  let filter_pos_ints = Reduction.filter (fun x -> x < 0) Reduction.int 10 in
  assert (List.length filter_pos_ints = 10); (* -10, ... , -1 = 10 valeurs *)
  assert (List.mem (-10) filter_pos_ints);
  assert (List.mem (-1) filter_pos_ints);
  assert (not (List.mem (0) filter_pos_ints));
  assert (not (List.mem (-0) filter_pos_ints));
  assert (not (List.mem (10) filter_pos_ints));
  print_endline "filter passed !";

  print_endline "All tests went well !";
;;
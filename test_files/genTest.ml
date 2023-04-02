#use "Generator.ml"

let run_test () =
  (* Test bool *)
  let gen = Generator.bool 0.5 in
  for i = 1 to 10 do
    let value = Generator.next gen in
    Printf.printf "bool %d: %B\n" i value
  done;
  Printf.printf "\n";

  (* Test int *)
  let gen = Generator.int (-10) 10 in
  for i = 1 to 10 do
    let value = Generator.next gen in
    Printf.printf "int %d: %d\n" i value
  done;
  Printf.printf "\n";

  (* Test int_nonneg *)
  let gen = Generator.int_nonneg 10 in
  for i = 1 to 10 do
    let value = Generator.next gen in
    Printf.printf "int_nonneg %d: %d\n" i value
  done;
  Printf.printf "\n";

  (* Test float *)
  let gen = Generator.float 0.0 10.0 in
  for i = 1 to 10 do
    let value = Generator.next gen in
    Printf.printf "float %d: %f\n" i value
  done;
  Printf.printf "\n";

  (* Test float_nonneg *)
  let gen = Generator.float_nonneg 10.0 in
  for i = 1 to 10 do
    let value = Generator.next gen in
    Printf.printf "float_nonneg %d: %f\n" i value
  done;
  Printf.printf "\n";
 
  (* Test char *)
  let gen = Generator.char in
  for i = 1 to 10 do
    let value = Generator.next gen in
    Printf.printf "char %d: %c\n" i value
  done;
  Printf.printf "\n";

  (* Test alphanum *)
  let gen = Generator.alphanum in
  for i = 1 to 10 do
    let value = Generator.next gen in
    Printf.printf "alphanum %d: %c\n" i value
  done;
  Printf.printf "\n";

  (* Test string *)
  let gen = Generator.string 10 Generator.alphanum in
  for i = 1 to 10 do
    let value = Generator.next gen in
    Printf.printf "string %d: %s\n" i value
  done;
  Printf.printf "\n";

  (* Test list *)
  let gen = Generator.list 10 (Generator.int 1 10) in
  for i = 1 to 10 do
    let value = Generator.next gen in
    Printf.printf "list %d: " i;
    List.iter (Printf.printf "%d ") value;
    Printf.printf "\n"
  done;
  Printf.printf "\n";

  (* Test combine *)
  let gen = Generator.combine (Generator.int 1 10) Generator.char in
  for i = 1 to 10 do
    let value = Generator.next gen in
    Printf.printf "combine %d: %d, %c\n" i (fst value) (snd value)
  done;
  Printf.printf "\n";

  (* Test map *)
  let gen = Generator.map (fun x -> x + 1) (Generator.int 1 10) in
  for i = 1 to 10 do
    let value = Generator.next gen in
    Printf.printf "map %d: %d\n" i value
  done;
  Printf.printf "\n";

  (* Test filter *)
  let gen = Generator.filter (fun x -> x mod 2 = 0) (Generator.int 1 10) in
  for i = 1 to 10 do
    let value = Generator.next gen in
    Printf.printf "filter %d: %d\n" i value
  done;

  (* Test filter_map
  let gen = Generator.filter_map (fun x -> if x mod 2 = 0 then Some x else None) (Generator.int 1 10) in
  for i = 1 to 10 do
    let value = Generator.next gen in
    Printf.printf "filter_map %d: %d\n" i value
  done; *)




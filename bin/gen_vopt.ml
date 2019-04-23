open! Core

module S = Cmm_vopt.Subcmm

let _main () =
  let example = Cmm_vopt.Schemes.example in
  let () =
    example
    |> S.smt_of_t
    |> List.iter ~f:print_s
  in
  let () =
    example.input
    |> S.Expr.cmm_pat_of_t
    |> print_string
  in
  ()

let main () =
  let open Cmm_vopt in
  let module Cmm = Spec_to_cmm.Cmm in
  let es, ed = Schemes.example_spec in
  let es_cmm =
    Spec_to_cmm.pattern_cmm_of_spec es
  in
  let es = 
    Cmm.Print.print_expression es_cmm
  in
  let ed_cmm =
    Spec_to_cmm.expression_cmm_of_spec ed
  in
  let ed =
    Cmm.Print.print_expression ed_cmm
  in
  let size_input = Cmm.size_of_expression es_cmm in
  let size_output = Cmm.size_of_expression ed_cmm in
  printf "let opt_cmm cmm =\n";
  printf "match cmm with\n";
  printf "| %s ->\n" es;
  printf "  (* Op Count: %i in -> %i out *)\n" size_input size_output;
  printf "  %s\n" ed

let (command : Command.t) =
  let open Command.Let_syntax in
  Command.basic
    ~summary:""
    [%map_open 
      let () = return ()
      in
      fun () ->
        main ()
    ]

let () = Command.run command

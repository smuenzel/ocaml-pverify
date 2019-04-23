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
  let es =
    Spec_to_cmm.pattern_cmm_of_spec es
    |> Cmm.Print.print_expression
  in
  let ed =
    Spec_to_cmm.expression_cmm_of_spec ed
    |> Cmm.Print.print_expression
  in
  printf "| %s ->\n    %s\n" es ed

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

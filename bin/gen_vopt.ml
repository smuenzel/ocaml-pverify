open! Core
open Cmm_vopt

let _main () =
  let open Cmm_vopt in
  let module Cmm = Spec_to_cmm.Cmm in
  let { Spec.Rule. input; output } = Schemes.example_spec in
  let input_cmm =
    Spec_to_cmm.pattern_cmm_of_spec input
  in
  let input = 
    Cmm.Print.print_expression input_cmm
  in
  let output_cmm =
    Spec_to_cmm.expression_cmm_of_spec output
  in
  let output =
    Cmm.Print.print_expression output_cmm
  in
  let size_input = Cmm.size_of_expression input_cmm in
  let size_output = Cmm.size_of_expression output_cmm in
  printf "let opt_cmm cmm =\n";
  printf "match cmm with\n";
  printf "| %s ->\n" input;
  printf "  (* Op Count: %i in -> %i out *)\n" size_input size_output;
  printf "  %s\n" output;
  printf "\n";
  (Spec_to_smt.verify "example" Schemes.example_spec
   |> List.map ~f:Smt.smt_of_phrase
   |> List.iter ~f:(printf !"%{sexp:Sexp.t}\n")
  )

let main ~smt_out ~ml_out =
  let rules = Schemes.all in
  Smt_file.write ~filename:smt_out ~rules;
  Cmm_file.write ~filename:ml_out ~rules ~fname:"reduce_cmm";
;;


let (command : Command.t) =
  let open Command.Let_syntax in
  Command.basic
    ~summary:""
    [%map_open 
      let smt_out = flag "smt" (required string) ~doc:"smt output file"
      and ml_out = flag "ml" (required string) ~doc:"ml output file"
      in
      fun () ->
        main ~smt_out ~ml_out
    ]

let () = Command.run command

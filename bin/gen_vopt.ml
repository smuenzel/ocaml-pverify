open! Core
open Cmm_vopt

let _main () =
  let open Cmm_vopt in
  let module Cmm = Spec_to_cmm.Cmm in
  let { Spec.Rule. input; output; _ } = Schemes.example_spec in
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

let main ~smt_out ~ml_out ~compiler_smt ~compiler_smt_dir ~instances_dir ~divisors
    ~magic_dir =
  let rules = Schemes.all in
  Smt_file.write ~filename:smt_out ~rules:(Map.to_alist rules);
  Cmm_file.write ~filename:ml_out ~rules ~fname:"reduce_cmm";
  Option.iter compiler_smt ~f:(fun filename ->
      Smt_file.write ~filename ~rules:Compiler_rules.all);
  Option.iter compiler_smt_dir ~f:(fun dirname ->
      Core_unix.mkdir_p dirname;
      Smt_file.write_dir ~dirname ~rules:Compiler_rules.all);
  Option.iter instances_dir ~f:(fun dirname ->
      Core_unix.mkdir_p dirname;
      Smt_file.write_dir ~dirname ~rules:(Compiler_rules.instances ~divisors ()));
  Option.iter magic_dir ~f:(fun dirname ->
      Core_unix.mkdir_p dirname;
      let divisors =
        if List.is_empty divisors then Compiler_rules.magic_divisors else divisors
      in
      (* a few controls, which must fail *)
      let controls = List.take divisors 3 in
      Magic_division.write_dir ~dirname ~divisors:controls ~controls:true
        ~first_index:0;
      Magic_division.write_dir ~dirname ~divisors ~controls:false
        ~first_index:(List.length controls))
;;


let (command : Command.t) =
  let open Command.Let_syntax in
  Command.basic
    ~summary:""
    [%map_open 
      let smt_out = flag "smt" (required string) ~doc:"smt output file"
      and ml_out = flag "ml" (required string) ~doc:"ml output file"
      and compiler_smt =
        flag "compiler-smt" (optional string)
          ~doc:"FILE smt output file for the compiler's Cmm rewrites"
      and compiler_smt_dir =
        flag "compiler-smt-dir" (optional string)
          ~doc:"DIR one smt file per compiler rewrite, with counterexamples"
      and instances_dir =
        flag "compiler-instances-dir" (optional string)
          ~doc:"DIR instances of the compiler rewrites that are too hard symbolically"
      and divisors =
        flag "divisor" (listed int)
          ~doc:"N only instantiate the division rules with this divisor (repeatable)"
      and magic_dir =
        flag "magic-division-dir" (optional string)
          ~doc:"DIR integer-arithmetic checks of the multiply-high division"
      in
      fun () ->
        main ~smt_out ~ml_out ~compiler_smt ~compiler_smt_dir ~instances_dir ~divisors
          ~magic_dir
    ]

let () = Command_unix.run command

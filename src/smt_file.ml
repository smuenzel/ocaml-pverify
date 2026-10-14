open! Core

let output_rule out ?get_values ?check_vacuity name rule =
  Spec_to_smt.verify ?get_values ?check_vacuity name rule
  |> List.iter ~f:(fun phrase ->
      Printf.fprintf out !"%{sexp:Sexp.t}\n" (Smt.smt_of_phrase phrase)
    )

let write ~filename ~rules =
  Out_channel.with_file filename
    ~f:(fun out ->
        Out_channel.output_string out Smt_preamble.full_preamble;
        Out_channel.newline out;
        Out_channel.newline out;
        List.iter rules
          ~f:(fun (name, rule) ->
              output_rule out name rule;
              Out_channel.newline out;
              Out_channel.newline out;
            )
      )

(* One self-contained file per rule, which also asks for a counterexample.
   [dirname] must exist. *)
let write_dir ~dirname ~rules =
  List.iteri rules
    ~f:(fun index (name, rule) ->
        let base =
          String.map name ~f:(fun c ->
              if Char.is_alphanum c || Char.equal c '_' || Char.equal c '-' then c else '_')
        in
        let filename = dirname ^/ sprintf "%03i_%s.smt2" index base in
        Out_channel.with_file filename
          ~f:(fun out ->
              Out_channel.output_string out Smt_preamble.full_preamble;
              Out_channel.newline out;
              output_rule out ~get_values:true ~check_vacuity:true name rule))

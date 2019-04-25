open! Core

let write ~filename ~rules =
  Out_channel.with_file filename
    ~f:(fun out ->
        Out_channel.output_string out Smt_preamble.full_preamble;
        Out_channel.newline out;
        Out_channel.newline out;
        Map.iteri rules
          ~f:(fun ~key ~data ->
              Spec_to_smt.verify key data
              |> List.iter ~f:(fun phrase -> 
                  Printf.fprintf out !"%{sexp:Sexp.t}\n" (Smt.smt_of_phrase phrase)
                );
              Out_channel.newline out;
              Out_channel.newline out;
            )
      )

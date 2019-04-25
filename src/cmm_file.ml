open! Core

let write ~filename ~fname ~rules =
  Out_channel.with_file filename
    ~f:(fun out ->
        Out_channel.newline out;
        Printf.fprintf out "let rec %s = function\n" fname;
        Map.iteri rules
          ~f:(fun ~key ~data ->
              Spec_to_cmm.to_match_case ~name:key data
              |> Out_channel.output_string out;
              Out_channel.newline out;
              Printf.fprintf out "  |> %s\n" fname
            );
        Printf.fprintf out "  | other -> other\n"
      )

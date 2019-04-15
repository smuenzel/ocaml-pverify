open! Core

module S = Cmm_vopt.Subcmm

let main () =
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

open! Core

module S = Cmm_vopt.Subcmm

let main () =
  Cmm_vopt.Schemes.example
  |> S.smt_of_t
  |> List.iter ~f:print_s

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

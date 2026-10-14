open! Core

module Smt_int = struct
  let size_in_bits = 64

  let of_int64 i64 =
    let bits =
      List.init size_in_bits
        ~f:(fun index ->
            if Int64.(i64 land (one lsl index) = zero)
            then '0'
            else '1'
          )
      |> List.rev
      |> String.of_char_list
    in
    "#b" ^ bits

  let of_int i = of_int64 (Int64.of_int i)

  let%expect_test "" =
    of_int 0 |> print_endline;
    [%expect {| #b0000000000000000000000000000000000000000000000000000000000000000 |}];
    of_int 1 |> print_endline;
    [%expect {| #b0000000000000000000000000000000000000000000000000000000000000001 |}];
    of_int (-1) |> print_endline;
    [%expect {| #b1111111111111111111111111111111111111111111111111111111111111111 |}];
  ;;

end

(* Helpers for building SMT-LIB terms *)
module Term = struct
  type t = Sexp.t

  let atom s : t = Atom s
  let app f args : t = List (Atom f :: args)
  let int i : t = Atom (Smt_int.of_int i)
  let int64 i : t = Atom (Smt_int.of_int64 i)
  let zero = int 0
  let one = int 1

  let true_ : t = Atom "true"
  let false_ : t = Atom "false"

  let and_ = function
    | [] -> true_
    | [ a ] -> a
    | l -> app "and" l

  let or_ = function
    | [] -> false_
    | [ a ] -> a
    | l -> app "or" l

  let not_ a = app "not" [ a ]
  let eq a b = app "=" [ a; b ]
  let ne a b = not_ (eq a b)
  let implies a b = app "=>" [ a; b ]
  let ite c a b = app "ite" [ c; a; b ]
end

type phrase =
  | Push of int
  | Pop of int
  | Check_sat
  | Get_model
  | Get_value of string list
  | Assert of Term.t
  | Echo of string
  | Declare_const of { name : string; typ : string }
  | Set_option of string * string

let smt_of_phrase : _ -> Sexp.t = function
  | Push i -> List [ Atom "push"; [%sexp_of:int] i ]
  | Pop i -> List [ Atom "pop"; [%sexp_of:int] i ]
  | Check_sat -> List [ Atom "check-sat" ]
  | Get_model -> List [ Atom "get-model" ]
  | Get_value names ->
    List [ Atom "get-value"; List (List.map names ~f:(fun n -> Sexp.Atom n)) ]
  | Assert term ->
    List [ Atom "assert"; term ]
  | Echo string ->
    List [ Atom "echo"; [%sexp_of: string] string ]
  | Declare_const { name; typ } ->
    List [ Atom "declare-const"; Atom name; Atom typ ]
  | Set_option (name, value) ->
    List [ Atom "set-option"; Atom name; Atom value ]

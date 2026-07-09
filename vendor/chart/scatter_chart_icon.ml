open! Core
open Bonsai_term

type t = Uchar.t [@@deriving sexp_of]

let of_uchar uchar =
  match View.uchar_tty_width uchar with
  | 1 -> Some uchar
  | _ -> None
;;

let of_uchar_exn uchar =
  let width = View.uchar_tty_width uchar in
  match width with
  | 1 -> uchar
  | _ -> raise_s [%message "uchar must have width 1" (uchar : Uchar.t) (width : int)]
;;

let dot = of_uchar_exn (Uchar.Utf8.of_string "•")
let to_string = Uchar.Utf8.to_string
let view ?attrs t = to_string t |> View.text ?attrs

open! Core
open Bonsai_term

type t [@@deriving sexp_of]

(** Returns [None] if [View.uchar_tty_width uchar] is not 1. *)
val of_uchar : Uchar.t -> t option

(** Raises if [View.uchar_tty_width uchar] is not 1. *)
val of_uchar_exn : Uchar.t -> t

(** Bullet point icon: • *)
val dot : t

val view : ?attrs:Attr.t list -> t -> View.t
val to_string : t -> string

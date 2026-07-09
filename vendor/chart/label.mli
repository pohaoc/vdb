open! Core
open Bonsai_term

type t =
  { index : int
  (** The bottom most row or left most column that [view] will be placed on. *)
  ; view : View.t
  }

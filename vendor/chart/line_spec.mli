open! Core
open Bonsai_term

type t =
  { coordinates : (x:float * y:float) list
  ; color : Attr.Color.t option
  ; line_type : Line_type.t
  }
[@@deriving fields ~getters]

val create
  :  ?color:Attr.Color.t
  -> ?line_type:Line_type.t
  -> (x:float * y:float) list
  -> t

val filter_to_finite_coordinates : t -> t

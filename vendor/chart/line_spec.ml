open! Core
open Bonsai_term

type t =
  { coordinates : (x:float * y:float) list
  ; color : Attr.Color.t option
  ; line_type : Line_type.t
  }
[@@deriving fields ~getters]

let create ?color ?(line_type = Line_type.Thin) coordinates =
  { coordinates; color; line_type }
;;

let filter_to_finite_coordinates { coordinates; color; line_type } =
  { coordinates =
      List.filter coordinates ~f:(fun (~x, ~y) -> Float.is_finite x && Float.is_finite y)
  ; color
  ; line_type
  }
;;

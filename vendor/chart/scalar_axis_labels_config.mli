open! Core
open Bonsai_term

type t =
  | Hidden
  | Reasonable_default
  (** Places a label on every 5th row for y axis labels or a label centered around every
      10th column for x axis labels. *)
  | Every_n_cells of
      { n : int
      ; make_label_string : Make_label_string.t
      }
  (** Places a label on every nth row or column (depending on if it's a vertical or
      horizontal axis). *)
  | Every_x_units of
      { x : float
      ; make_label_string : Make_label_string.t
      }
  (** Ex: if x is 5, a label would be placed on the row or column associated with values
      0, 5, 10, ... *)
  | Custom of (int * string) list (** (0 indexed row / column number, label string) *)

module Internal : sig
  val resolve_y_labels
    :  t
    -> length:int
    -> min_value:float
    -> max_value:float
    -> axis_scale_config:Axis_scale_config.t
    -> axis_scale:Axis_scale.t
    -> label_color:Attr.Color.t option
    -> View.t option

  val resolve_x_labels
    :  t
    -> length:int
    -> min_value:float
    -> max_value:float
    -> axis_scale_config:Axis_scale_config.t
    -> axis_scale:Axis_scale.t
    -> label_color:Attr.Color.t option
    -> View.t option
end

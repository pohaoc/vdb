open! Core

type t =
  | Default
  (** Defaults to setting the minimum value on the axis to 0 if all of the data is >= 0 or
      the minimum value of the data if the minimum value is < 0. The max value will be the
      maximum of 0 and the greatest value from the data. *)
  | Linear of
      { min_value : Axis_range.t
      ; max_value : Axis_range.t
      }
  | Logarithmic of
      { min_value : Axis_range.t
      ; max_value : Axis_range.t
      ; base : int
      }

module Internal : sig
  (** [min_value] could possibly be greater than [max_value]. They actually represent the
      value at the bottom or left of the chart and the top or right of the chart
      (depending on if it is being used for a horizontal or vertical axis). *)
  val extrema : t -> data:float list -> min_value:float * max_value:float

  val resolve
    :  t
    -> data:float list
    -> length:int (** The height or width of the axis in terminal cells. *)
    -> Axis_scale.t
end

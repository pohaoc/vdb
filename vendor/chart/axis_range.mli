open! Core

(** The min/max values on an axis. [Use_min_value] and [Use_max_value] use the min/max
    value in the data used for your graph. *)
type t =
  | Constant of float
  | Use_min_value
  | Use_max_value

module Internal : sig
  val resolve : t -> float list -> float
end

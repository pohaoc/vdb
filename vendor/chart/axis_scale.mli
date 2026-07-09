open! Core

(** Represents a function from a float value in data space to a float value in plot space.
    For example, a [t] is used to translate a value associated with a (the float in data
    space) to how tall the bar should be (the float in plot space). *)
type t [@@deriving quickcheck, sexp_of]

(** The value in plot space will scale linearly with a change in the data space float. *)
val create_linear
  :  min_value:float
  -> max_value:float
  -> length:int (** The height or width of the given axis (in terminal cells). *)
  -> t

val create_logarithmic
  :  min_value:float
  -> max_value:float
  -> length:int (** The height or width of the given axis (in terminal cells). *)
  -> t

module Internal : sig
  (** Data value to plot value. *)
  val apply : t -> float -> float

  (** Plot value to data value. *)
  val invert : t -> float -> float
end

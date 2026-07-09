open! Core

(** Convert a value to a label string. For example, if you're graphing percentages, you
    might use [fun x -> Float.to_string_hum ~decimals:0 x ^ "%"]. *)
type t = float -> string

(** The returned function uses a metrix prefix (K, M, B, T) determined by the magnitude of
    the greatest magnitude of [min_value] and [max_value].

    There are enough decimals that there will be a different label string for each row. *)
val make_reasonable_linear : max_length:int -> min_value:float -> max_value:float -> t

val make_reasonable_logarithmic : base:int -> t

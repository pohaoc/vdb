open! Core
open Bonsai_term

(** The coordinates in these [Line_spec.t] refer to rows and columns. For example, a line
    that starts at (0.0, 0.0) will start at the bottom left corner. Floats are rounded
    down to the closest integer to determine which cell they refer to, meaning that (0.9,
    0.9) also maps to the bottom left corner.

    A line [p1; p2; p3] is plotted by drawing a line between p1 and p2 and then drawing a
    line between p2 and p3.

    Lines can overlap. If they do, the overlapping character will be the color of the
    earlier line in the list. *)
val view : Line_spec.t list -> height:int -> width:int -> View.t

module For_testing : sig
  (** Change the [start] and [end_] so that they're within the defined boundary. Returns
      [None] if the line never intersects the boundary. *)
  val clip_line
    :  start:(x:float * y:float)
    -> end_:(x:float * y:float)
    -> x_min:float
    -> x_max:float
    -> y_min:float
    -> y_max:float
    -> (start:(x:float * y:float) * end_:(x:float * y:float)) option
end

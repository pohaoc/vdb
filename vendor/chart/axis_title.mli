open! Core
open Bonsai_term

(** Centers the [View.t] within a rectangle of width [goal_width] if the starting width is
    less than [goal_width] and takes the first [goal_width] columns (from left to right)
    from the [View.t] if it is wider than [goal_width]. *)
val resize_x_axis_title : View.t -> goal_width:int -> View.t

(** Centers the [View.t] within a rectangle of height [goal_height] if the starting height
    is less than [goal_height] and takes the first [goal_height] rows (from top down) from
    the [View.t] if it is taller than [goal_height]. *)
val resize_y_axis_title : View.t -> goal_height:int -> View.t

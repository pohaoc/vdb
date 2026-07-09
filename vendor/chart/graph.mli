open! Core
open Bonsai_term

(** Given a graph with a [data_view] that has dimensions X × Y, the following must be true
    for views that are not [None]:
    - [x_labels], [x_axis_title], and [title] have width X
    - [y_labels] and [y_axis_title] have height Y

    If an input does not meet its required dimension, it will be hidden. *)
val view
  :  data_view:View.t
  -> x_labels:View.t option
  -> y_labels:View.t option
  -> x_axis_title:View.t option
  -> y_axis_title:View.t option
  -> title:View.t option
  -> View.t

(** Same as [view], except raises if any of the inputs do not meet their required
    dimension. *)
val view_exn
  :  data_view:View.t
  -> x_labels:View.t option
  -> y_labels:View.t option
  -> x_axis_title:View.t option
  -> y_axis_title:View.t option
  -> title:View.t option
  -> View.t

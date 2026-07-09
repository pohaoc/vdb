open! Core
open Bonsai_term

(** Common logic shared between all charts that have two scalar axes. *)
val view
  :  ?theme:Theme.t
  -> ?x_labels_config:Scalar_axis_labels_config.t
  -> ?y_labels_config:Scalar_axis_labels_config.t
  -> ?x_axis_scale_config:Axis_scale_config.t
  -> ?y_axis_scale_config:Axis_scale_config.t
  -> ?x_axis_title:View.t
  -> ?y_axis_title:View.t
  -> ?title:string
  -> (x:float * y:float) list
  -> data_height_without_border:int
  -> data_width_without_border:int
  -> make_data_view:(x_scale:Axis_scale.t -> y_scale:Axis_scale.t -> View.t)
  -> View.t

val view_exn
  :  ?theme:Theme.t
  -> ?x_labels_config:Scalar_axis_labels_config.t
  -> ?y_labels_config:Scalar_axis_labels_config.t
  -> ?x_axis_scale_config:Axis_scale_config.t
  -> ?y_axis_scale_config:Axis_scale_config.t
  -> ?x_axis_title:View.t
  -> ?y_axis_title:View.t
  -> ?title:string
  -> (x:float * y:float) list
  -> data_height_without_border:int
  -> data_width_without_border:int
  -> make_data_view:(x_scale:Axis_scale.t -> y_scale:Axis_scale.t -> View.t)
  -> View.t

open! Core
open Bonsai_term

val view
  :  ?theme:Theme.t (** Default: [Theme.catppuccin ~flavor:Mocha ~data_color:Blue] *)
  -> ?x_labels_config:Scalar_axis_labels_config.t
       (** See [Scalar_axis_labels_config.Reasonable_default]. *)
  -> ?y_labels_config:Scalar_axis_labels_config.t
       (** See [Scalar_axis_labels_config.Reasonable_default]. *)
  -> ?x_axis_scale_config:Axis_scale_config.t (** See [Axis_scale_config.Default]. *)
  -> ?y_axis_scale_config:Axis_scale_config.t (** See [Axis_scale_config.Default]. *)
  -> ?x_axis_title:View.t
       (** Placed at the bottom of the chart. The [View.t] is cropped if it is wider than
           the data and border. *)
  -> ?y_axis_title:View.t
       (** Placed to the left of the chart. The [View.t] is cropped if it is taller than
           the data and border. *)
  -> ?title:string
  -> Line_spec.t list
  -> data_height:int
       (** [data_height] refers to the height of the part of the chart containing the
           lines (as opposed to the height of the graph as a whole). *)
  -> data_width:int
       (** Similarly to above, [data_width] refers to the width of the part of the chart
           containing the lines (as opposed to the width of the graph as a whole). *)
  -> View.t

module For_testing : sig
  (** The same as [view] except raises when any component is not the correct size instead
      of hiding that component. *)
  val view_exn
    :  ?theme:Theme.t
    -> ?x_labels_config:Scalar_axis_labels_config.t
    -> ?y_labels_config:Scalar_axis_labels_config.t
    -> ?x_axis_scale_config:Axis_scale_config.t
    -> ?y_axis_scale_config:Axis_scale_config.t
    -> ?x_axis_title:View.t
    -> ?y_axis_title:View.t
    -> ?title:string
    -> Line_spec.t list
    -> data_height:int
    -> data_width:int
    -> View.t
end

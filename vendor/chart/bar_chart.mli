open! Core
open Bonsai_term

module Bar_spec : sig
  type t =
    { value : float
    ; label : string option
    ; color : Attr.Color.t option
    }
end

module Bar_width_config : sig
  type t =
    | Custom of
        { width : int
        ; padding : int
        } (** There will be [padding] blank columns between each bar. *)
    | Choose_for_me_from_max_total_width of int
    (** Chooses a width and padding for the bars that will bring the total width of the
        graph as close to the max total width as possible without going over. However, if
        the graph is still wider than the provided total width when the bar width is 1 and
        the padding is 0, the graph will be longer than the given total width. *)
end

val view
  :  ?theme:Theme.t (** Default: [Theme.catppuccin ~flavor:Mocha ~data_color:Blue] *)
  -> ?show_x_labels:bool (** Default [true]. *)
  -> ?y_labels_config:Scalar_axis_labels_config.t
       (** See [Scalar_axis_labels_config.Reasonable_default]. *)
  -> ?x_axis_title:View.t
       (** Placed at the bottom of the chart. The [View.t] is cropped if it is wider than
           the data and border. *)
  -> ?y_axis_title:View.t
       (** Placed to the left of the chart. The [View.t] is cropped if it is taller than
           the data and border. *)
  -> ?title:string
  -> ?bar_height_config:Axis_scale_config.t (** See [Axis_scale_config.Default]. *)
  -> Bar_spec.t list (** Each bar in the list will be graphed from left to right. *)
  -> max_bar_height:int (** The number of vertical cells the y axis will take up. *)
  -> bar_width_config:Bar_width_config.t
  -> View.t

module For_testing : sig
  val calculate_bar_width : width_for_bars:int -> num_bars:int -> width:int * padding:int

  (** The same as [view] except raises when any component is not the correct size instead
      of hiding that component. *)
  val view_exn
    :  ?theme:Theme.t
    -> ?show_x_labels:bool
    -> ?y_labels_config:Scalar_axis_labels_config.t
    -> ?x_axis_title:View.t
    -> ?y_axis_title:View.t
    -> ?title:string
    -> ?bar_height_config:Axis_scale_config.t
    -> Bar_spec.t list
    -> max_bar_height:int
    -> bar_width_config:Bar_width_config.t
    -> View.t
end

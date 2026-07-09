open! Core
open Bonsai_term

let create_view
  ?(theme = Theme.catppuccin ~flavor:Bonsai_term_catppuccin.Mocha.flavor ~data_color:Blue)
  ?(x_labels_config = Scalar_axis_labels_config.Reasonable_default)
  ?(y_labels_config = Scalar_axis_labels_config.Reasonable_default)
  ?(x_axis_scale_config = Axis_scale_config.Default)
  ?(y_axis_scale_config = Axis_scale_config.Default)
  ?x_axis_title
  ?y_axis_title
  ?title
  data
  ~data_height_without_border
  ~data_width_without_border
  ~make_data_view
  ~view_fn
  =
  let data =
    List.filter data ~f:(fun (~x, ~y) -> Float.is_finite x && Float.is_finite y)
  in
  let x_data_values, y_data_values =
    List.map data ~f:(fun (~x, ~y) -> x, y) |> List.unzip
  in
  let x_scale =
    Axis_scale_config.Internal.resolve
      x_axis_scale_config
      ~data:x_data_values
      ~length:(data_width_without_border - 1)
    (* We subtract 1 from the [data_width_without_border] when creating the [x_scale] to
       make sure that the column associated with the maximum value is on the graph. 

       For example, imagine a graph where the max data value was 2 and the max width was
       2. We'd map the values [0, 1) to column 0, [1, 2) to column 1, and 2 would map to
       column 2 which is out of bounds. To solve this, we subtract 1 from
       [data_width_without_border] and squeeze everything to 2 columns. *)
  in
  let y_scale =
    Axis_scale_config.Internal.resolve
      y_axis_scale_config
      ~data:y_data_values
      ~length:(data_height_without_border - 1)
    (* see comment above *)
  in
  let data_view =
    let view = make_data_view ~x_scale ~y_scale in
    Bonsai_term_border_box.view
      view
      ~attrs:(Option.map theme.border ~f:Attr.fg |> Option.to_list)
      ~line_type:Thick
      ~hide_right:true
      ~hide_top:true
  in
  let { Dimensions.height = data_height; width = data_width } =
    View.dimensions data_view
  in
  let y_labels =
    let ~min_value, ~max_value =
      Axis_scale_config.Internal.extrema y_axis_scale_config ~data:y_data_values
    in
    Scalar_axis_labels_config.Internal.resolve_y_labels
      y_labels_config
      ~length:data_height
      ~min_value
      ~max_value
      ~axis_scale_config:y_axis_scale_config
      ~axis_scale:y_scale
      ~label_color:theme.label_text
  in
  let x_labels =
    let ~min_value, ~max_value =
      Axis_scale_config.Internal.extrema x_axis_scale_config ~data:x_data_values
    in
    Scalar_axis_labels_config.Internal.resolve_x_labels
      x_labels_config
      ~length:data_width
      ~min_value
      ~max_value
      ~axis_scale_config:x_axis_scale_config
      ~axis_scale:x_scale
      ~label_color:theme.label_text
  in
  let title =
    let%map.Option title in
    Graph_title.view
      title
      ~width:data_width
      ~text_color:theme.title
      ~border_color:theme.title_border
  in
  let x_axis_title =
    Option.map x_axis_title ~f:(Axis_title.resize_x_axis_title ~goal_width:data_width)
  in
  let y_axis_title =
    Option.map y_axis_title ~f:(Axis_title.resize_y_axis_title ~goal_height:data_height)
  in
  view_fn ~data_view ~x_labels ~y_labels ~x_axis_title ~y_axis_title ~title
;;

let view = create_view ~view_fn:Graph.view
let view_exn = create_view ~view_fn:Graph.view_exn

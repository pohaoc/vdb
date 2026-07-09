open! Core
open Bonsai_term

let create_view
  ?(theme = Theme.catppuccin ~flavor:Bonsai_term_catppuccin.Mocha.flavor ~data_color:Blue)
  ?x_labels_config
  ?y_labels_config
  ?x_axis_scale_config
  ?y_axis_scale_config
  ?x_axis_title
  ?y_axis_title
  ?title
  data
  ~collision_resolution
  ~data_height:data_height_without_border
  ~data_width:data_width_without_border
  ~view_fn
  =
  let data =
    List.filter
      data
      ~f:(fun { Scatter_chart_point.coordinate = ~x, ~y; color = _; icon = _ } ->
        Float.is_finite x && Float.is_finite y)
    |> List.map ~f:(fun { Scatter_chart_point.coordinate; color; icon } ->
      let color =
        match color with
        | Some color -> Some color
        | None -> theme.data
      in
      { Scatter_chart_point.coordinate; color; icon })
  in
  let data_coordinates = List.map data ~f:Scatter_chart_point.coordinate in
  view_fn
    ?theme:(Some theme)
    ?x_labels_config
    ?y_labels_config
    ?x_axis_scale_config
    ?y_axis_scale_config
    ?x_axis_title
    ?y_axis_title
    ?title
    data_coordinates
    ~data_height_without_border
    ~data_width_without_border
    ~make_data_view:(fun ~x_scale ~y_scale ->
      let scaled_data =
        List.map data ~f:(fun { coordinate = ~x, ~y; color; icon } ->
          let x, y =
            ( Float.iround_exn ~dir:`Down (Axis_scale.Internal.apply x_scale x)
            , Float.iround_exn ~dir:`Down (Axis_scale.Internal.apply y_scale y) )
          in
          { Scatter_chart_point.coordinate = { Position.x; y }; color; icon })
      in
      Dots.view
        scaled_data
        ~collision_resolution
        ~height:data_height_without_border
        ~width:data_width_without_border)
;;

let create_view_subpixel
  ?(theme = Theme.catppuccin ~flavor:Bonsai_term_catppuccin.Mocha.flavor ~data_color:Blue)
  ?x_labels_config
  ?y_labels_config
  ?x_axis_scale_config
  ?y_axis_scale_config
  ?x_axis_title
  ?y_axis_title
  ?title
  data
  ~collision_resolution
  ~data_height:data_height_without_border
  ~data_width:data_width_without_border
  ~view_fn
  =
  let data =
    List.filter data ~f:(fun (~x, ~y) -> Float.is_finite x && Float.is_finite y)
  in
  view_fn
    ?theme:(Some theme)
    ?x_labels_config
    ?y_labels_config
    ?x_axis_scale_config
    ?y_axis_scale_config
    ?x_axis_title
    ?y_axis_title
    ?title
    data
    ~data_height_without_border
    ~data_width_without_border
    ~make_data_view:(fun ~x_scale ~y_scale ->
      let scaled_data =
        List.map data ~f:(fun (~x, ~y) ->
          ( ~x:(Axis_scale.Internal.apply x_scale x)
          , ~y:(Axis_scale.Internal.apply y_scale y) ))
      in
      Braille_dots.view
        scaled_data
        ~color:theme.data
        ~collision_resolution
        ~height:data_height_without_border
        ~width:data_width_without_border)
;;

let view = create_view ~view_fn:Chart_common.view
let view_subpixel = create_view_subpixel ~view_fn:Chart_common.view

module For_testing = struct
  let view_exn = create_view ~view_fn:Chart_common.view_exn
  let view_subpixel_exn = create_view_subpixel ~view_fn:Chart_common.view_exn
end

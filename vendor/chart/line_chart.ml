open! Core

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
  ~data_height:data_height_without_border
  ~data_width:data_width_without_border
  ~view_fn
  =
  let data =
    List.map data ~f:Line_spec.filter_to_finite_coordinates
    |> List.map
         ~f:(fun ({ Line_spec.color; coordinates = _; line_type = _ } as line_spec) ->
           let color =
             match color with
             | Some color -> Some color
             | None -> theme.data
           in
           { line_spec with color })
  in
  let data_coordinates = List.map data ~f:Line_spec.coordinates |> List.join in
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
      let cell_coordinate_lines =
        List.map
          data
          ~f:(fun ({ Line_spec.coordinates; color = _; line_type = _ } as line_spec) ->
            let coordinates =
              List.map coordinates ~f:(fun (~x, ~y) ->
                ( ~x:(Axis_scale.Internal.apply x_scale x)
                , ~y:(Axis_scale.Internal.apply y_scale y) ))
            in
            { line_spec with coordinates })
      in
      Line.view
        cell_coordinate_lines
        ~height:data_height_without_border
        ~width:data_width_without_border)
;;

let view = create_view ~view_fn:Chart_common.view

module For_testing = struct
  let view_exn = create_view ~view_fn:Chart_common.view_exn
end

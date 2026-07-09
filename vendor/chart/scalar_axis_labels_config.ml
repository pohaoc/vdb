open! Core
open Bonsai_term

type t =
  | Hidden
  | Reasonable_default
  | Every_n_cells of
      { n : int
      ; make_label_string : Make_label_string.t
      }
  | Every_x_units of
      { x : float
      ; make_label_string : Make_label_string.t
      }
  | Custom of (int * string) list

let strings_to_views ~label_color =
  List.map ~f:(fun (index, s) ->
    let label_view =
      View.text ~attrs:(Option.map label_color ~f:Attr.fg |> Option.to_list) s
    in
    { Label.index; view = View.crop ~t:(View.height label_view - 1) label_view })
;;

let values_to_views label_values ~make_label_string ~label_color =
  List.map label_values ~f:(fun (index, value) -> index, make_label_string value)
  |> strings_to_views ~label_color
;;

let every_n_rows ~n ~make_label_string ~length ~axis_scale ~label_color =
  let n = Int.max n 1 in
  let labels =
    List.range 0 (length + 1) ~stride:n
    |> List.map ~f:(fun row_num ->
      row_num, Axis_scale.Internal.invert axis_scale (Float.of_int row_num))
    |> values_to_views ~make_label_string ~label_color
  in
  Axis_labels.view ~labels ~direction:`Vertical ~length
;;

let every_n_cols ~n ~make_label_string ~length ~axis_scale ~label_color =
  let n = Int.max n 1 in
  let label_positions = List.range ~stride:n 0 length in
  let labels =
    List.map label_positions ~f:(fun col ->
      let label =
        make_label_string (Axis_scale.Internal.invert axis_scale (Float.of_int col))
        |> View.text ~attrs:(Option.map label_color ~f:Attr.fg |> Option.to_list)
      in
      let label =
        if View.width label >= n
        then View.hcat [ View.crop ~r:(View.width label - n + 2) label; View.text "…" ]
        else label
      in
      { Label.index = col - (View.width label / 2); view = label })
    |> List.filter ~f:(fun { Label.index = start_col; view } ->
      start_col >= 0 && start_col + View.width view <= length)
  in
  Axis_labels.view ~labels ~direction:`Horizontal ~length
;;

let value_at_position_divisible_by_x ~x ~cell_position ~axis_scale =
  let value_at_cell_position =
    Axis_scale.Internal.invert axis_scale (Float.of_int cell_position)
  in
  let increasing_axis_scale =
    Float.( < )
      (Axis_scale.Internal.apply axis_scale 1.)
      (Axis_scale.Internal.apply axis_scale 2.)
  in
  if Float.Robustly_comparable.( =. ) x 0.
  then Some value_at_cell_position
  else (
    let round_dir = if increasing_axis_scale then `Up else `Down in
    let label_value = x *. Float.round ~dir:round_dir (value_at_cell_position /. x) in
    let%bind.Option cell_position_that_value_belongs_to =
      Axis_scale.Internal.apply axis_scale label_value |> Float.iround ~dir:`Down
    in
    if cell_position_that_value_belongs_to = cell_position then Some label_value else None)
;;

let every_x_units_vertical ~x ~make_label_string ~length ~axis_scale ~label_color =
  let labels =
    List.range 0 length
    |> List.filter_map ~f:(fun row_num ->
      let%map.Option label_value =
        value_at_position_divisible_by_x ~x ~cell_position:row_num ~axis_scale
      in
      row_num, label_value)
    |> values_to_views ~make_label_string ~label_color
  in
  Axis_labels.view ~labels ~direction:`Vertical ~length
;;

let every_x_units_horizontal ~x ~make_label_string ~length ~axis_scale ~label_color =
  let make_label ~label_value ~center_position =
    let label =
      make_label_string label_value
      |> View.text ~attrs:(Option.map label_color ~f:Attr.fg |> Option.to_list)
    in
    let index = center_position - (View.width label / 2) in
    Option.some_if (index >= 0) { Label.view = label; index }
  in
  let labels =
    List.range 0 length
    |> List.fold ~init:[] ~f:(fun acc col_num ->
      let label_and_start_pos =
        let%bind.Option label_value =
          value_at_position_divisible_by_x ~x ~cell_position:col_num ~axis_scale
        in
        make_label ~label_value ~center_position:col_num
      in
      match label_and_start_pos, acc with
      | None, _ -> acc
      | Some label, [] -> [ label ]
      | ( Some ({ Label.index; view = _ } as label)
        , { Label.index = prev_start_pos; Label.view = prev_label } :: _ ) ->
        if prev_start_pos + View.width prev_label >= index then acc else label :: acc)
  in
  Axis_labels.view ~labels ~direction:`Horizontal ~length
;;

module Internal = struct
  let resolve_y_labels
    t
    ~length
    ~min_value
    ~max_value
    ~axis_scale_config
    ~axis_scale
    ~label_color
    =
    match t with
    | Hidden -> None
    | Reasonable_default ->
      let make_label_string =
        match axis_scale_config with
        | Axis_scale_config.Linear _ | Default ->
          Make_label_string.make_reasonable_linear
            ~max_length:length
            ~min_value
            ~max_value
        | Logarithmic { base; min_value = _; max_value = _ } ->
          Make_label_string.make_reasonable_logarithmic ~base
      in
      Some (every_n_rows ~n:5 ~make_label_string ~length ~axis_scale ~label_color)
    | Every_n_cells { n; make_label_string } ->
      Some (every_n_rows ~n ~make_label_string ~length ~axis_scale ~label_color)
    | Every_x_units { x; make_label_string } ->
      Some (every_x_units_vertical ~x ~make_label_string ~length ~axis_scale ~label_color)
    | Custom labels ->
      Some
        (Axis_labels.view
           ~labels:(strings_to_views labels ~label_color)
           ~direction:`Vertical
           ~length)
  ;;

  let resolve_x_labels
    t
    ~length
    ~min_value
    ~max_value
    ~axis_scale_config
    ~axis_scale
    ~label_color
    =
    match t with
    | Hidden -> None
    | Reasonable_default ->
      let make_label_string =
        match axis_scale_config with
        | Axis_scale_config.Linear _ | Default ->
          Make_label_string.make_reasonable_linear
            ~max_length:length
            ~min_value
            ~max_value
        | Logarithmic { base; min_value = _; max_value = _ } ->
          Make_label_string.make_reasonable_logarithmic ~base
      in
      Some (every_n_cols ~n:10 ~make_label_string ~length ~axis_scale ~label_color)
    | Every_n_cells { n; make_label_string } ->
      Some (every_n_cols ~n ~make_label_string ~length ~axis_scale ~label_color)
    | Every_x_units { x; make_label_string } ->
      Some
        (every_x_units_horizontal ~x ~make_label_string ~length ~axis_scale ~label_color)
    | Custom labels ->
      Some
        (Axis_labels.view
           ~labels:(strings_to_views labels ~label_color)
           ~direction:`Horizontal
           ~length)
  ;;
end

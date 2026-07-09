open! Core
open Bonsai_term

let validate_option ~f = function
  | None -> Ok None
  | Some view -> f view |> Or_error.map ~f:Option.some
;;

let validate_view_option_has_data_width_if_some view_opt ~data_width ~view_name =
  validate_option view_opt ~f:(fun view ->
    let view_width = View.width view in
    match view_width = data_width with
    | true -> Ok view
    | false ->
      error_s
        [%message
          [%string "%{view_name} width <> data width"]
            (view_width : int)
            (data_width : int)])
;;

let validate_view_option_has_data_height_if_some view_opt ~data_height ~view_name =
  validate_option view_opt ~f:(fun view ->
    let view_height = View.height view in
    if view_height = data_height
    then Ok view
    else
      error_s
        [%message
          [%string "%{view_name} <> data height"] (view_height : int) (data_height : int)])
;;

let validate_x_labels ~x_labels ~data_width =
  validate_view_option_has_data_width_if_some x_labels ~data_width ~view_name:"x labels"
;;

let validate_y_labels ~y_labels ~data_height =
  validate_view_option_has_data_height_if_some y_labels ~data_height ~view_name:"y labels"
;;

let validate_x_axis_title ~x_axis_title ~data_width =
  validate_view_option_has_data_width_if_some
    x_axis_title
    ~data_width
    ~view_name:"x axis title"
;;

let validate_y_axis_title ~y_axis_title ~data_height =
  validate_view_option_has_data_height_if_some
    y_axis_title
    ~data_height
    ~view_name:"y axis title"
;;

let validate_title ~title ~data_width =
  validate_view_option_has_data_width_if_some title ~data_width ~view_name:"title"
;;

let validate_dimensions_exn
  ~data_view
  ~x_labels
  ~y_labels
  ~x_axis_title
  ~y_axis_title
  ~title
  =
  let { Dimensions.width = data_width; height = data_height } =
    View.dimensions data_view
  in
  [ validate_x_labels ~x_labels ~data_width
  ; validate_y_labels ~y_labels ~data_height
  ; validate_x_axis_title ~x_axis_title ~data_width
  ; validate_y_axis_title ~y_axis_title ~data_height
  ; validate_title ~title ~data_width
  ]
  |> List.map ~f:(Or_error.map ~f:(fun _view -> ()))
  |> Or_error.combine_errors_unit
  |> Or_error.ok_exn
;;

let vcat_right_align views =
  let greatest_width =
    List.map views ~f:View.width
    |> List.max_elt ~compare:Int.compare
    |> Option.value ~default:0
  in
  List.map views ~f:(fun view ->
    let l_pad = greatest_width - View.width view in
    View.pad ~l:l_pad view)
  |> View.vcat
;;

(* Does not check if the components have the correct dimensions. *)
let view_validated ~data_view ~x_labels ~y_labels ~x_axis_title ~y_axis_title ~title =
  let cells_above_data_view = Option.value_map title ~default:0 ~f:View.height in
  [ Option.map y_axis_title ~f:(View.pad ~t:cells_above_data_view)
  ; Option.map y_labels ~f:(View.pad ~t:cells_above_data_view)
  ; List.filter_opt [ title; Some data_view; x_labels; x_axis_title ]
    |> vcat_right_align
    |> Some
  ]
  |> List.filter_opt
  |> View.hcat
;;

let view ~data_view ~x_labels ~y_labels ~x_axis_title ~y_axis_title ~title =
  let { Dimensions.width = data_width; height = data_height } =
    View.dimensions data_view
  in
  let view_opt_from_or_error = function
    | Ok view -> view
    | Error _ -> None
  in
  let x_labels = validate_x_labels ~x_labels ~data_width |> view_opt_from_or_error in
  let y_labels = validate_y_labels ~y_labels ~data_height |> view_opt_from_or_error in
  let x_axis_title =
    validate_x_axis_title ~x_axis_title ~data_width |> view_opt_from_or_error
  in
  let y_axis_title =
    validate_y_axis_title ~y_axis_title ~data_height |> view_opt_from_or_error
  in
  let title = validate_title ~title ~data_width |> view_opt_from_or_error in
  view_validated ~data_view ~x_labels ~y_labels ~x_axis_title ~y_axis_title ~title
;;

let view_exn ~data_view ~x_labels ~y_labels ~x_axis_title ~y_axis_title ~title =
  validate_dimensions_exn
    ~data_view
    ~x_labels
    ~y_labels
    ~x_axis_title
    ~y_axis_title
    ~title;
  view_validated ~data_view ~x_labels ~y_labels ~x_axis_title ~y_axis_title ~title
;;

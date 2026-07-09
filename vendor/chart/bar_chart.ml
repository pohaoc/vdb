open! Core
open Bonsai_term

let truncate_text s max_length =
  let utf8_string = String.Utf8.of_string s in
  let total_width = Bonsai_term_typography.Tty_width.string_width s in
  if total_width = 0 || total_width <= max_length
  then s
  else (
    (* Reserve space for the ellipsis character *)
    let max_width = max_length - 1 in
    let truncated, _ =
      String.Utf8.fold utf8_string ~init:([], 0) ~f:(fun (acc, current_width) uchar ->
        let char_width = View.uchar_tty_width uchar in
        if current_width + char_width <= max_width
        then uchar :: acc, current_width + char_width
        else acc, current_width)
    in
    String.Utf8.to_string (String.Utf8.of_list (List.rev truncated)) ^ "…")
;;

module Bar_spec = struct
  type t =
    { value : float
    ; label : string option
    ; color : Attr.Color.t option
    }
  [@@deriving fields ~getters]
end

module Bar_width_config = struct
  type t =
    | Custom of
        { width : int
        ; padding : int
        }
    | Choose_for_me_from_max_total_width of int

  let calculate_bar_width ~width_for_bars ~num_bars =
    (* In order of importance, our priorities are that:
       1. [bar_width] >= 1.
       2. Total width of bars and padding does not exceed [width_for_bars].
       3. [padding] >= 1.
       4. [padding] is as large as possible, but the total width taken up by bars is at
          least 3x the total width taken up by padding.

       This logic is slightly complicated by the fact that there are N bars but N + 1
       segments of padding. *)
    if num_bars <= 0
    then ~width:1, ~padding:0
    else if width_for_bars <= 2 * num_bars
    then ~width:(Int.max 1 (width_for_bars / num_bars)), ~padding:0
    else (
      let padding = Int.max 1 (width_for_bars / (4 * (num_bars + 1))) in
      let width = (width_for_bars - ((num_bars + 1) * padding)) / num_bars in
      (* Because we're rounding down when calculating the width, we sometimes we violate
         the condition that bars to take up 3x the space that the padding does so we need
         to reduce the padding by 1. *)
      if padding = 1 || num_bars * width >= 3 * (num_bars + 1) * padding
      then ~width, ~padding
      else (
        let padding = padding - 1 in
        ~width:((width_for_bars - ((num_bars + 1) * padding)) / num_bars), ~padding))
  ;;

  (* We need to know the width added by the y labels and y axis title if we're going to
     try to fit the whole graph to a certain width. *)
  let get_bar_width t ~y_labels_length ~y_axis_title_length ~num_bars =
    match t with
    | Custom { width; padding } -> ~width:(Int.max 1 width), ~padding:(Int.max 0 padding)
    | Choose_for_me_from_max_total_width total_width ->
      let width_for_bars =
        total_width
        - y_labels_length
        - y_axis_title_length
        - 1 (* reserve 1 unit for the border. *)
      in
      calculate_bar_width ~width_for_bars ~num_bars
  ;;
end

let x_labels ~label_color ~bar_width ~bar_padding ~labels =
  let total_width_per_bar = bar_width + bar_padding in
  let center view =
    let leftover_width = total_width_per_bar - View.width view in
    if leftover_width % 2 = 0
    then View.pad ~l:(leftover_width / 2) view
    else if bar_padding % 2 = 0
            (* If we can't perfectly center a label within its allocated width, we prefer
               for the label to be shifted one character to the left. *)
    then View.pad ~l:(leftover_width / 2) view
    else View.pad ~l:((leftover_width / 2) + 1) view
  in
  let label_start_pos i = (bar_padding / 2) + (total_width_per_bar * i) + 1 in
  let labels =
    List.mapi labels ~f:(fun i s ->
      { Label.index = label_start_pos i
      ; view =
          truncate_text s (total_width_per_bar - 1)
          |> View.text ~attrs:(Option.map label_color ~f:Attr.fg |> Option.to_list)
          |> center
      })
  in
  Axis_labels.view
    ~labels
    ~direction:`Horizontal
    ~length:((total_width_per_bar * List.length labels) + bar_padding + 1)
;;

let bars_view bars ~bar_height ~max_bar_height ~bar_width ~bar_padding =
  match List.is_empty bars with
  | true ->
    View.transparent_rectangle ~width:(Int.max bar_padding 1) ~height:max_bar_height
  | false ->
    let bars =
      List.map bars ~f:(fun { Bar_spec.value; color; label = _ } ->
        Bar.view
          ~bar_height:(Axis_scale.Internal.apply bar_height value)
          ~max_bar_height
          ~bar_width
          ~color)
    in
    let views =
      List.mapi bars ~f:(fun i bar ->
        let l = if i = 0 then bar_padding else 0 in
        View.pad bar ~l ~r:bar_padding)
    in
    View.hcat views
;;

let create_view
  ?(theme = Theme.catppuccin ~flavor:Bonsai_term_catppuccin.Mocha.flavor ~data_color:Blue)
  ?(show_x_labels = true)
  ?(y_labels_config = Scalar_axis_labels_config.Reasonable_default)
  ?x_axis_title
  ?y_axis_title
  ?title
  ?(bar_height_config = Axis_scale_config.Default)
  data
  ~max_bar_height
  ~bar_width_config
  ~view_fn
  =
  (* We filter out non-finite values when constructing the axis labels and axis scale, but
     leave the bar in when rendering the bars. The bar will have a height of 0, but still
     takes up horizontal space and its label will be shown. *)
  let finite_data_values =
    List.map data ~f:Bar_spec.value |> List.filter ~f:Float.is_finite
  in
  let bar_height =
    Axis_scale_config.Internal.resolve
      bar_height_config
      ~data:finite_data_values
      ~length:max_bar_height
  in
  let data =
    List.map data ~f:(fun bar ->
      { bar with color = (if Option.is_some bar.color then bar.color else theme.data) })
  in
  let y_labels =
    let ~min_value, ~max_value =
      Axis_scale_config.Internal.extrema bar_height_config ~data:finite_data_values
    in
    Scalar_axis_labels_config.Internal.resolve_y_labels
      y_labels_config
      ~length:(max_bar_height + 1)
      ~min_value
      ~max_value
      ~axis_scale_config:bar_height_config
      ~axis_scale:bar_height
      ~label_color:theme.label_text
  in
  let ~width:bar_width, ~padding:bar_padding =
    let y_labels_length = Option.value_map y_labels ~default:0 ~f:View.width in
    let y_axis_title_length = Option.value_map y_axis_title ~default:0 ~f:View.width in
    Bar_width_config.get_bar_width
      bar_width_config
      ~y_labels_length
      ~y_axis_title_length
      ~num_bars:(List.length data)
  in
  let data_view =
    bars_view data ~bar_height ~max_bar_height ~bar_width ~bar_padding
    |> Bonsai_term_border_box.view
         ~attrs:(Option.map theme.border ~f:Attr.fg |> Option.to_list)
         ~line_type:Thick
         ~hide_right:true
         ~hide_top:true
  in
  let { Dimensions.width = data_width; height = data_height } =
    View.dimensions data_view
  in
  let x_labels =
    Option.some_if
      (show_x_labels && not (List.is_empty data))
      (let labels =
         List.map data ~f:Bar_spec.label |> List.map ~f:(Option.value ~default:"")
       in
       x_labels ~label_color:theme.label_text ~bar_width ~bar_padding ~labels)
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

module For_testing = struct
  let calculate_bar_width = Bar_width_config.calculate_bar_width
  let view_exn = create_view ~view_fn:Graph.view_exn
end

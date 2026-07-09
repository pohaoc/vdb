open! Core
open Bonsai_term

module Cell_info = struct
  type t =
    { icon : Scatter_chart_icon.t
    ; color : (Attr.Color.t option[@sexp.opaque])
    ; background_color : (Attr.Color.t option[@sexp.opaque])
    ; collision : bool
    }
  [@@deriving sexp_of]

  let resolve_collision
    { icon; color; collision; background_color = _ }
    ~collision_resolution:
      { Scatter_chart_collision_resolution.background
      ; icon_color = collision_color
      ; icon = collision_icon
      }
    =
    let icon =
      match collision, collision_icon with
      | false, (`Take_first | `Special _) | true, `Take_first -> icon
      | true, `Special icon -> icon
    in
    let color =
      match collision, collision_color with
      | false, (`Take_first | `Special _) | true, `Take_first -> color
      | true, `Special color -> Some color
    in
    let background_color =
      match collision, background with
      | false, (None | Some _) | true, None -> None
      | true, Some background_color -> Some background_color
    in
    { icon; color; background_color; collision }
  ;;

  let view { icon; color; background_color; collision = _ } =
    let fg_color_attr = Option.map color ~f:Attr.fg in
    let bg_color_attr = Option.map background_color ~f:Attr.bg in
    let attrs = List.filter_opt [ fg_color_attr; bg_color_attr ] in
    Scatter_chart_icon.view ~attrs icon
  ;;
end

let make_view cell_info_map ~height ~width =
  let row_view y =
    List.range 0 width
    |> List.map ~f:(fun x ->
      Map.find cell_info_map { Position.x; y }
      |> Option.value_map ~default:(View.text " ") ~f:Cell_info.view)
    |> View.hcat
  in
  List.range ~stride:(-1) (height - 1) (-1) |> List.map ~f:row_view |> View.vcat
;;

let position_map points ~collision_resolution ~height ~width =
  List.filter
    points
    ~f:(fun { Scatter_chart_point.coordinate = { Position.x; y }; icon = _; color = _ } ->
      Int.between x ~low:0 ~high:(width - 1) && Int.between y ~low:0 ~high:(height - 1))
  |> List.fold
       ~init:(Map.empty (module Position))
       ~f:(fun acc { Scatter_chart_point.coordinate = position; icon; color } ->
         Map.update acc position ~f:(function
           | None -> { Cell_info.icon; color; collision = false; background_color = None }
           | Some cell_info -> { cell_info with collision = true }))
  |> Map.map ~f:(Cell_info.resolve_collision ~collision_resolution)
;;

let view points ~collision_resolution ~height ~width =
  position_map points ~collision_resolution ~height ~width |> make_view ~height ~width
;;

module For_testing = struct
  module Cell_info = Cell_info

  let position_map = position_map
end

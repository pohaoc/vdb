open! Core
open Bonsai_term

module Cell_info = struct
  type t =
    { covered_cells : Set.M(Position).t
    ; collision : bool
    }
  [@@deriving sexp_of]

  let empty = { covered_cells = Set.empty (module Position); collision = false }

  let add { covered_cells; collision } ~sub_cell_position =
    let collision = collision || Set.mem covered_cells sub_cell_position in
    let covered_cells = Set.add covered_cells sub_cell_position in
    { covered_cells; collision }
  ;;

  (* Braille characters start at code point U+2800. Each subpixel has a bit associated
     with it. *)
  let bit_for_dot { Position.x; y } =
    match x, y with
    | 0, 3 -> 0x01 (* ⠁ *)
    | 0, 2 -> 0x02 (* ⠂ *)
    | 0, 1 -> 0x04 (* ⠄ *)
    | 1, 3 -> 0x08 (* ⠈ *)
    | 1, 2 -> 0x10 (* ⠐ *)
    | 1, 1 -> 0x20 (* ⠠ *)
    | 0, 0 -> 0x40 (* ⡀ *)
    | 1, 0 -> 0x80 (* ⢀ *)
    | _ -> 0
  ;;

  let base_braille_codepoint = 0x2800

  let braille_char covered_cells =
    let codepoint =
      Set.fold covered_cells ~init:base_braille_codepoint ~f:(fun acc coord ->
        acc lor bit_for_dot coord)
    in
    Uchar.Utf8.to_string (Uchar.of_scalar_exn codepoint)
  ;;

  let resolve { covered_cells; collision } ~color ~collision_resolution =
    match
      collision, (collision_resolution : Subpixel_scatter_chart_collision_resolution.t)
    with
    | false, _ | true, Ignore_collision ->
      ~text:(braille_char covered_cells), ~fg_color:color, ~bg_color:None
    | true, Special { icon; color = collision_color; background } ->
      let text =
        match icon with
        | None -> braille_char covered_cells
        | Some scatter_chart_icon -> Scatter_chart_icon.to_string scatter_chart_icon
      in
      ~text, ~fg_color:(Option.first_some collision_color color), ~bg_color:background
  ;;

  let view t ~color ~collision_resolution =
    let ~text, ~fg_color, ~bg_color = resolve t ~color ~collision_resolution in
    let attrs =
      List.filter_opt [ Option.map fg_color ~f:Attr.fg; Option.map bg_color ~f:Attr.bg ]
    in
    View.text ~attrs text
  ;;
end

let make_view cell_info_map ~color ~collision_resolution ~height ~width =
  let row_view y =
    List.range 0 width
    |> List.map ~f:(fun x ->
      Map.find cell_info_map { Position.x; y }
      |> Option.value_map
           ~default:(View.text " ")
           ~f:(Cell_info.view ~color ~collision_resolution))
    |> View.hcat
  in
  List.range ~stride:(-1) (height - 1) (-1) |> List.map ~f:row_view |> View.vcat
;;

let position_map points ~height ~width =
  List.filter points ~f:(fun (~x, ~y) ->
    Float.is_finite x
    && Float.( >= ) x 0.
    && Float.( < ) x (Float.of_int width)
    && Float.is_finite y
    && Float.( >= ) y 0.
    && Float.( < ) y (Float.of_int height))
  |> List.fold
       ~init:(Map.empty (module Position))
       ~f:(fun acc (~x, ~y) ->
         let cell_position =
           { Position.x = Float.iround_exn ~dir:`Down x
           ; y = Float.iround_exn ~dir:`Down y
           }
         in
         let sub_cell_position =
           { Position.x = (x *. 2. |> Float.iround_exn ~dir:`Down) % 2
           ; y = (y *. 4. |> Float.iround_exn ~dir:`Down) % 4
           }
         in
         Map.update acc cell_position ~f:(function
           | None -> Cell_info.add Cell_info.empty ~sub_cell_position
           | Some cell_info -> Cell_info.add cell_info ~sub_cell_position))
;;

let view points ~color ~collision_resolution ~height ~width =
  position_map points ~height ~width
  |> make_view ~color ~collision_resolution ~height ~width
;;

module For_testing = struct
  module Cell_info = Cell_info
end

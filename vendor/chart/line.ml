open! Core
open Bonsai_term

module Dir = struct
  type t =
    | Right
    | Left
    | Up
    | Down
  [@@deriving sexp_of, compare]

  let opposite = function
    | Right -> Left
    | Up -> Down
    | Down -> Up
    | Left -> Right
  ;;

  include functor Comparator.Make
end

module Cell_info = struct
  type t =
    { dirs : Set.M(Dir).t
    ; color : Attr.Color.t option
    ; line_type : Line_type.t
    }

  let view { dirs; color; line_type } =
    let ~up, ~down, ~left, ~right =
      ( ~up:(Set.mem dirs Dir.Up)
      , ~down:(Set.mem dirs Dir.Down)
      , ~left:(Set.mem dirs Dir.Left)
      , ~right:(Set.mem dirs Dir.Right) )
    in
    let s =
      match line_type, ~up, ~down, ~left, ~right with
      | _, ~up:false, ~down:false, ~left:false, ~right:false -> " "
      | (Thin | Round), ~up:false, ~down:false, ~left:true, ~right:_
      | (Thin | Round), ~up:false, ~down:false, ~left:_, ~right:true -> "─"
      | (Thin | Round), ~up:true, ~down:_, ~left:false, ~right:false
      | (Thin | Round), ~up:_, ~down:true, ~left:false, ~right:false -> "│"
      | (Thin | Round), ~up:true, ~down:false, ~left:true, ~right:true -> "┴"
      | (Thin | Round), ~up:true, ~down:true, ~left:false, ~right:true -> "├"
      | (Thin | Round), ~up:false, ~down:true, ~left:true, ~right:true -> "┬"
      | (Thin | Round), ~up:true, ~down:true, ~left:true, ~right:false -> "┤"
      | (Thin | Round), ~up:true, ~down:true, ~left:true, ~right:true -> "┼"
      | Thin, ~up:true, ~down:false, ~left:true, ~right:false -> "┘"
      | Thin, ~up:true, ~down:false, ~left:false, ~right:true -> "└"
      | Thin, ~up:false, ~down:true, ~left:false, ~right:true -> "┌"
      | Thin, ~up:false, ~down:true, ~left:true, ~right:false -> "┐"
      | Round, ~up:true, ~down:false, ~left:true, ~right:false -> "╯"
      | Round, ~up:true, ~down:false, ~left:false, ~right:true -> "╰"
      | Round, ~up:false, ~down:true, ~left:false, ~right:true -> "╭"
      | Round, ~up:false, ~down:true, ~left:true, ~right:false -> "╮"
      | Thick, ~up:false, ~down:false, ~left:true, ~right:_
      | Thick, ~up:false, ~down:false, ~left:_, ~right:true -> "━"
      | Thick, ~up:true, ~down:_, ~left:false, ~right:false
      | Thick, ~up:_, ~down:true, ~left:false, ~right:false -> "┃"
      | Thick, ~up:true, ~down:false, ~left:true, ~right:false -> "┛"
      | Thick, ~up:true, ~down:false, ~left:false, ~right:true -> "┗"
      | Thick, ~up:false, ~down:true, ~left:false, ~right:true -> "┏"
      | Thick, ~up:false, ~down:true, ~left:true, ~right:false -> "┓"
      | Thick, ~up:true, ~down:false, ~left:true, ~right:true -> "┻"
      | Thick, ~up:true, ~down:true, ~left:false, ~right:true -> "┣"
      | Thick, ~up:false, ~down:true, ~left:true, ~right:true -> "┳"
      | Thick, ~up:true, ~down:true, ~left:true, ~right:false -> "┫"
      | Thick, ~up:true, ~down:true, ~left:true, ~right:true -> "╋"
      | Double, ~up:false, ~down:false, ~left:true, ~right:_
      | Double, ~up:false, ~down:false, ~left:_, ~right:true -> "═"
      | Double, ~up:true, ~down:_, ~left:false, ~right:false
      | Double, ~up:_, ~down:true, ~left:false, ~right:false -> "║"
      | Double, ~up:true, ~down:false, ~left:true, ~right:false -> "╝"
      | Double, ~up:true, ~down:false, ~left:false, ~right:true -> "╚"
      | Double, ~up:false, ~down:true, ~left:false, ~right:true -> "╔"
      | Double, ~up:false, ~down:true, ~left:true, ~right:false -> "╗"
      | Double, ~up:true, ~down:false, ~left:true, ~right:true -> "╩"
      | Double, ~up:true, ~down:true, ~left:false, ~right:true -> "╠"
      | Double, ~up:false, ~down:true, ~left:true, ~right:true -> "╦"
      | Double, ~up:true, ~down:true, ~left:true, ~right:false -> "╣"
      | Double, ~up:true, ~down:true, ~left:true, ~right:true -> "╬"
    in
    View.text ~attrs:(Option.map color ~f:Attr.fg |> Option.to_list) s
  ;;
end

module Dir_map = struct
  type t =
    { dir_map : Cell_info.t Map.M(Position).t
    ; height : int
    ; width : int
    }

  let in_bounds { height; width; dir_map = _ } { Position.x; y } =
    Int.between x ~low:0 ~high:(width - 1) && Int.between y ~low:0 ~high:(height - 1)
  ;;

  let add_dir ({ dir_map; height = _; width = _ } as t) ~position ~dir ~color ~line_type =
    match in_bounds t position with
    | false -> t
    | true ->
      let dir_map =
        Map.update dir_map position ~f:(function
          | None ->
            let dirs = Set.singleton (module Dir) dir in
            { dirs; color; line_type }
          | Some { dirs; color; line_type } ->
            let dirs = Set.add dirs dir in
            { dirs; color; line_type })
      in
      { t with dir_map }
  ;;

  let view { dir_map; height; width } =
    let row_view y =
      List.range 0 width
      |> List.map ~f:(fun x ->
        Map.find dir_map { Position.x; y }
        |> Option.value_map ~default:(View.text " ") ~f:Cell_info.view)
      |> View.hcat
    in
    List.range ~stride:(-1) (height - 1) (-1) |> List.map ~f:row_view |> View.vcat
  ;;
end

(* Liang-Barsky line clipping algorithm. See
   https://en.wikipedia.org/wiki/Liang%E2%80%93Barsky_algorithm *)
let clip_line ~start:(~x:x1, ~y:y1) ~end_:(~x:x2, ~y:y2) ~x_min ~x_max ~y_min ~y_max =
  let clip ~p ~q =
    match p with
    | p when Float.Robustly_comparable.( =. ) p 0. ->
      if Float.( >= ) q 0.
      then 0., 1.
      else
        (* the line doesn't intersect the rectangle, so we set [t_min] to a value greater
           than [t_max] *)
        1., 0.
    | p when Float.( < ) p 0. -> q /. p, 1.
    | p -> 0., q /. p
  in
  let dx = x2 -. x1 in
  let dy = y2 -. y1 in
  let t_min, t_max =
    [ clip ~p:(Float.neg dx) ~q:(x1 -. x_min)
    ; clip ~p:dx ~q:(x_max -. x1)
    ; clip ~p:(Float.neg dy) ~q:(y1 -. y_min)
    ; clip ~p:dy ~q:(y_max -. y1)
    ]
    |> List.fold ~init:(0., 1.) ~f:(fun (t_min, t_max) (cur_t_min, cur_t_max) ->
      Float.max t_min cur_t_min, Float.min t_max cur_t_max)
  in
  Option.some_if
    (Float.( < ) t_min t_max)
    ( ~start:(~x:(x1 +. (t_min *. dx)), ~y:(y1 +. (t_min *. dy)))
    , ~end_:(~x:(x1 +. (t_max *. dx)), ~y:(y1 +. (t_max *. dy))) )
;;

let add_line_segment_to_dir_map_clipped
  dir_map
  ~start:(~x:x1, ~y:y1)
  ~end_:(~x:x2, ~y:y2)
  ~color
  ~line_type
  =
  let ({ Position.x = end_cell_x; y = end_cell_y } as end_cell_coordinate) =
    { Position.x = Float.iround_exn ~dir:`Down x2; y = Float.iround_exn ~dir:`Down y2 }
  in
  (* interpolate between [start] and [end_]. This will never cause a divide by 0 error. In
     order for that to happen, [x2] would have to equal [x1], which can only happen
     there's no line to draw or we're drawing a vertical line.

     In the case of [start] equalling [end_], we return in the first call of [aux]. For
     vertical lines, we check that [x <> end_cell_x] before calling [get_y_coordinate]. *)
  let get_y_coordinate ~x =
    y1 +. ((y2 -. y1) *. (Float.of_int x -. x1) /. (x2 -. x1))
    |> Float.iround_exn ~dir:`Nearest
  in
  let should_move_right { Position.x; y } =
    x < end_cell_x && (y = end_cell_y || get_y_coordinate ~x:(x + 1) = y)
  in
  let should_move_left { Position.x; y } =
    x > end_cell_x && (y = end_cell_y || get_y_coordinate ~x:(x - 1) = y)
  in
  let should_move_up ({ Position.x = _; y } as pos) =
    y < end_cell_y && (not (should_move_right pos)) && not (should_move_left pos)
  in
  let rec aux dir_map ({ Position.x; y } as pos) =
    if Position.equal pos end_cell_coordinate
    then dir_map
    else (
      let dir, next_pos =
        if should_move_right pos
        then Dir.Right, { Position.x = x + 1; y }
        else if should_move_left pos
        then Left, { Position.x = x - 1; y }
        else if should_move_up pos
        then Up, { Position.x; y = y + 1 }
        else Down, { Position.x; y = y - 1 }
      in
      aux
        (Dir_map.add_dir dir_map ~position:pos ~dir ~color ~line_type
         |> Dir_map.add_dir ~position:next_pos ~dir:(Dir.opposite dir) ~color ~line_type)
        next_pos)
  in
  aux
    dir_map
    { Position.x = Float.iround_exn ~dir:`Down x1; y = Float.iround_exn ~dir:`Down y1 }
;;

let add_line_segment_to_dir_map
  ({ Dir_map.height; width; dir_map = _ } as dir_map)
  ~start
  ~end_
  ~color
  ~line_type
  =
  (* Make the boundaries slightly larger than they need to be to make sure that we draw
     the section where the line goes from out of bounds to in bounds. *)
  let x_min, y_min = -2., -2. in
  let x_max, y_max = Float.of_int (width + 2), Float.of_int (height + 2) in
  match clip_line ~start ~end_ ~x_min ~y_min ~x_max ~y_max with
  | None -> (* the line never intersects the canvas *) dir_map
  | Some (~start, ~end_) ->
    add_line_segment_to_dir_map_clipped dir_map ~start ~end_ ~color ~line_type
;;

let add_line_to_dir_map dir_map { Line_spec.coordinates; color; line_type } =
  let rec aux acc = function
    | [ _ ] | [] -> acc
    | start :: end_ :: tl ->
      aux (add_line_segment_to_dir_map acc ~start ~end_ ~color ~line_type) (end_ :: tl)
  in
  aux dir_map coordinates
;;

(* The API would be more clear if [Line_spec.t] was parameterized on the coordinate type
   (so this function could take a [Position.t Line_spec.t]), but that loses some
   information.

   If you draw a line from (0.9, 0.0) -> (1.9, 1.0), you would go from position (0, 0) ->
   (1, 0) -> (1, 1).

   On the other hand, if you draw a line from (0.0, 0.9) -> (1.0, 1.9), you would go from
   position (0, 0) -> (0, 1) -> (1, 1). *)
let view lines ~height ~width =
  List.map lines ~f:Line_spec.filter_to_finite_coordinates
  |> List.fold
       ~init:{ Dir_map.dir_map = Map.empty (module Position); height; width }
       ~f:add_line_to_dir_map
  |> Dir_map.view
;;

module For_testing = struct
  let clip_line = clip_line
end

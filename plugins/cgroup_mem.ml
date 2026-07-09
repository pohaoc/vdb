open! Core
open Bonsai_term
open Bonsai.Let_syntax
module Catppuccin = Bonsai_term_catppuccin
module Line_chart = Bonsai_term_chart.Line_chart
module Line_spec = Bonsai_term_chart.Line_spec
module Sampler = Vdb.Sampler

let color = Catppuccin.color ~flavor:Catppuccin.Mocha.flavor
let cgroup_root = "/sys/fs/cgroup"

(* Where this process lives in the unified hierarchy, e.g.
   "0::/user.slice/user-1000.slice/session-1.scope". *)
let self_cgroup () =
  match
    In_channel.read_lines "/proc/self/cgroup"
    |> List.find_map ~f:(String.chop_prefix ~prefix:"0::")
  with
  | Some path -> Ok (String.rstrip path)
  | None -> Or_error.error_string "no cgroup v2 entry in /proc/self/cgroup"
;;

let resolve_path ~cgroup =
  let is_file path =
    match Stdlib.Sys.file_exists path, Stdlib.Sys.is_directory path with
    | true, false -> true
    | _ -> false
    | exception _ -> false
  in
  match cgroup with
  | Some path when is_file path -> Ok path
  | Some path when String.is_prefix path ~prefix:"/" ->
    Ok (path ^/ "memory.current")
  | Some path -> Ok (cgroup_root ^/ path ^/ "memory.current")
  | None ->
    let%map.Or_error self = self_cgroup () in
    cgroup_root ^ self ^/ "memory.current"
;;

let parse contents =
  match Float.of_string (String.strip contents) with
  | value -> Ok value
  | exception _ ->
    Or_error.error_string (sprintf "not a number: %s" (String.strip contents))
;;

let mib bytes = bytes /. (1024. *. 1024.)

let format_mib bytes =
  let mib = mib bytes in
  if Float.(mib >= 1024.)
  then sprintf "%.2f GiB" (mib /. 1024.)
  else sprintf "%.1f MiB" mib
;;

let y_axis_option =
  Vdb.Plugin.Option_spec.create ~key:"y-axis" [ "fit data"; "anchor at 0" ]
;;

(* An in-pane browser of the cgroup hierarchy: [b] opens it, arrows move, right/left
   expand/collapse, Enter watches the selected cgroup. *)
module Browser = struct
  module Row = struct
    type t =
      { dir : string (** Absolute cgroup directory. *)
      ; label : string
      ; depth : int
      ; expanded : bool
      ; mem : string (** memory.current at scan time, formatted. *)
      }
  end

  module Model = struct
    type t =
      { browsing : bool
      ; cursor : int
      ; rows : Row.t list
      ; watched : string (** Absolute path of the file being sampled. *)
      }
  end

  module Action = struct
    type t =
      | Open
      | Close
      | Cursor of int
      | Expand
      | Collapse
      | Select
    [@@deriving sexp_of]
  end

  let read_mem_label dir =
    match In_channel.read_all (dir ^/ "memory.current") with
    | contents ->
      (match parse contents with
       | Ok value -> format_mib value
       | Error _ -> "?")
    | exception _ -> "?"
  ;;

  let read_children ~depth dir =
    match Stdlib.Sys.readdir dir with
    | exception _ -> []
    | entries ->
      Array.to_list entries
      |> List.sort ~compare:String.compare
      |> List.filter_map ~f:(fun entry ->
        let sub = dir ^/ entry in
        match
          Stdlib.Sys.is_directory sub
          && Stdlib.Sys.file_exists (sub ^/ "memory.current")
        with
        | true ->
          Some
            { Row.dir = sub
            ; label = entry
            ; depth
            ; expanded = false
            ; mem = read_mem_label sub
            }
        | false | exception _ -> None)
  ;;

  (* The filesystem reads below are synchronous, but they are tiny cgroupfs reads done
     once per keypress, not per frame. *)
  let apply_action _ctx (model : Model.t) (action : Action.t) =
    let clamp_cursor cursor =
      Int.clamp_exn cursor ~min:0 ~max:(Int.max 0 (List.length model.rows - 1))
    in
    match action with
    | Open ->
      { model with browsing = true; cursor = 0; rows = read_children ~depth:0 cgroup_root }
    | Close -> { model with browsing = false }
    | Cursor delta -> { model with cursor = clamp_cursor (model.cursor + delta) }
    | Expand ->
      (match List.nth model.rows model.cursor with
       | Some row when not row.expanded ->
         (match read_children ~depth:(row.depth + 1) row.dir with
          | [] -> model
          | children ->
            let before, after = List.split_n model.rows model.cursor in
            let after = List.tl after |> Option.value ~default:[] in
            { model with
              rows = before @ ({ row with expanded = true } :: children) @ after
            })
       | _ -> model)
    | Collapse ->
      (match List.nth model.rows model.cursor with
       | Some row when row.expanded ->
         let before, after = List.split_n model.rows model.cursor in
         let after =
           List.tl after
           |> Option.value ~default:[]
           |> List.drop_while ~f:(fun (r : Row.t) -> r.depth > row.depth)
         in
         { model with rows = before @ ({ row with expanded = false } :: after) }
       | _ -> model)
    | Select ->
      (match List.nth model.rows model.cursor with
       | Some row ->
         { model with watched = row.dir ^/ "memory.current"; browsing = false }
       | None -> model)
  ;;

  let view ~locked ~(model : Model.t) ~(dimensions : Dimensions.t) =
    let header =
      View.text ~attrs:[ Attr.bold; Attr.fg (color Lavender) ] "Browse cgroups"
    in
    let footer =
      if locked
      then
        View.hcat
          [ View.text
              ~attrs:[ Attr.fg (color Peach) ]
              "🔒 recording — switching locked · "
          ; View.text
              ~attrs:[ Attr.fg (color Overlay0) ]
              "↑/↓ move · →/← expand/collapse · Esc close"
          ]
      else
        View.text
          ~attrs:[ Attr.fg (color Overlay0) ]
          "↑/↓ move · → expand · ← collapse · Enter watch · Esc close"
    in
    let visible_height = Int.max 1 (dimensions.height - 3) in
    let offset =
      if List.length model.rows <= visible_height
      then 0
      else
        Int.clamp_exn
          (model.cursor - (visible_height / 2))
          ~min:0
          ~max:(List.length model.rows - visible_height)
    in
    let rows =
      List.sub model.rows ~pos:offset ~len:(Int.min visible_height (List.length model.rows - offset))
      |> List.mapi ~f:(fun i (row : Row.t) ->
        let index = i + offset in
        let selected = index = model.cursor in
        let watched = String.equal (row.dir ^/ "memory.current") model.watched in
        let marker = if row.expanded then "▾ " else "▸ " in
        let attrs =
          if selected
          then [ Attr.bold; Attr.fg (color Text); Attr.bg (color Surface1) ]
          else if watched
          then [ Attr.bold; Attr.fg (color Sapphire) ]
          else [ Attr.fg (color Subtext0) ]
        in
        View.hcat
          [ View.text
              ~attrs
              (sprintf
                 "%s%s%s%s"
                 (String.make (2 * row.depth) ' ')
                 marker
                 row.label
                 (if watched then " ●" else ""))
          ; View.text ~attrs:[ Attr.fg (color Overlay1) ] ("  " ^ row.mem)
          ])
    in
    let rows =
      match rows with
      | [] -> [ View.text ~attrs:[ Attr.fg (color Red) ] "no cgroups found" ]
      | _ :: _ -> rows
    in
    View.vcat ((header :: rows) @ [ footer ])
  ;;

  let handler ~locked ~inject (event : Event.t) =
    match event with
    | Key_press { key = Arrow `Up; _ } | Mouse { kind = Scroll `Up; _ } ->
      inject (Action.Cursor (-1))
    | Key_press { key = Arrow `Down; _ } | Mouse { kind = Scroll `Down; _ } ->
      inject (Action.Cursor 1)
    | Key_press { key = Arrow `Right; _ } -> inject Action.Expand
    | Key_press { key = Arrow `Left; _ } -> inject Action.Collapse
    | Key_press { key = Enter; _ } ->
      (* Switching the data source mid-recording would silently change what the
         recording measures. *)
      if locked then Effect.Ignore else inject Action.Select
    | Key_press { key = Escape; _ } | Key_press { key = ASCII 'b'; _ } ->
      inject Action.Close
    | _ -> Effect.Ignore
  ;;
end

let chart ~y_axis ~(samples : Sampler.Sample.t list) ~width ~height =
  match List.last samples with
  | None -> View.text ~attrs:[ Attr.fg (color Overlay1) ] "sampling…"
  | Some latest ->
    let coordinates =
      List.map samples ~f:(fun { at; value } ->
        let seconds_ago = Time_ns.diff at latest.at |> Time_ns.Span.to_sec in
        (~x:seconds_ago, ~y:(mib value)))
    in
    (* Leave room for the y labels + axis border on the left and the title and x labels
       above/below the data area. *)
    let data_width = Int.max 8 (width - 12) in
    let data_height = Int.max 3 (height - 5) in
    (* Headroom above (and for [`Fit], below) the data keeps the line clear of the
       chart frame. [`Anchor] pins the axis at 0; [`Fit] zooms to the data window so
       small fluctuations of a large-but-stable value stay visible. *)
    let y_axis_scale_config =
      let values = List.map coordinates ~f:(fun (~x:_, ~y) -> y) in
      let lo = List.reduce_exn values ~f:Float.min in
      let hi = List.reduce_exn values ~f:Float.max in
      let min_value, max_value =
        match y_axis with
        | `Anchor -> 0., Float.max 1. (hi *. 1.05)
        | `Fit ->
          let pad = Float.max ((hi -. lo) *. 0.1) 1. in
          Float.max 0. (lo -. pad), hi +. pad
      in
      Bonsai_term_chart.Axis_scale_config.Linear
        { min_value = Constant min_value; max_value = Constant max_value }
    in
    let x_labels_config =
      Bonsai_term_chart.Scalar_axis_labels_config.Every_n_cells
        { n = 20
        ; make_label_string = (fun x -> sprintf "%.1fs" x)
        }
    in
    Line_chart.view
      ~title:"memory.current (MiB)"
      ~y_axis_scale_config
      ~x_labels_config
      [ Line_spec.create ~color:(color Sapphire) coordinates ]
      ~data_width
      ~data_height
;;

let chart_view ~y_axis ~path ~(output : Sampler.Output.t) ~(dimensions : Dimensions.t) =
  let { Dimensions.width; height } = dimensions in
  let header =
    let current =
      match List.last output.samples with
      | Some { value; _ } -> format_mib value
      | None -> "—"
    in
    let peak =
      match List.map output.samples ~f:(fun s -> s.value) with
      | [] -> "—"
      | values -> format_mib (List.reduce_exn values ~f:Float.max)
    in
    View.vcat
      [ View.hcat
          [ View.text ~attrs:[ Attr.fg (color Overlay1) ] "cgroup "
          ; View.text ~attrs:[ Attr.fg (color Subtext1) ] path
          ]
      ; View.hcat
          [ View.text ~attrs:[ Attr.fg (color Overlay1) ] "current "
          ; View.text ~attrs:[ Attr.bold; Attr.fg (color Sapphire) ] current
          ; View.text ~attrs:[ Attr.fg (color Overlay1) ] "   peak "
          ; View.text ~attrs:[ Attr.fg (color Peach) ] peak
          ; View.text ~attrs:[ Attr.fg (color Overlay0) ] "   · b browse cgroups"
          ]
      ]
  in
  let error =
    match output.last_error with
    | None -> View.none
    | Some error ->
      View.text
        ~attrs:[ Attr.fg (color Red) ]
        (sprintf "read error: %s" (Error.to_string_hum error))
  in
  View.vcat
    [ header
    ; error
    ; chart ~y_axis ~samples:output.samples ~width ~height:(Int.max 0 (height - 3))
    ]
;;

let component ~default_path ~interval ~history ~dimensions ~settings ~recording (local_ graph) =
  let model, inject =
    Bonsai.state_machine
      ~default_model:
        { Browser.Model.browsing = false
        ; cursor = 0
        ; rows = []
        ; watched = default_path
        }
      ~apply_action:Browser.apply_action
      graph
  in
  let path =
    let%arr model in
    model.Browser.Model.watched
  in
  let output = Sampler.poll_file ~interval ~history ~path ~parse graph in
  let window = Time_ns.Span.scale_int interval history in
  let%arr model and output and dimensions and settings and inject and recording in
  (* Recording exports absolute time (unix epoch seconds) rather than the chart's
     relative seconds-ago, and raw bytes rather than MiB. *)
  let record =
    Some
      { Vdb.Plugin.Record_source.x_axis = "time_epoch_s"
      ; y_axis = "memory_current_bytes"
      ; latest =
          List.last output.samples
          |> Option.map ~f:(fun (s : Sampler.Sample.t) ->
            Time_ns.to_span_since_epoch s.at |> Time_ns.Span.to_sec, s.value)
      }
  in
  if model.browsing
  then
    Vdb.Plugin.Output.create
      ~view:(Browser.view ~locked:recording ~model ~dimensions)
      ~handler:(Browser.handler ~locked:recording ~inject)
      ?record
      ()
  else (
    (* When the user switches back to a cgroup they watched earlier, samples from the
       previous visit may still be in its window; drop anything older than the window's
       duration so the x axis doesn't stretch across the gap. *)
    let output =
      match List.last output.samples with
      | None -> output
      | Some latest ->
        { output with
          samples =
            List.filter output.samples ~f:(fun s ->
              Time_ns.Span.( <= ) (Time_ns.diff latest.at s.at) window)
        }
    in
    let y_axis =
      match Vdb.Plugin.Settings.find settings y_axis_option with
      | "fit data" -> `Fit
      | _ -> `Anchor
    in
    let view = chart_view ~y_axis ~path:model.watched ~output ~dimensions in
    let handler (event : Event.t) =
      match event with
      | Key_press { key = ASCII 'b'; mods = [] } -> inject Browser.Action.Open
      | _ -> Effect.Ignore
    in
    Vdb.Plugin.Output.create ~view ~handler ?record ())
;;

let plugin
  ?(name = "cgroup-memory")
  ?cgroup
  ?(interval = Time_ns.Span.of_ms 500.)
  ?history
  ()
  =
  (* Default to keeping one minute of samples, whatever the sampling interval. *)
  let history =
    match history with
    | Some history -> history
    | None ->
      Int.max 2 (Float.iround_nearest_exn Time_ns.Span.(Time_ns.Span.of_sec 60. // interval))
  in
  (* Resolved once, at registration. *)
  let path = resolve_path ~cgroup in
  Vdb.Plugin.create
    ~name
    ~description:"live memory.current of a cgroup (v2)"
    ~options:[ y_axis_option ]
    (fun ~dimensions ~focused:_ ~settings ~recording (local_ graph) ->
      match path with
      | Error error ->
        Bonsai.return
          (Vdb.Plugin.Output.of_view
             (View.text
                ~attrs:[ Attr.fg (color Red) ]
                (sprintf "cannot resolve cgroup: %s" (Error.to_string_hum error))))
      | Ok default_path ->
        component ~default_path ~interval ~history ~dimensions ~settings ~recording graph)
;;

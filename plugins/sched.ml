open! Core
open Bonsai_term
open Bonsai.Let_syntax
module Catppuccin = Bonsai_term_catppuccin
module Sampler = Vdb.Sampler
module Theme = Vdb.Theme

let color = Theme.color

let color_option =
  Vdb.Plugin.Option_spec.create ~key:"color" [ "by state"; "by cpu"; "stacked" ]
;;

let sort_option = Vdb.Plugin.Option_spec.create ~key:"sort" [ "busiest first"; "by tid" ]

(* One timeline cell per sampling interval, so this is the display resolution. The
   procfs scan is ~2 tiny reads per thread; even 50ms is cheap for tens of threads. *)
let interval_option =
  Vdb.Plugin.Option_spec.create ~key:"interval" [ "500ms"; "250ms"; "100ms"; "50ms" ]
;;

let interval_of_settings settings =
  Time_ns.Span.of_string (Vdb.Plugin.Settings.find settings interval_option)
;;

(* One thread's scheduler counters at one instant. [run_ns] and [wait_ns] are
   cumulative since thread start (from schedstat), so state over an interval comes from
   deltas between consecutive snapshots. *)
module Thread_stat = struct
  type t =
    { comm : string
    ; state : char (** R running/runnable, S sleeping, D uninterruptible, ... *)
    ; cpu : int (** The CPU the thread last ran on. *)
    ; run_ns : float (** Cumulative time on CPU. *)
    ; wait_ns : float (** Cumulative time runnable but waiting on a runqueue. *)
    }
  [@@deriving sexp_of, equal]
end

module Snapshot = struct
  type t = Thread_stat.t Map.M(Int).t [@@deriving sexp_of, equal]
end

(* /proc/<pid>/task/<tid>/stat is "<tid> (<comm>) <state> <ppid> ...". [comm] may itself
   contain spaces and parens, so the space-separated fields resume after the *last* ')':
   the first one is state (field 3 of the file) and index 36 is the processor the thread
   last ran on (field 39). *)
let parse_stat contents =
  let open_paren = String.index_exn contents '(' in
  let close_paren = String.rindex_exn contents ')' in
  let comm =
    String.sub contents ~pos:(open_paren + 1) ~len:(close_paren - open_paren - 1)
  in
  let fields =
    String.sub
      contents
      ~pos:(close_paren + 1)
      ~len:(String.length contents - close_paren - 1)
    |> String.strip
    |> String.split ~on:' '
  in
  let state = (List.hd_exn fields).[0] in
  let cpu = Int.of_string (List.nth_exn fields 36) in
  comm, state, cpu
;;

(* /proc/<pid>/task/<tid>/schedstat: "<on-cpu ns> <runqueue-wait ns> <timeslices>". *)
let parse_schedstat contents =
  match String.strip contents |> String.split ~on:' ' with
  | run :: wait :: _ -> Float.of_string run, Float.of_string wait
  | _ -> failwith "malformed schedstat"
;;

let scan pid =
  let task_dir = sprintf "/proc/%d/task" pid in
  Stdlib.Sys.readdir task_dir
  |> Array.to_list
  |> List.filter_map ~f:(fun entry ->
    match Int.of_string_opt entry with
    | None -> None
    | Some tid ->
      (* A thread can exit between the readdir and these reads; skip it. *)
      (match
         let dir = task_dir ^/ entry in
         let comm, state, cpu = parse_stat (In_channel.read_all (dir ^/ "stat")) in
         let run_ns, wait_ns = parse_schedstat (In_channel.read_all (dir ^/ "schedstat")) in
         { Thread_stat.comm; state; cpu; run_ns; wait_ns }
       with
       | stat -> Some (tid, stat)
       | exception _ -> None))
  |> Map.of_alist_exn (module Int)
;;

(* All the per-thread procfs reads for one sample, done in one blocking pass off the
   Async scheduler. The sampler key is "<pid>@<interval>" (the cadence is part of the
   key so each interval keeps its own window); only the pid part matters here. *)
let read key =
  let open Async in
  let pid_str =
    match String.lsplit2 key ~on:'@' with
    | Some (pid, _) -> pid
    | None -> key
  in
  match Int.of_string_opt (String.strip pid_str) with
  | None -> Deferred.return (Or_error.error_string (sprintf "not a pid: %S" pid_str))
  | Some pid ->
    In_thread.run (fun () ->
      Or_error.try_with (fun () -> scan pid)
      |> Or_error.tag ~tag:(sprintf "reading /proc/%d/task" pid))
;;

(* What the scheduler did with a thread during one sampling interval — one timeline
   cell. The schedstat deltas give the exact shares, so cells keep both: the paint mode
   decides whether to render the dominant one (block height / shade) or stack the two
   in one glyph (green run block on a red wait background). *)
module Cell = struct
  type t =
    | Busy of
        { run : float (** Fraction of the interval spent on CPU. *)
        ; wait : float
          (** Fraction spent runnable but waiting for a CPU: preempted or queued. *)
        ; cpu : int (** The CPU the thread last ran on. *)
        }
    | Io (** Uninterruptible sleep, usually blocked on I/O. *)
    | Sleep
    | Gone (** The thread did not exist. *)
end

let classify ~prev ~(cur : Thread_stat.t) ~dt_ns : Cell.t =
  (* Without a previous snapshot to delta against (first sighting, or the tid was reused
     and the counters restarted), fall back to the sampled state character. *)
  let of_state () =
    match cur.state with
    | 'R' -> Cell.Busy { run = 1.; wait = 0.; cpu = cur.cpu }
    | 'D' -> Io
    | _ -> Sleep
  in
  match prev with
  | None -> of_state ()
  | Some (prev : Thread_stat.t) ->
    if Float.(dt_ns <= 0.)
    then of_state ()
    else (
      let run = (cur.run_ns -. prev.run_ns) /. dt_ns in
      let wait = (cur.wait_ns -. prev.wait_ns) /. dt_ns in
      if Float.(run < 0. || wait < 0.)
      then of_state ()
      else if Float.(run +. wait < 0.05)
      then (
        match cur.state with
        | 'D' -> Io
        | _ -> Sleep)
      else Busy { run = Float.min 1. run; wait = Float.min 1. wait; cpu = cur.cpu })
;;

module Row = struct
  type t =
    { tid : int
    ; comm : string
    ; cells : Cell.t array (** Oldest first; all rows have the same length. *)
    ; busy_ns : float (** Run + wait time over the window, for busiest-first sorting. *)
    }
end

(* Turn the last [max_cells + 1] snapshots into per-thread rows of [max_cells] cells
   (fewer while the window is still filling). *)
let build_rows ~(samples : Snapshot.t Sampler.Sample.t list) ~max_cells =
  let samples = List.drop samples (Int.max 0 (List.length samples - (max_cells + 1))) in
  match samples with
  | [] | [ _ ] -> []
  | first :: rest ->
    let pairs =
      List.folding_map rest ~init:first ~f:(fun prev cur ->
        let dt_ns = Time_ns.diff cur.Sampler.Sample.at prev.at |> Time_ns.Span.to_ns in
        cur, (dt_ns, prev.value, cur.value))
    in
    let n = List.length pairs in
    let tids =
      List.fold
        pairs
        ~init:(Set.empty (module Int))
        ~f:(fun acc (_, _, cur) -> Set.union acc (Map.key_set cur))
    in
    Set.to_list tids
    |> List.map ~f:(fun tid ->
      let cells = Array.create ~len:n Cell.Gone in
      let comm = ref "" in
      let busy_ns = ref 0. in
      List.iteri pairs ~f:(fun i (dt_ns, prev, cur) ->
        match Map.find cur tid with
        | None -> ()
        | Some cur_t ->
          comm := cur_t.Thread_stat.comm;
          let prev_t = Map.find prev tid in
          Option.iter prev_t ~f:(fun (p : Thread_stat.t) ->
            busy_ns
            := !busy_ns
               +. Float.max 0. (cur_t.run_ns -. p.run_ns)
               +. Float.max 0. (cur_t.wait_ns -. p.wait_ns));
          cells.(i) <- classify ~prev:prev_t ~cur:cur_t ~dt_ns);
      { Row.tid; comm = !comm; cells; busy_ns = !busy_ns })
;;

let cpu_bucket cpu = ((cpu mod 8) + 8) mod 8

(* Eight visually distinct hues; CPUs beyond eight share by wrapping. *)
let cpu_colors : Catppuccin.t array = [| Blue; Green; Yellow; Mauve; Sapphire; Peach; Pink; Teal |]

(* Run cells grow with the on-CPU share; wait cells keep their shaded texture (so
   run vs wait never hinges on green-vs-red alone) and darken with the wait share. *)
let run_ramp = [| "▁"; "▂"; "▃"; "▄"; "▅"; "▆"; "▇"; "█" |]

let run_level share =
  Int.clamp_exn (Float.to_int (Float.round_up (share *. 8.)) - 1) ~min:0 ~max:7
;;

let wait_ramp = [| "░"; "▒"; "▓" |]

let wait_level share =
  Int.clamp_exn (Float.to_int (Float.round_up (share *. 3.)) - 1) ~min:0 ~max:2
;;

(* In the stacked mode, cells with less than half a ramp step of run render as pure
   background so a fully-starved thread reads as a solid red cell, not a green sliver. *)
let stacked_level run = if Float.(run < 1. /. 16.) then 0 else 1 + run_level run

let cell_paint ~mode (cell : Cell.t) =
  match cell with
  | Busy { run; wait; cpu } ->
    (match mode with
     | `Stacked ->
       (* One glyph as a stacked bar: green block from the bottom for the run share,
          red background above it for the wait. Exact when the thread didn't also
          sleep within the interval (then the remainder overstates the wait). *)
       let glyph = if stacked_level run = 0 then " " else run_ramp.(run_level run) in
       let bg = if Float.(wait >= 0.05) then [ Attr.bg (color Red) ] else [] in
       glyph, (Attr.fg (color Green) :: bg)
     | (`State | `Cpu) as mode ->
       if Float.(wait >= run)
       then wait_ramp.(wait_level wait), [ Attr.fg (color Red) ]
       else (
         let fg =
           match mode with
           | `State -> color Green
           | `Cpu -> color cpu_colors.(cpu_bucket cpu)
         in
         run_ramp.(run_level run), [ Attr.fg fg ]))
  | Io -> "~", [ Attr.fg (color Mauve) ]
  | Sleep -> "·", [ Attr.fg (color Overlay0) ]
  | Gone -> " ", []
;;

(* Identifies a cell's glyph+color so runs of identical cells collapse into one
   [View.text]. *)
let paint_key ~mode (cell : Cell.t) =
  match cell with
  | Busy { run; wait; cpu } ->
    (match mode with
     | `Stacked -> 200 + (stacked_level run * 2) + Bool.to_int Float.(wait >= 0.05)
     | `State | `Cpu ->
       if Float.(wait >= run)
       then 100 + wait_level wait
       else (
         let hue =
           match mode with
           | `Cpu -> cpu_bucket cpu
           | `State | `Stacked -> 8
         in
         (hue * 8) + run_level run))
  | Io -> 110
  | Sleep -> 111
  | Gone -> 112
;;

let gutter_label ~width ~(row : Row.t) =
  let tid = Int.to_string row.tid in
  let comm = String.prefix row.comm (Int.max 1 (width - String.length tid - 1)) in
  let label = comm ^ " " ^ tid in
  if String.length label > width
  then String.suffix label width (* keep the tid visible however narrow the gutter *)
  else sprintf "%-*s" width label
;;

let row_view ~mode ~gutter ~pad (row : Row.t) =
  let chunks =
    Array.to_list row.cells
    |> List.group ~break:(fun a b -> paint_key ~mode a <> paint_key ~mode b)
    |> List.map ~f:(fun chunk ->
      let glyph, attrs = cell_paint ~mode (List.hd_exn chunk) in
      View.text ~attrs (String.concat (List.map chunk ~f:(fun (_ : Cell.t) -> glyph))))
  in
  View.hcat
    (View.text ~attrs:[ Attr.fg (color Subtext0) ] (gutter_label ~width:gutter ~row)
     :: View.text ~attrs:[ Attr.fg (color Surface2) ] "│"
     :: View.text (String.make pad ' ')
     :: chunks)
;;

(* A thin rule between adjacent lanes, joining the gutter's vertical line. *)
let lane_separator ~gutter ~cells_width =
  View.text
    ~attrs:[ Attr.fg (color Surface1) ]
    (String.make gutter ' '
     ^ "├"
     ^ String.concat (List.init cells_width ~f:(fun (_ : int) -> "┄")))
;;

(* The time axis under the cells: "now" pinned at the right edge, a label every 20
   cells. *)
let axis_view ~gutter ~cells_width ~dt_s =
  let chars = Array.create ~len:cells_width "─" in
  let rec place cols_from_right prev_label_start =
    if cols_from_right < cells_width
    then (
      let text =
        if cols_from_right = 0
        then "now"
        else sprintf "-%.0fs" (Float.of_int cols_from_right *. dt_s)
      in
      let end_at = cells_width - 1 - cols_from_right in
      let start = end_at - String.length text + 1 in
      if start >= 0 && end_at < prev_label_start - 1
      then (
        String.iteri text ~f:(fun i c -> chars.(start + i) <- String.of_char c);
        place (cols_from_right + 20) start)
      else place (cols_from_right + 20) prev_label_start)
  in
  place 0 (cells_width + 1);
  View.text
    ~attrs:[ Attr.fg (color Overlay0) ]
    (String.make gutter ' ' ^ "└" ^ String.concat (Array.to_list chars))
;;

let legend_view ~mode =
  let entry attrs glyph label =
    [ View.text ~attrs (glyph ^ " "); View.text ~attrs:[ Attr.fg (color Overlay1) ] (label ^ "  ") ]
  in
  let run_and_wait =
    match mode with
    | `State -> entry [ Attr.fg (color Green) ] "▁▄█" "run"
                @ entry [ Attr.fg (color Red) ] "░▒▓" "wait for cpu"
    | `Cpu ->
      entry [ Attr.fg (color Green) ] "▁▄█" "run (hue = cpu)"
      @ entry [ Attr.fg (color Red) ] "░▒▓" "wait for cpu"
    | `Stacked ->
      entry [ Attr.fg (color Green); Attr.bg (color Red) ] "▄" "run over wait for cpu"
  in
  View.hcat
    (List.concat
       [ run_and_wait
       ; entry [ Attr.fg (color Mauve) ] "~" "io"
       ; entry [ Attr.fg (color Overlay0) ] "·" "sleep"
       ])
;;

module Model = struct
  type t =
    { pid : string (** The watched pid, as typed. *)
    ; entering : string option (** [Some typed] while the pid prompt is open. *)
    ; scroll : int
    }
end

module Action = struct
  type t =
    | Start_entry
    | Entry_char of char
    | Entry_backspace
    | Entry_cancel
    | Entry_commit
    | Scroll_to of int
  [@@deriving sexp_of]
end

let apply_action _ctx (model : Model.t) (action : Action.t) =
  match action with
  | Start_entry -> { model with entering = Some "" }
  | Entry_char c ->
    (match model.entering with
     | Some typed when Char.is_digit c ->
       { model with entering = Some (typed ^ String.of_char c) }
     | Some _ | None -> model)
  | Entry_backspace ->
    (match model.entering with
     | Some typed -> { model with entering = Some (String.drop_suffix typed 1) }
     | None -> model)
  | Entry_cancel -> { model with entering = None }
  | Entry_commit ->
    (match model.entering with
     | Some typed when not (String.is_empty typed) ->
       { pid = typed; entering = None; scroll = 0 }
     | Some _ -> { model with entering = None }
     | None -> model)
  | Scroll_to offset -> { model with scroll = Int.max 0 offset }
;;

let header_view ~(model : Model.t) ~main_comm ~n_threads ~cell ~scroll_info ~locked =
  match model.entering with
  | Some typed ->
    View.hcat
      [ View.text ~attrs:[ Attr.bold; Attr.fg (color Peach) ] (sprintf "watch pid: %s▏" typed)
      ; View.text
          ~attrs:[ Attr.fg (color Overlay0) ]
          (if locked
           then "  🔒 recording — pid switching locked"
           else "  · Enter watch · Esc cancel")
      ]
  | None ->
    View.hcat
      [ View.text ~attrs:[ Attr.fg (color Overlay1) ] "pid "
      ; View.text ~attrs:[ Attr.bold; Attr.fg (color Sapphire) ] model.pid
      ; View.text ~attrs:[ Attr.fg (color Subtext1) ] (" " ^ main_comm)
      ; View.text
          ~attrs:[ Attr.fg (color Overlay1) ]
          (sprintf " · %d threads · cell %s%s" n_threads cell scroll_info)
      ; View.text ~attrs:[ Attr.fg (color Overlay0) ] " · p change pid"
      ]
;;

let record_columns = [ "time_epoch_s"; "tid"; "comm"; "cpu"; "run_share"; "wait_share" ]

(* One CSV row per live thread for the newest sample: which fraction of the interval it
   spent on CPU vs waiting for one. *)
let record_rows (samples : Snapshot.t Sampler.Sample.t list) =
  match List.rev samples with
  | cur :: prev :: _ ->
    let dt_ns = Time_ns.diff cur.at prev.at |> Time_ns.Span.to_ns in
    if Float.(dt_ns <= 0.)
    then []
    else (
      let t = Time_ns.to_span_since_epoch cur.at |> Time_ns.Span.to_sec in
      Map.fold_right cur.value ~init:[] ~f:(fun ~key:tid ~data:(cur_t : Thread_stat.t) acc ->
        match Map.find prev.value tid with
        | None -> acc
        | Some prev_t ->
          let share cur_ns prev_ns =
            Float.clamp_exn ((cur_ns -. prev_ns) /. dt_ns) ~min:0. ~max:1.
          in
          [ Vdb.Plugin.Record_source.cell t
          ; Int.to_string tid
          ; cur_t.comm
          ; Int.to_string cur_t.cpu
          ; sprintf "%.4f" (share cur_t.run_ns prev_t.run_ns)
          ; sprintf "%.4f" (share cur_t.wait_ns prev_t.wait_ns)
          ]
          :: acc))
  | [] | [ _ ] -> []
;;

let component ~default_pid ~history ~dimensions ~settings ~recording (local_ graph) =
  let model, inject =
    Bonsai.state_machine
      ~default_model:{ Model.pid = Int.to_string default_pid; entering = None; scroll = 0 }
      ~apply_action
      graph
  in
  let interval =
    let%arr settings in
    interval_of_settings settings
  in
  let key =
    (* The cadence is part of the sampler key so each interval keeps its own window:
       cells of different time-widths must not mix in one timeline. *)
    let%arr model and settings in
    model.Model.pid ^ "@" ^ Vdb.Plugin.Settings.find settings interval_option
  in
  let output = Sampler.poll ~interval ~history ~key ~read graph in
  let%arr model and output and dimensions and settings and recording and inject and interval in
  let window = Time_ns.Span.scale_int interval history in
  (* When the user switches back to a pid they watched earlier, samples from the previous
     visit may still be in its window; drop anything older than the window's duration so
     the timeline doesn't stretch across the gap. *)
  let output =
    match List.last output.Sampler.Output.samples with
    | None -> output
    | Some latest ->
      { output with
        samples =
          List.filter output.samples ~f:(fun s ->
            Time_ns.Span.( <= ) (Time_ns.diff latest.at s.at) window)
      }
  in
  let mode =
    match Vdb.Plugin.Settings.find settings color_option with
    | "by cpu" -> `Cpu
    | "stacked" -> `Stacked
    | _ -> `State
  in
  let { Dimensions.width; height } = dimensions in
  let rows =
    let rows = build_rows ~samples:output.samples ~max_cells:(Int.max 4 (width - 7)) in
    match Vdb.Plugin.Settings.find settings sort_option with
    | "by tid" -> List.sort rows ~compare:(fun a b -> Int.compare a.Row.tid b.tid)
    | _ ->
      List.sort rows ~compare:(fun a b ->
        match Float.compare b.Row.busy_ns a.busy_ns with
        | 0 -> Int.compare a.tid b.tid
        | c -> c)
  in
  let gutter =
    let widest =
      List.fold rows ~init:0 ~f:(fun acc (r : Row.t) ->
        Int.max acc (String.length r.comm + 1 + String.length (Int.to_string r.tid)))
    in
    Int.clamp_exn widest ~min:6 ~max:(Int.max 6 (Int.min 22 (width / 3)))
  in
  let cells_width = Int.max 4 (width - gutter - 1) in
  (* [build_rows] was given the pane's worth of cells before the gutter width was known;
     re-cap to what actually fits. All rows share one cell count. *)
  let n_cells =
    match rows with
    | [] -> 0
    | row :: _ -> Int.min cells_width (Array.length row.cells)
  in
  let rows =
    List.map rows ~f:(fun row ->
      { row with
        cells = Array.sub row.cells ~pos:(Array.length row.cells - n_cells) ~len:n_cells
      })
  in
  let error_line =
    match output.last_error with
    | None -> None
    | Some error ->
      Some
        (View.text
           ~attrs:[ Attr.fg (color Red) ]
           (sprintf "read error: %s" (Error.to_string_hum error)))
  in
  let visible =
    (* Header, legend, and axis take 3 rows; [k] lanes plus the rules between them
       take [2k - 1]. *)
    let available = height - 3 - (if Option.is_some error_line then 1 else 0) in
    Int.max 1 ((available + 1) / 2)
  in
  let max_scroll = Int.max 0 (List.length rows - visible) in
  let offset = Int.min model.scroll max_scroll in
  let scroll_info =
    if List.length rows > visible
    then
      sprintf
        " (%d–%d of %d · ↑↓ scroll)"
        (offset + 1)
        (Int.min (List.length rows) (offset + visible))
        (List.length rows)
    else ""
  in
  let main_comm =
    let open Option.Let_syntax in
    (let%bind latest = List.last output.samples in
     let%bind pid = Int.of_string_opt model.pid in
     let%map (main : Thread_stat.t) = Map.find latest.value pid in
     main.comm)
    |> Option.value ~default:""
  in
  let body =
    match rows with
    | [] -> [ View.text ~attrs:[ Attr.fg (color Overlay1) ] "sampling…" ]
    | _ :: _ ->
      let pad = cells_width - n_cells in
      (List.sub rows ~pos:offset ~len:(Int.min visible (List.length rows - offset))
       |> List.map ~f:(row_view ~mode ~gutter ~pad)
       |> List.intersperse ~sep:(lane_separator ~gutter ~cells_width))
      @ [ axis_view ~gutter ~cells_width ~dt_s:(Time_ns.Span.to_sec interval) ]
  in
  let view =
    View.vcat
      (header_view
         ~model
         ~main_comm
         ~n_threads:(List.length rows)
         ~cell:(Vdb.Plugin.Settings.find settings interval_option)
         ~scroll_info
         ~locked:recording
       :: legend_view ~mode
       :: (Option.to_list error_line @ body))
  in
  let entering = Option.is_some model.Model.entering in
  let handler (event : Event.t) =
    let scroll delta =
      inject (Action.Scroll_to (Int.clamp_exn (offset + delta) ~min:0 ~max:max_scroll))
    in
    match event with
    | Key_press { key = ASCII 'p'; mods = [] } ->
      inject (if entering then Action.Entry_cancel else Action.Start_entry)
    | Key_press { key = Enter; _ } when entering ->
      (* Retargeting the pane mid-recording would silently change what the recording
         measures. *)
      if recording then Effect.Ignore else inject Action.Entry_commit
    | Key_press { key = Escape; _ } when entering -> inject Action.Entry_cancel
    | Key_press { key = Backspace; _ } when entering -> inject Action.Entry_backspace
    | Key_press { key = ASCII c; mods = [] | [ Shift ] } when entering && Char.is_digit c
      -> inject (Action.Entry_char c)
    | Key_press { key = Arrow `Up; _ } | Mouse { kind = Scroll `Up; _ } -> scroll (-1)
    | Key_press { key = Arrow `Down; _ } | Mouse { kind = Scroll `Down; _ } -> scroll 1
    | _ -> Effect.Ignore
  in
  let record =
    Some
      { Vdb.Plugin.Record_source.columns = record_columns
      ; rows = record_rows output.samples
      }
  in
  Vdb.Plugin.Output.create ~view ~handler ?record ()
;;

let self_pid () =
  In_channel.read_all "/proc/self/stat" |> String.split ~on:' ' |> List.hd_exn |> Int.of_string
;;

let plugin ?(name = "sched") ?pid ?(history = 600) () =
  (* Resolved once, at registration. *)
  let pid =
    match pid with
    | Some pid -> Ok pid
    | None -> Or_error.try_with self_pid
  in
  Vdb.Plugin.create
    ~name
    ~description:"per-thread scheduling timeline: on cpu vs waiting for one"
    ~options:[ interval_option; color_option; sort_option ]
    (fun ~dimensions ~focused:_ ~settings ~recording (local_ graph) ->
      match pid with
      | Error error ->
        Bonsai.return
          (Vdb.Plugin.Output.of_view
             (View.text
                ~attrs:[ Attr.fg (color Red) ]
                (sprintf "cannot resolve own pid: %s" (Error.to_string_hum error))))
      | Ok default_pid ->
        component ~default_pid ~history ~dimensions ~settings ~recording graph)
;;

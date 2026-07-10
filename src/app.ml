open! Core
open Bonsai_term
open Bonsai.Let_syntax
module Catppuccin = Bonsai_term_catppuccin
module Border_box = Bonsai_term_border_box

let color = Catppuccin.color ~flavor:Catppuccin.Mocha.flavor

module Content = struct
  type t =
    | Picker
    | Plugin of string
  [@@deriving sexp, equal]
end

module Model = struct
  type t =
    { tree : Pane_tree.t option
    ; focus : Pane_tree.Id.t option
    ; panes : Content.t Map.M(Pane_tree.Id).t
    ; settings : Plugin.Settings.t Map.M(Pane_tree.Id).t
    ; next_id : Pane_tree.Id.t
    ; options_open : bool
    ; options_cursor : int
    ; recorder : Recorder.State.t
    ; notice : string option (** One-line feedback shown in the status bar. *)
    }
  [@@deriving sexp_of, equal]

  let initial ~content =
    let id = Pane_tree.Id.zero in
    { tree = Some (Pane_tree.leaf id)
    ; focus = Some id
    ; panes = Map.singleton (module Pane_tree.Id) id content
    ; settings = Map.empty (module Pane_tree.Id)
    ; next_id = Pane_tree.Id.succ id
    ; options_open = false
    ; options_cursor = 0
    ; recorder = Recorder.State.Idle
    ; notice = None
    }
  ;;
end

module Action = struct
  type t =
    | Split of Pane_tree.Split_dir.t
    | Close
    | Focus_dir of Pane_tree.Direction.t * Dimensions.t
    | Move_dir of Pane_tree.Direction.t * Dimensions.t
    | Resize of Pane_tree.Split_dir.t * float
    | Set_plugin of Pane_tree.Id.t * string
    | Focus_at of int * int * Dimensions.t
    | Toggle_options
    | Options_nav of int
    | Options_cycle of int
    | Record_start of
        { pane : Pane_tree.Id.t
        ; plugin_name : string
        ; columns : string list
        }
    | Record_append of string list list
    | Record_stop
    | Prompt_input of char
    | Prompt_backspace
    | Prompt_cancel
    | Prompt_save
    | Save_result of (string, string) Result.t
  [@@deriving sexp_of]
end

(* The bottom row is reserved for the status bar. *)
let pane_area (dimensions : Dimensions.t) =
  { Region.x = 0
  ; y = 0
  ; width = dimensions.width
  ; height = Int.max 0 (dimensions.height - 1)
  }
;;

let find_plugin registry name =
  List.find registry ~f:(fun plugin -> String.equal (Plugin.name plugin) name)
;;

(* The focused pane's plugin and option specs, if it has any options to configure. *)
let focused_options ~registry (model : Model.t) =
  let open Option.Let_syntax in
  let%bind id = model.focus in
  let%bind content = Map.find model.panes id in
  match content with
  | Content.Picker -> None
  | Content.Plugin name ->
    let%bind plugin = find_plugin registry name in
    (match Plugin.options plugin with
     | [] -> None
     | specs -> Some (id, plugin, specs))
;;

let pane_settings (model : Model.t) id =
  Map.find model.settings id |> Option.value ~default:Plugin.Settings.empty
;;

let apply_action ~registry ctx (model : Model.t) (action : Action.t) =
  let layout_for dimensions tree = Pane_tree.layout tree ~region:(pane_area dimensions) in
  (* Any structural change dismisses the options window. Options and recorder actions
     (notably the per-sample [Record_append]) must leave it alone. *)
  let model =
    match action with
    | Toggle_options | Options_nav _ | Options_cycle _
    | Record_start _ | Record_append _ | Record_stop
    | Prompt_input _ | Prompt_backspace | Prompt_cancel | Prompt_save | Save_result _ ->
      model
    | _ -> { model with options_open = false; options_cursor = 0 }
  in
  match action with
  | Split dir ->
    let new_id = model.next_id in
    (match model.tree, model.focus with
     | Some tree, Some focus ->
       { model with
         tree = Some (Pane_tree.split tree ~at:focus ~dir ~new_id)
       ; focus = Some new_id
       ; panes = Map.set model.panes ~key:new_id ~data:Content.Picker
       ; next_id = Pane_tree.Id.succ new_id
       }
     | _ ->
       (* No panes on screen: create the first one. *)
       { model with
         tree = Some (Pane_tree.leaf new_id)
       ; focus = Some new_id
       ; panes = Map.singleton (module Pane_tree.Id) new_id Content.Picker
       ; next_id = Pane_tree.Id.succ new_id
       })
  | Close ->
    (match model.tree, model.focus with
     | Some tree, Some focus ->
       let panes = Map.remove model.panes focus in
       let settings = Map.remove model.settings focus in
       (match Pane_tree.close tree ~id:focus with
        | `Empty -> { model with tree = None; focus = None; panes; settings }
        | `Closed (tree, new_focus) ->
          { model with tree = Some tree; focus = Some new_focus; panes; settings }
        | `Not_found -> model)
     | _ -> model)
  | Focus_dir (direction, dimensions) ->
    (match model.tree, model.focus with
     | Some tree, Some focus ->
       (match Pane_tree.neighbor (layout_for dimensions tree) ~of_:focus ~direction with
        | Some id -> { model with focus = Some id }
        | None -> model)
     | _ -> model)
  | Move_dir (direction, dimensions) ->
    (match model.tree, model.focus with
     | Some tree, Some focus ->
       (match Pane_tree.neighbor (layout_for dimensions tree) ~of_:focus ~direction with
        | Some other -> { model with tree = Some (Pane_tree.swap tree ~a:focus ~b:other) }
        | None -> model)
     | _ -> model)
  | Resize (dir, by) ->
    (match model.tree, model.focus with
     | Some tree, Some focus ->
       { model with tree = Some (Pane_tree.resize tree ~id:focus ~dir ~by) }
     | _ -> model)
  | Set_plugin (id, name) ->
    if Map.mem model.panes id
    then
      (* Fresh plugin instance: drop any settings from a previous occupant. *)
      { model with
        panes = Map.set model.panes ~key:id ~data:(Content.Plugin name)
      ; settings = Map.remove model.settings id
      }
    else model
  | Focus_at (x, y, dimensions) ->
    (match model.tree with
     | Some tree ->
       (match Pane_tree.find_at (layout_for dimensions tree) ~x ~y with
        | Some id -> { model with focus = Some id }
        | None -> model)
     | None -> model)
  | Toggle_options ->
    if model.options_open
    then { model with options_open = false; options_cursor = 0 }
    else (
      match focused_options ~registry model with
      | Some _ -> { model with options_open = true; options_cursor = 0 }
      | None -> model)
  | Options_nav delta ->
    (match focused_options ~registry model with
     | Some (_, _, specs) when model.options_open ->
       let n = List.length specs in
       { model with options_cursor = ((model.options_cursor + delta) mod n + n) mod n }
     | _ -> model)
  | Options_cycle delta ->
    (match focused_options ~registry model with
     | Some (id, _, specs) when model.options_open ->
       (match List.nth specs model.options_cursor with
        | None -> model
        | Some spec ->
          let settings_for_pane = pane_settings model id in
          let current = Plugin.Settings.find settings_for_pane spec in
          let n = List.length spec.choices in
          let index =
            List.findi spec.choices ~f:(fun _ choice -> String.equal choice current)
            |> Option.value_map ~default:0 ~f:fst
          in
          let next = List.nth_exn spec.choices (((index + delta) mod n + n) mod n) in
          { model with
            settings =
              Map.set
                model.settings
                ~key:id
                ~data:(Map.set settings_for_pane ~key:spec.key ~data:next)
          })
     | _ -> model)
  | Record_start { pane; plugin_name; columns } ->
    (match model.recorder with
     | Idle ->
       { model with
         recorder = Recording { pane; plugin_name; columns; rows = [] }
       ; notice = None
       }
     | Recording _ | Prompting _ -> model)
  | Record_append batch ->
    (match model.recorder with
     | Recording r ->
       (* [batch] is oldest-first within itself; the buffer is newest-first. *)
       { model with recorder = Recording { r with rows = List.rev batch @ r.rows } }
     | Idle | Prompting _ -> model)
  | Record_stop ->
    (match model.recorder with
     | Recording { pane = _; plugin_name; columns; rows } ->
       { model with recorder = Prompting { plugin_name; columns; rows; filename = "" } }
     | Idle | Prompting _ -> model)
  | Prompt_input c ->
    (match model.recorder with
     | Prompting p ->
       { model with recorder = Prompting { p with filename = p.filename ^ String.of_char c } }
     | Idle | Recording _ -> model)
  | Prompt_backspace ->
    (match model.recorder with
     | Prompting p ->
       { model with recorder = Prompting { p with filename = String.drop_suffix p.filename 1 } }
     | Idle | Recording _ -> model)
  | Prompt_cancel ->
    (match model.recorder with
     | Prompting _ -> { model with recorder = Idle; notice = Some "recording discarded" }
     | Idle | Recording _ -> model)
  | Prompt_save ->
    (match model.recorder with
     | Prompting { plugin_name; columns; rows; filename } ->
       let save_and_report =
         let%bind.Effect result =
           Recorder.save ~input:filename ~plugin_name ~columns ~rows:(List.rev rows)
         in
         Bonsai.Apply_action_context.inject ctx (Action.Save_result result)
       in
       Bonsai.Apply_action_context.schedule_event ctx save_and_report;
       { model with recorder = Idle; notice = Some "saving…" }
     | Idle | Recording _ -> model)
  | Save_result (Ok path) -> { model with notice = Some (sprintf "saved %s" path) }
  | Save_result (Error message) ->
    { model with notice = Some (sprintf "save failed: %s" message) }
;;

(* Selects a plugin component by name from the (fixed at startup) registry. Panes showing
   different plugins are different [Bonsai.enum] branches, so only the plugin a pane
   actually shows is active in it. *)
let dispatch ~registry ~name ~dimensions ~focused ~settings ~recording (local_ graph) =
  let unknown = List.length registry in
  let module Index = struct
    type t = int [@@deriving compare, equal, sexp_of]

    let all = List.init (unknown + 1) ~f:Fn.id
  end
  in
  let index =
    let%arr name in
    match
      List.findi registry ~f:(fun _ plugin -> String.equal (Plugin.name plugin) name)
    with
    | Some (i, _) -> i
    | None -> unknown
  in
  Bonsai.enum
    (module Index)
    ~match_:index
    ~with_:(fun i (local_ graph) ->
      match List.nth registry i with
      | Some plugin ->
        Plugin.instantiate plugin ~dimensions ~focused ~settings ~recording graph
      | None ->
        let%arr name in
        Plugin.Output.of_view
          (View.text
             ~attrs:[ Attr.fg (color Red) ]
             (sprintf "unknown plugin: %s" name)))
    graph
;;

let fallback_region = { Region.x = 0; y = 0; width = 20; height = 6 }

(* Crop/extend [view] so it is exactly [width] x [height]. *)
let fit view ~width ~height =
  let cropped =
    View.crop
      ~r:(Int.max 0 (View.width view - width))
      ~b:(Int.max 0 (View.height view - height))
      view
  in
  View.zcat [ cropped; View.transparent_rectangle ~width ~height ]
;;

let panes_by_id ~registry ~model ~inject ~regions (local_ graph) =
  Bonsai.assoc
    (module Pane_tree.Id)
    (let%arr model in model.Model.panes)
    ~f:(fun id content (local_ graph) ->
      let region =
        let%arr regions and id in
        Option.value (Map.find regions id) ~default:fallback_region
      in
      let focused =
        let%arr model and id in
        [%equal: Pane_tree.Id.t option] model.Model.focus (Some id)
      in
      let content_dimensions =
        let%arr region in
        { Dimensions.width = Int.max 0 (region.width - 2)
        ; height = Int.max 0 (region.height - 2)
        }
      in
      let body =
        match%sub content with
        | Content.Picker ->
          let set_plugin =
            let%arr inject and id in
            fun name -> inject (Action.Set_plugin (id, name))
          in
          let picker =
            Picker.component
              ~registry
              ~set_plugin
              ~dimensions:content_dimensions
              ~focused
              graph
          in
          let%arr picker in
          let (~view, ~handler) = picker in
          { Plugin.Output.view; handler; record = None }
        | Content.Plugin name ->
          let settings =
            let%arr model and id in
            pane_settings model id
          in
          let recording =
            let%arr model and id in
            match model.Model.recorder with
            | Recording { pane; _ } -> Pane_tree.Id.equal pane id
            | Idle | Prompting _ -> false
          in
          dispatch
            ~registry
            ~name
            ~dimensions:content_dimensions
            ~focused
            ~settings
            ~recording
            graph
      in
      let%arr body and region and focused and content in
      let { Plugin.Output.view = inner; handler; record } = body in
      let title =
        match (content : Content.t) with
        | Picker -> "open"
        | Plugin name -> name
      in
      let border_color = if focused then color Mauve else color Surface2 in
      let line_type =
        if focused then Border_box.Line_type.Thick else Border_box.Line_type.Thin
      in
      let boxed =
        Border_box.view
          ~line_type
          ~title
          ~title_attrs:
            [ Attr.bold
            ; Attr.fg (if focused then color Mauve else color Overlay1)
            ]
          ~attrs:[ Attr.fg border_color ]
          (fit
             inner
             ~width:(Int.max 0 (region.width - 2))
             ~height:(Int.max 0 (region.height - 2)))
      in
      View.pad ~l:region.x ~t:region.y boxed, handler, record)
    graph
;;

(* The options window for the focused pane, positioned over the middle of that pane. *)
let options_overlay ~registry ~model ~regions =
  let%arr model and regions in
  if not model.Model.options_open
  then View.none
  else (
    match focused_options ~registry model with
    | None -> View.none
    | Some (id, plugin, specs) ->
      let settings_for_pane = pane_settings model id in
      let rows =
        List.mapi specs ~f:(fun i (spec : Plugin.Option_spec.t) ->
          let value = Plugin.Settings.find settings_for_pane spec in
          let selected = i = model.Model.options_cursor in
          let attrs =
            if selected
            then [ Attr.bold; Attr.fg (color Text); Attr.bg (color Surface1) ]
            else [ Attr.fg (color Subtext0) ]
          in
          View.text ~attrs (sprintf " %-14s ◂ %s ▸ " spec.label value))
      in
      let hint =
        View.text
          ~attrs:[ Attr.fg (color Overlay0) ]
          " j/k select · h/l change · o close "
      in
      let box =
        Border_box.view
          ~line_type:Border_box.Line_type.Double
          ~title:(Plugin.name plugin ^ " · options")
          ~title_attrs:[ Attr.bold; Attr.fg (color Peach) ]
          ~attrs:[ Attr.fg (color Peach) ]
          (View.vcat (rows @ [ View.text ""; hint ])
           |> View.with_colors' ~fill_backdrop:true ~bg:(color Mantle))
      in
      let region = Option.value (Map.find regions id) ~default:fallback_region in
      let l = region.x + Int.max 0 ((region.width - View.width box) / 2) in
      let t = region.y + Int.max 0 ((region.height - View.height box) / 2) in
      View.pad ~l ~t box)
;;

let status_bar ~model ~dimensions =
  let%arr model and dimensions in
  let { Dimensions.width; _ } = dimensions in
  let focus_label =
    match model.Model.focus with
    | None -> "no panes — press v or s to open one"
    | Some id ->
      (match Map.find model.Model.panes id with
       | Some (Content.Plugin name) -> sprintf "[%s] %s" (Pane_tree.Id.to_string id) name
       | Some Content.Picker -> sprintf "[%s] picker" (Pane_tree.Id.to_string id)
       | None -> "")
  in
  let keys =
    match model.Model.recorder with
    | Prompting { filename; rows; _ } ->
      sprintf
        "save %d rows to: %s▏  (empty = ./vdb-<plugin>-<time>.csv · Enter save · Esc discard)"
        (List.length rows)
        filename
    | Idle | Recording _ ->
      if model.Model.options_open
      then "options: j/k select · h/l change · o/esc close"
      else
        "v/s split · hjkl focus · HJKL move · <>-+ resize · o options · r record · x \
         close · q quit"
  in
  let recording_segment =
    match model.Model.recorder with
    | Recording { rows; plugin_name; pane; _ } ->
      [ View.text
          ~attrs:[ Attr.bold; Attr.fg (color Red); Attr.bg (color Surface0) ]
          (sprintf " ● REC %d " (List.length rows))
      ; View.text
          ~attrs:[ Attr.fg (color Peach); Attr.bg (color Surface0) ]
          (sprintf "🔒[%s] %s " (Pane_tree.Id.to_string pane) plugin_name)
      ]
    | Idle | Prompting _ -> []
  in
  let notice_segment =
    match model.Model.notice with
    | None -> []
    | Some notice ->
      let notice_color =
        if String.is_prefix notice ~prefix:"saved" then color Green else color Peach
      in
      [ View.text ~attrs:[ Attr.fg notice_color; Attr.bg (color Surface0) ] (" " ^ notice ^ " ") ]
  in
  let bar =
    View.hcat
      ([ View.text ~attrs:[ Attr.bold; Attr.fg (color Crust); Attr.bg (color Mauve) ] " vdb "
       ; View.text
           ~attrs:[ Attr.fg (color Text); Attr.bg (color Surface0) ]
           (" " ^ focus_label ^ "  ")
       ]
       @ recording_segment
       @ notice_segment
       @ [ View.text ~attrs:[ Attr.fg (color Overlay1); Attr.bg (color Surface0) ] keys ])
  in
  View.zcat
    [ bar
    ; View.rectangle
        ~attrs:[ Attr.bg (color Surface0) ]
        ~width:(Int.max 0 width)
        ~height:1
        ()
    ]
;;

let component ~registry ?initial ~exit ~dimensions (local_ graph) =
  let initial_content =
    match initial with
    | Some name -> Content.Plugin name
    | None -> Content.Picker
  in
  let model, inject =
    Bonsai.state_machine
      ~default_model:(Model.initial ~content:initial_content)
      ~apply_action:(apply_action ~registry)
      graph
  in
  let regions =
    let%arr model and dimensions in
    match model.Model.tree with
    | None -> Map.empty (module Pane_tree.Id)
    | Some tree ->
      Pane_tree.layout tree ~region:(pane_area dimensions)
      |> Map.of_alist_exn (module Pane_tree.Id)
  in
  let panes = panes_by_id ~registry ~model ~inject ~regions graph in
  (* While recording, append the recorded pane's newest datapoint whenever it changes.
     The buffer lives in the app model, so it is unbounded and independent of however
     much history the plugin itself displays. *)
  let recorded_latest =
    let%arr model and panes in
    match model.Model.recorder with
    | Recording { pane; _ } ->
      (match Map.find panes pane with
       | Some (_view, _handler, Some (source : Plugin.Record_source.t)) -> source.rows
       | _ -> [])
    | Idle | Prompting _ -> []
  in
  let () =
    Bonsai.Edge.on_change
      ~equal:[%equal: string list list]
      recorded_latest
      ~callback:
        (let%arr inject in
         function
         | [] -> Effect.Ignore
         | rows -> inject (Action.Record_append rows))
      graph
  in
  let overlay = options_overlay ~registry ~model ~regions in
  let status = status_bar ~model ~dimensions in
  let view =
    let%arr panes and overlay and status and model and dimensions in
    let area = pane_area dimensions in
    let canvas =
      match model.Model.tree with
      | None ->
        View.center
          (View.vcat
             [ View.text ~attrs:[ Attr.bold; Attr.fg (color Lavender) ] "vdb"
             ; View.text
                 ~attrs:[ Attr.fg (color Overlay1) ]
                 "all panes closed — press v or s to open one"
             ])
          ~within:{ Dimensions.width = area.width; height = area.height }
      | Some _ ->
        View.zcat
          (overlay
           :: (Map.data panes |> List.map ~f:(fun (view, _handler, _record) -> view))
           @ [ View.transparent_rectangle ~width:area.width ~height:area.height ])
    in
    View.vcat [ canvas; status ]
  in
  let handler =
    let%arr panes and model and inject and dimensions in
    let forward (event : Event.t) =
      match model.Model.focus with
      | Some id ->
        (match Map.find panes id with
         | Some (_view, handler, _record) -> handler event
         | None -> Effect.Ignore)
      | None -> Effect.Ignore
    in
    let start_recording () =
      match model.Model.focus with
      | None -> Effect.Ignore
      | Some id ->
        (match Map.find panes id, Map.find model.Model.panes id with
         | Some (_, _, Some (source : Plugin.Record_source.t)), Some (Content.Plugin name)
           ->
           inject
             (Action.Record_start
                { pane = id; plugin_name = name; columns = source.columns })
         | _ -> Effect.Ignore)
    in
    fun (event : Event.t) ->
      match model.Model.recorder with
      | Prompting _ ->
        (* The save prompt is modal: it takes all keyboard input until closed. *)
        (match event with
         | Key_press { key = Enter; _ } -> inject Action.Prompt_save
         | Key_press { key = Escape; _ } -> inject Action.Prompt_cancel
         | Key_press { key = Backspace; _ } -> inject Action.Prompt_backspace
         | Key_press { key = ASCII c; mods = [] | [ Shift ] } when Char.is_print c ->
           inject (Action.Prompt_input c)
         | _ -> Effect.Ignore)
      | Idle | Recording _ ->
        if model.Model.options_open
        then (
          (* The options window is modal: it captures the keyboard until closed. *)
          match event with
          | Key_press { key; mods = [] } ->
            (match key with
             | ASCII 'j' | Arrow `Down -> inject (Action.Options_nav 1)
             | ASCII 'k' | Arrow `Up -> inject (Action.Options_nav (-1))
             | ASCII 'h' | Arrow `Left -> inject (Action.Options_cycle (-1))
             | ASCII 'l' | Arrow `Right | Enter | ASCII ' ' ->
               inject (Action.Options_cycle 1)
             | ASCII 'o' | ASCII 'q' | Escape -> inject Action.Toggle_options
             | _ -> Effect.Ignore)
          | Mouse { kind = Left; position; mods = [] } ->
            inject (Action.Focus_at (position.x, position.y, dimensions))
          | _ -> Effect.Ignore)
        else (
          match event with
          | Key_press { key; mods } ->
            (match key, mods with
             | ASCII 'q', [] -> exit ()
             | ASCII 'c', [ Ctrl ] -> exit ()
             | ASCII 'v', [] -> inject (Action.Split Vertical)
             | ASCII 's', [] -> inject (Action.Split Horizontal)
             | ASCII 'x', [] -> inject Action.Close
             | ASCII 'o', [] -> inject Action.Toggle_options
             | ASCII 'r', [] -> start_recording ()
             | Escape, []
               when (match model.Model.recorder with
                     | Recording _ -> true
                     | Idle | Prompting _ -> false) -> inject Action.Record_stop
             | ASCII 'h', [] -> inject (Action.Focus_dir (Left, dimensions))
             | ASCII 'j', [] -> inject (Action.Focus_dir (Down, dimensions))
             | ASCII 'k', [] -> inject (Action.Focus_dir (Up, dimensions))
             | ASCII 'l', [] -> inject (Action.Focus_dir (Right, dimensions))
             | ASCII 'H', _ | ASCII 'h', [ Shift ] ->
               inject (Action.Move_dir (Left, dimensions))
             | ASCII 'J', _ | ASCII 'j', [ Shift ] ->
               inject (Action.Move_dir (Down, dimensions))
             | ASCII 'K', _ | ASCII 'k', [ Shift ] ->
               inject (Action.Move_dir (Up, dimensions))
             | ASCII 'L', _ | ASCII 'l', [ Shift ] ->
               inject (Action.Move_dir (Right, dimensions))
             | ASCII '<', _ -> inject (Action.Resize (Vertical, -0.05))
             | ASCII '>', _ -> inject (Action.Resize (Vertical, 0.05))
             | ASCII '-', [] -> inject (Action.Resize (Horizontal, -0.05))
             | ASCII ('+' | '='), _ -> inject (Action.Resize (Horizontal, 0.05))
             | _ -> forward event)
          | Mouse { kind = Left; position; mods = [] } ->
            inject (Action.Focus_at (position.x, position.y, dimensions))
          | _ -> forward event)
  in
  let%arr view and handler in
  ~view, ~handler
;;

let command ~registry ?initial ~summary () =
  Async.Command.async_or_error
    ~summary
    (let%map_open.Command () = return () in
     fun () ->
       Bonsai_term.start_with_exit (fun ~exit ~dimensions (local_ graph) ->
         Bonsai_term.unstitch (component ~registry ?initial ~exit ~dimensions graph)))
;;

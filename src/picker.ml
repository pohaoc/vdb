open! Core
open Bonsai_term
open Bonsai.Let_syntax
module Catppuccin = Bonsai_term_catppuccin

let color = Catppuccin.color ~flavor:Catppuccin.Mocha.flavor

let component ~registry ~set_plugin ~dimensions:_ ~focused (local_ graph) =
  let n = List.length registry in
  (* A state machine (rather than [Bonsai.state]) so that Enter acts on the selection as
     of when it is processed, even if the selection changed in the same frame. *)
  let selected, inject =
    Bonsai.state_machine_with_input
      ~default_model:0
      ~apply_action:(fun ctx input selected action ->
        match action with
        | `Up -> if n = 0 then 0 else (selected - 1 + n) mod n
        | `Down -> if n = 0 then 0 else (selected + 1) mod n
        | `Open explicit ->
          let choice = Option.value explicit ~default:selected in
          (match input with
           | Bonsai.Computation_status.Active set_plugin ->
             (match List.nth registry choice with
              | Some plugin ->
                Bonsai.Apply_action_context.schedule_event
                  ctx
                  (set_plugin (Plugin.name plugin))
              | None -> ())
           | Inactive -> ());
          choice)
      set_plugin
      graph
  in
  let view =
    let%arr selected and focused in
    let header =
      View.text ~attrs:[ Attr.bold; Attr.fg (color Lavender) ] "Open a plugin"
    in
    let rows =
      List.mapi registry ~f:(fun i plugin ->
        let is_selected = i = selected in
        let marker = if is_selected then "❯ " else "  " in
        let attrs =
          if is_selected && focused
          then [ Attr.bold; Attr.fg (color Text); Attr.bg (color Surface1) ]
          else if is_selected
          then [ Attr.bold; Attr.fg (color Text) ]
          else [ Attr.fg (color Subtext0) ]
        in
        View.hcat
          [ View.text ~attrs (sprintf "%s%d. %s" marker (i + 1) (Plugin.name plugin))
          ; View.text
              ~attrs:[ Attr.fg (color Overlay1) ]
              ("  — " ^ Plugin.description plugin)
          ])
    in
    let footer =
      View.text
        ~attrs:[ Attr.fg (color Overlay0) ]
        "↑/↓ or digit to select · Enter to open"
    in
    match registry with
    | [] -> View.text ~attrs:[ Attr.fg (color Red) ] "No plugins registered."
    | _ :: _ -> View.vcat ([ header; View.text "" ] @ rows @ [ View.text ""; footer ])
  in
  let handler =
    let%arr inject in
    fun (event : Event.t) ->
      match event with
      | _ when n = 0 -> Effect.Ignore
      | Key_press { key = Arrow `Up; _ } -> inject `Up
      | Key_press { key = Arrow `Down; _ } -> inject `Down
      | Key_press { key = Enter; _ } -> inject (`Open None)
      | Key_press { key = ASCII c; _ } when Char.is_digit c ->
        let i = Char.to_int c - Char.to_int '1' in
        if i >= 0 && i < n then inject (`Open (Some i)) else Effect.Ignore
      | _ -> Effect.Ignore
  in
  let%arr view and handler in
  ~view, ~handler
;;

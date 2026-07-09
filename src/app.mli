open! Core
open Bonsai_term

(** The vdb application: a tiling-pane terminal UI where each pane's content is provided
    by a {!Plugin}.

    Keybindings:
    - [v] / [s]: split the focused pane vertically (side by side) / horizontally
      (stacked); the new pane opens a plugin picker
    - [h] [j] [k] [l]: move focus left / down / up / right
    - [H] [J] [K] [L]: move (swap) the focused pane left / down / up / right
    - [<] [>]: shrink / grow the focused pane's width; [-] [+]: its height
    - [x]: close the focused pane
    - [q] / [Ctrl+C]: quit
    - mouse click: focus the clicked pane

    All other events go to the focused pane's plugin handler. *)

val component
  :  registry:Plugin.t list
  -> ?initial:string (** Plugin name shown in the first pane; defaults to the picker. *)
  -> exit:(unit -> unit Effect.t)
  -> dimensions:Dimensions.t Bonsai.t
  -> local_ Bonsai.graph
  -> View.With_handler.t Bonsai.t

(** An [Async.Command.t] that runs the app in the terminal. *)
val command : registry:Plugin.t list -> ?initial:string -> summary:string -> unit -> Command.t

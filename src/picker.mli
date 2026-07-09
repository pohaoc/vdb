open! Core
open Bonsai_term

(** The built-in pane shown when a pane is first created: a menu of registered plugins.
    Arrow keys (or a digit) select, Enter opens the plugin in this pane via
    [set_plugin]. *)
val component
  :  registry:Plugin.t list
  -> set_plugin:(string -> unit Effect.t) Bonsai.t
  -> dimensions:Dimensions.t Bonsai.t
  -> focused:bool Bonsai.t
  -> local_ Bonsai.graph
  -> View.With_handler.t Bonsai.t

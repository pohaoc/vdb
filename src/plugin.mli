open! Core
open Bonsai_term

(** A plugin provides the content of a pane: a Bonsai component producing the pane's view
    and, optionally, a handler for events.

    [dimensions] is the size of the pane's content area (inside the border). It changes
    as panes are split, closed, moved and as the terminal is resized; the view is cropped
    to it if it draws outside.

    [focused] is whether the pane currently has focus. Key events that the app-level
    tiling keybindings don't consume are delivered to the focused pane's handler.

    [settings] holds the current choice for each of the plugin's declared {!Option_spec}s,
    keyed by [Option_spec.key]. A missing key means the option's default (its first
    choice). The user changes these at runtime through the options window ([o]). *)

module Option_spec : sig
  type t =
    { key : string (** Stable identifier, used to look the choice up in [settings]. *)
    ; label : string (** Shown in the options window. *)
    ; choices : string list (** Must be non-empty. The first choice is the default. *)
    }

  val create : key:string -> ?label:string (** Default: [key]. *) -> string list -> t
  val default_value : t -> string
end

module Settings : sig
  type t = string Map.M(String).t [@@deriving sexp_of, equal]

  val empty : t

  (** The chosen value for [spec], falling back to its default. *)
  val find : t -> Option_spec.t -> string
end

module Record_source : sig
  (** What a pane exposes for recording. When the user hits [r] on a pane whose plugin
      provides a record source, the app appends [latest] to a buffer every time it
      changes (so [latest] should include something that distinguishes datapoints,
      typically a timestamp x), until the user hits Escape and exports the buffer as a
      CSV whose header row is [x_axis,y_axis]. *)
  type t =
    { x_axis : string (** CSV column name for x, e.g. "time_epoch_s". *)
    ; y_axis : string (** CSV column name for y, e.g. "memory_current_bytes". *)
    ; latest : (float * float) option (** The newest datapoint, as [(x, y)]. *)
    }
  [@@deriving sexp_of, equal]
end

module Output : sig
  (** What a plugin component produces each frame. *)
  type t =
    { view : View.t
    ; handler : Event.t -> unit Effect.t
    ; record : Record_source.t option (** [None]: the pane is not recordable. *)
    }

  val of_view : ?record:Record_source.t -> View.t -> t

  val create
    :  ?record:Record_source.t
    -> view:View.t
    -> handler:(Event.t -> unit Effect.t)
    -> unit
    -> t
end

type component =
  dimensions:Dimensions.t Bonsai.t
  -> focused:bool Bonsai.t
  -> settings:Settings.t Bonsai.t
  -> recording:bool Bonsai.t
     (** True while this pane's datapoints are being recorded. Plugins that can switch
         their data source must not switch while this is set — doing so would silently
         change what the recording measures mid-file. *)
  -> local_ Bonsai.graph
  -> Output.t Bonsai.t

type t

val create
  :  name:string
  -> description:string
  -> ?options:Option_spec.t list (** Default: none (no options window). *)
  -> component
  -> t

(** For display-only plugins that don't handle input. *)
val of_view
  :  name:string
  -> description:string
  -> ?options:Option_spec.t list
  -> (dimensions:Dimensions.t Bonsai.t
      -> focused:bool Bonsai.t
      -> settings:Settings.t Bonsai.t
      -> local_ Bonsai.graph
      -> View.t Bonsai.t)
  -> t

val name : t -> string
val description : t -> string
val options : t -> Option_spec.t list
val instantiate : t -> component

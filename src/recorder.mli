open! Core
open Bonsai_term

(** Recording datapoints from a pane to a CSV file. The app owns the state: [r] starts
    recording the focused pane's {!Plugin.Record_source}, every change of the source's
    latest datapoint is appended to a buffer (unbounded — independent of the plugin's
    display window), and Escape stops and prompts for a save location. *)

module State : sig
  type t =
    | Idle
    | Recording of
        { pane : Pane_tree.Id.t
        ; plugin_name : string
        ; x_axis : string
        ; y_axis : string
        ; points : (float * float) list (** Newest first. *)
        }
    | Prompting of
        { plugin_name : string
        ; x_axis : string
        ; y_axis : string
        ; points : (float * float) list (** Newest first. *)
        ; filename : string (** What the user has typed so far. *)
        }
  [@@deriving sexp_of, equal]
end

(** Writes the CSV (header row [x_axis,y_axis], then one row per point) and returns the
    path written, or an error message.

    [input] is the user-typed location, interpreted relative to the process's working
    directory: empty → a default-named file ("vdb-<plugin>-<date>-<time>.csv") in the
    cwd; an existing directory or anything ending in '/' → the default-named file inside
    it; anything else → used as the file path as-is. *)
val save
  :  input:string
  -> plugin_name:string
  -> x_axis:string
  -> y_axis:string
  -> points:(float * float) list (** Oldest first. *)
  -> (string, string) Result.t Effect.t

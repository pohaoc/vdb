open! Core
open Bonsai_term

(** Polls a data source on an interval and keeps a rolling window of samples. This is the
    basic building block for telemetry plugins that read their data from the filesystem
    (procfs, sysfs, cgroupfs, an application's own stats file, ...). The sample type is
    whatever the plugin's reader produces — a float for single-scalar sources, a record
    or map for sources that measure several things at once. *)

module Sample : sig
  type 'a t =
    { at : Time_ns.t
    ; value : 'a
    }
  [@@deriving sexp_of, equal]
end

module Output : sig
  type 'a t =
    { samples : 'a Sample.t list (** Oldest first. *)
    ; last_error : Error.t option
    (** The most recent read/parse failure, if the latest poll failed. Successful polls
        clear it; history is kept across failures. *)
    }
  [@@deriving sexp_of, equal]
end

(** Runs [read key] every [interval] (default 500ms) and keeps the last [history]
    samples (default 600). Exceptions raised by [read] are caught and reported as
    [last_error].

    [key] is reactive: when it changes, sampling switches to reading the new key. Each
    distinct key keeps its own rolling window, so switching back to an earlier key
    restores its history rather than mixing samples from different sources.

    [interval] is reactive too. A plugin that lets the user change it at runtime should
    fold the choice into [key] so that each cadence keeps its own window, rather than
    mixing samples taken at different rates in one. *)
val poll
  :  ?interval:Time_ns.Span.t Bonsai.t
  -> ?history:int
  -> key:string Bonsai.t
  -> read:(string -> 'a Or_error.t Async.Deferred.t)
  -> local_ Bonsai.graph
  -> 'a Output.t Bonsai.t

(** [poll] specialized to reading a single file: the key is the file's path and [parse]
    turns its contents into a sample. *)
val poll_file
  :  ?interval:Time_ns.Span.t Bonsai.t
  -> ?history:int
  -> path:string Bonsai.t
  -> parse:(string -> 'a Or_error.t)
  -> local_ Bonsai.graph
  -> 'a Output.t Bonsai.t

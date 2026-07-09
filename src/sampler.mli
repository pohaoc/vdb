open! Core
open Bonsai_term

(** Polls a file on an interval and keeps a rolling window of parsed samples. This is the
    basic building block for telemetry plugins that read their data from the filesystem
    (procfs, sysfs, cgroupfs, an application's own stats file, ...). *)

module Sample : sig
  type t =
    { at : Time_ns.t
    ; value : float
    }
  [@@deriving sexp_of, equal]
end

module Output : sig
  type t =
    { samples : Sample.t list (** Oldest first. *)
    ; last_error : Error.t option
    (** The most recent read/parse failure, if the latest poll failed. Successful polls
        clear it; history is kept across failures. *)
    }
  [@@deriving sexp_of, equal]
end

(** Reads [path] every [interval] (default 500ms), parses it with [parse], and keeps the
    last [history] samples (default 600).

    [path] is reactive: when it changes, sampling switches to the new file. Each distinct
    path keeps its own rolling window, so switching back to an earlier path restores its
    history rather than mixing samples from different files. *)
val poll_file
  :  ?interval:Time_ns.Span.t
  -> ?history:int
  -> path:string Bonsai.t
  -> parse:(string -> float Or_error.t)
  -> local_ Bonsai.graph
  -> Output.t Bonsai.t

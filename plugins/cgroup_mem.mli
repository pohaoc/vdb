open! Core

(** Live-plots [memory.current] of a cgroup (cgroup v2 unified hierarchy).

    By default it samples the cgroup the vdb process itself runs in (resolved from
    [/proc/self/cgroup]). Pass [~cgroup] to watch another application's cgroup — either a
    path relative to the cgroup root (e.g. "system.slice/foo.service"), an absolute
    cgroupfs directory (e.g. "/sys/fs/cgroup/system.slice/foo.service"), or a path to any
    file containing a byte count, which is sampled as-is.

    Pressing [b] in the pane opens an in-pane browser of the cgroup hierarchy (every
    directory under [/sys/fs/cgroup] exposing a [memory.current], with its current usage
    shown inline): arrows move, right/left expand/collapse, Enter switches the pane to
    watching the selected cgroup, Esc closes. Each watched cgroup keeps its own sample
    window, and only the currently watched one is polled. *)
val plugin
  :  ?name:string
       (** Registry name; default "cgroup-memory". Give distinct names when registering
           several instances (e.g. watching different cgroups). *)
  -> ?cgroup:string
  -> ?interval:Time_ns.Span.t (** Default: 500ms. *)
  -> ?history:int
       (** Number of samples kept, i.e. the width of the plotted window. Default: one
           minute's worth (60s divided by [interval]; 120 at the default interval). *)
  -> unit
  -> Vdb.Plugin.t

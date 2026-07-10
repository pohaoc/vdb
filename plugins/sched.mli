open! Core

(** A scheduling timeline for one process: one swimlane per thread, one cell per sampling
    interval, showing what the scheduler did with the thread during that interval —

    - [▁▂▃▄▅▆▇█] running on a CPU; the glyph height is the exact fraction of the
      interval spent on CPU
    - [░▒▓] runnable but waiting for a CPU (preempted / queued — scheduling delay);
      the shade darkens with the fraction of the interval spent waiting
    - [~] uninterruptible sleep (D state, usually blocked on I/O)
    - [·] sleeping

    Run and wait keep distinct glyph families (solid blocks vs shades), so the
    distinction never rests on green-vs-red alone.

    Each cell comes from the deltas of [/proc/<pid>/task/<tid>/schedstat]'s cumulative
    on-CPU and runqueue-wait counters over the interval, so it needs no root and no
    tracing. The counters are exact; the sampling interval only sets the display
    resolution. Cells where wait exceeds run render as wait.

    The options window ([o]) sets the sampling interval (500ms down to 50ms per cell),
    switches sorting (busiest-first or tid order), and picks the cell rendering:
    "by state" (dominant share, as above), "by cpu" (running cells hued by the CPU the
    thread was last seen on, making migrations visible), or "stacked" (each cell is a
    stacked bar — green block from the bottom for the run share, red background above
    it for the wait share — so a thread running 33% and waiting 67% shows both at
    once; exact when the thread didn't also sleep within the interval). Arrows/scroll
    wheel scroll when there are more threads than fit; [p] retargets the pane to
    another pid (locked while recording).

    Recording exports one row per live thread per sample:
    [time_epoch_s, tid, comm, cpu, run_share, wait_share], where the shares are the
    fraction of the time since the previous sample (the interval in effect) spent
    on-CPU / waiting on the runqueue. *)
val plugin
  :  ?name:string (** Registry name; default "sched". *)
  -> ?pid:int (** Initial process to watch; default: the vdb process itself. *)
  -> ?history:int (** Number of samples kept per pid/interval. Default: 600. *)
  -> unit
  -> Vdb.Plugin.t

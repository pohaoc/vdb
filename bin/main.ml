open! Core

let registry =
  [ Vdb_plugins.Cgroup_mem.plugin ()
  ; Vdb_plugins.Help.plugin
  ]
;;

let () =
  Command_unix.run
    (Vdb.App.command
       ~registry
       ~initial:"cgroup-memory"
       ~summary:"vdb: a tiling visual debugger for live telemetry"
       ())
;;

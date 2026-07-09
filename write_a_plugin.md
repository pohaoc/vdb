## Writing a plugin

```ocaml
let unit_option = Vdb.Plugin.Option_spec.create ~key:"unit" [ "seconds"; "minutes" ]

let uptime =
  Vdb.Plugin.create
    ~name:"uptime"
    ~description:"time since boot"
    ~options:[ unit_option ] (* omit [?options] if there is nothing to configure *)
    (fun ~dimensions:_ ~focused:_ ~settings ~recording:_ (local_ graph) ->
      let output =
        Vdb.Sampler.poll_file
          ~path:"/proc/uptime"
          ~parse:(fun s -> Or_error.try_with (fun () ->
            Float.of_string (List.hd_exn (String.split s ~on:' '))))
          graph
      in
      let%arr output and settings in
      let latest = List.last output.samples in
      let view =
        match latest with
        | None -> View.text "sampling…"
        | Some { value; _ } ->
          (match Vdb.Plugin.Settings.find settings unit_option with
           | "minutes" -> View.text (sprintf "up %.1f min" (value /. 60.))
           | _ -> View.text (sprintf "up %.0f s" value))
      in
      { Vdb.Plugin.Output.view
      ; handler = (fun (_ : Event.t) -> Effect.Ignore)
      ; record =
          (* Omit (or use [Output.of_view]) if the pane has nothing to record. *)
          Some
            { x_axis = "time_epoch_s"
            ; y_axis = "uptime_s"
            ; latest =
                Option.map latest ~f:(fun { at; value } ->
                  Time_ns.to_span_since_epoch at |> Time_ns.Span.to_sec, value)
            }
      })
;;
```

Register it in `bin/main.ml` by adding it to the `registry` list; it then shows up in
the picker (and can be passed as the `~initial` pane).

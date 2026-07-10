open! Core
open Bonsai_term
open Bonsai.Let_syntax

module Sample = struct
  type 'a t =
    { at : Time_ns.Alternate_sexp.t
    ; value : 'a
    }
  [@@deriving sexp_of, equal]
end

module Window = struct
  type 'a t =
    { samples : 'a Sample.t Fdeque.t
    ; last_error : Error.t option
    }

  let empty = { samples = Fdeque.empty; last_error = None }
end

(* One rolling window per key ever sampled, so switching back to an earlier key restores
   its history. Keeping them all in one flat model (rather than [Bonsai.scope_model])
   matters: the polling clock's own bookkeeping must live *outside* any per-key state,
   otherwise a key switch can strand the clock mid-cycle in a saved model that never
   re-arms when restored — which froze sampling for that key. *)
module Model = struct
  type 'a t = 'a Window.t Map.M(String).t

  let empty = Map.empty (module String)
end

module Action = struct
  type 'a t =
    | Sampled of
        { key : string
        ; sample : 'a Sample.t
        }
    | Failed of
        { key : string
        ; error : Error.t
        }
end

module Output = struct
  type 'a t =
    { samples : 'a Sample.t list
    ; last_error : Error.t option
    }
  [@@deriving sexp_of, equal]
end

let poll ?interval ?(history = 600) ~key ~read (local_ graph) =
  let interval =
    Option.value interval ~default:(Bonsai.return (Time_ns.Span.of_ms 500.))
  in
  let model, inject =
    Bonsai.state_machine
      ~default_model:Model.empty
      ~apply_action:(fun _ctx (model : _ Model.t) (action : _ Action.t) ->
        let update key ~f =
          Map.update model key ~f:(fun window ->
            f (Option.value window ~default:Window.empty))
        in
        match action with
        | Failed { key; error } ->
          update key ~f:(fun window -> { window with last_error = Some error })
        | Sampled { key; sample } ->
          update key ~f:(fun window ->
            let samples = Fdeque.enqueue_back window.samples sample in
            let samples =
              if Fdeque.length samples > history
              then Fdeque.drop_front_exn samples
              else samples
            in
            { Window.samples; last_error = None }))
      graph
  in
  (* Samples carry the key they were read under, so one that completes after a key
     switch still lands in the window it belongs to. *)
  let sample_now =
    let%arr inject and key in
    let%bind.Effect action =
      Effect.of_deferred_thunk (fun () ->
        let open Async in
        let%map result =
          Monitor.try_with_or_error (fun () -> read key) >>| Or_error.join
        in
        let at = Time_ns.now () in
        match result with
        | Ok value -> Action.Sampled { key; sample = { at; value } }
        | Error error -> Action.Failed { key; error })
    in
    inject action
  in
  let () =
    Bonsai.Clock.every
      ~when_to_start_next_effect:`Every_multiple_of_period_non_blocking
      ~trigger_on_activate:true
      interval
      sample_now
      graph
  in
  (* A key change takes its first sample immediately rather than waiting out the rest of
     the current tick ([on_change'] so the initial value doesn't double up with the
     clock's [trigger_on_activate]). *)
  let () =
    Bonsai.Edge.on_change'
      ~equal:String.equal
      key
      ~callback:
        (let%arr sample_now in
         fun previous (_current : string) ->
           match previous with
           | None -> Effect.Ignore
           | Some _ -> sample_now)
      graph
  in
  let%arr model and key in
  match Map.find model key with
  | None -> { Output.samples = []; last_error = None }
  | Some window ->
    { Output.samples = Fdeque.to_list window.samples
    ; last_error = window.last_error
    }
;;

let poll_file ?interval ?history ~path ~parse (local_ graph) =
  poll
    ?interval
    ?history
    ~key:path
    ~read:(fun path ->
      let open Async in
      let%map contents =
        Monitor.try_with_or_error (fun () -> Reader.file_contents path)
      in
      Or_error.bind contents ~f:parse)
    graph
;;

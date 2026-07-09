open! Core
open Bonsai_term
open Bonsai.Let_syntax

module Sample = struct
  type t =
    { at : Time_ns.Alternate_sexp.t
    ; value : float
    }
  [@@deriving sexp_of, equal]
end

module Model = struct
  type t =
    { samples : Sample.t Fdeque.t
    ; last_error : Error.t option
    }
  [@@deriving sexp_of, equal]

  let empty = { samples = Fdeque.empty; last_error = None }
end

module Output = struct
  type t =
    { samples : Sample.t list
    ; last_error : Error.t option
    }
  [@@deriving sexp_of, equal]
end

module Action = struct
  type t =
    | Sampled of Sample.t
    | Failed of Error.t
  [@@deriving sexp_of]
end

let poll_file
  ?(interval = Time_ns.Span.of_ms 500.)
  ?(history = 600)
  ~path
  ~parse
  (local_ graph)
  =
  (* Scoping the state machine's model by [path] means a path change swaps to that
     path's own rolling window (fresh for never-seen paths) instead of mixing samples
     from different files in one window. *)
  Bonsai.scope_model
    (module String)
    ~on:path
    ~for_:(fun (local_ graph) ->
      let model, inject =
        Bonsai.state_machine
          ~default_model:Model.empty
          ~apply_action:(fun _ctx (model : Model.t) (action : Action.t) ->
            match action with
            | Failed error -> { model with last_error = Some error }
            | Sampled sample ->
              let samples = Fdeque.enqueue_back model.samples sample in
              let samples =
                if Fdeque.length samples > history
                then Fdeque.drop_front_exn samples
                else samples
              in
              { samples; last_error = None })
          graph
      in
      let () =
        Bonsai.Clock.every
          ~when_to_start_next_effect:`Every_multiple_of_period_non_blocking
          ~trigger_on_activate:true
          (Bonsai.return interval)
          (let%arr inject and path in
           let%bind.Effect action =
             Effect.of_deferred_thunk (fun () ->
               let open Async in
               let%map contents =
                 Monitor.try_with_or_error (fun () -> Reader.file_contents path)
               in
               let at = Time_ns.now () in
               match Or_error.bind contents ~f:parse with
               | Ok value -> Action.Sampled { at; value }
               | Error error -> Action.Failed error)
           in
           inject action)
          graph
      in
      let%arr model in
      { Output.samples = Fdeque.to_list model.samples; last_error = model.last_error })
    graph
;;

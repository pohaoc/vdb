open! Core
open Bonsai_term
open Bonsai.Let_syntax

module Option_spec = struct
  type t =
    { key : string
    ; label : string
    ; choices : string list
    }

  let create ~key ?label choices =
    (match choices with
     | [] -> raise_s [%message "Plugin.Option_spec.create: empty choices" (key : string)]
     | _ :: _ -> ());
    { key; label = Option.value label ~default:key; choices }
  ;;

  let default_value t = List.hd_exn t.choices
end

module Settings = struct
  type t = string Map.M(String).t [@@deriving sexp_of, equal]

  let empty = Map.empty (module String)

  let find t (spec : Option_spec.t) =
    Map.find t spec.key |> Option.value ~default:(Option_spec.default_value spec)
  ;;
end

module Record_source = struct
  type t =
    { columns : string list
    ; rows : string list list
    }
  [@@deriving sexp_of, equal]

  let cell value =
    if Float.is_integer value && Float.(abs value < 1e15)
    then sprintf "%.0f" value
    else Float.to_string value
  ;;
end

module Output = struct
  type t =
    { view : View.t
    ; handler : Event.t -> unit Effect.t
    ; record : Record_source.t option
    }

  let create ?record ~view ~handler () = { view; handler; record }

  let of_view ?record view =
    { view; handler = (fun (_ : Event.t) -> Effect.Ignore); record }
  ;;
end

type component =
  dimensions:Dimensions.t Bonsai.t
  -> focused:bool Bonsai.t
  -> settings:Settings.t Bonsai.t
  -> recording:bool Bonsai.t
  -> local_ Bonsai.graph
  -> Output.t Bonsai.t

type t =
  { name : string
  ; description : string
  ; options : Option_spec.t list
  ; component : component
  }

let create ~name ~description ?(options = []) component =
  { name; description; options; component }
;;

let of_view ~name ~description ?options f =
  create
    ~name
    ~description
    ?options
    (fun ~dimensions ~focused ~settings ~recording:_ (local_ graph) ->
      let view = f ~dimensions ~focused ~settings graph in
      let%arr view in
      Output.of_view view)
;;

let name t = t.name
let description t = t.description
let options t = t.options
let instantiate t = t.component

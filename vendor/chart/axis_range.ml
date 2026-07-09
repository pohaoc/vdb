open! Core

type t =
  | Constant of float
  | Use_min_value
  | Use_max_value

module Internal = struct
  let resolve t values =
    match t with
    | Constant x -> x
    | Use_min_value ->
      List.min_elt values ~compare:Float.compare |> Option.value ~default:0.
    | Use_max_value ->
      List.max_elt values ~compare:Float.compare |> Option.value ~default:0.
  ;;
end

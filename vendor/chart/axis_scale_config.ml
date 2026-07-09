open! Core

type t =
  | Default
  | Linear of
      { min_value : Axis_range.t
      ; max_value : Axis_range.t
      }
  | Logarithmic of
      { min_value : Axis_range.t
      ; max_value : Axis_range.t
      ; base : int
      (* The base does not affect bar height calculations. It only determines the scale
         used when auto-generating y-axis labels. We include it here so that a reasonable
         [Scalar_axis_labels_config] can be derived from this config without requiring the
         user to specify the base separately. *)
      }

module Internal = struct
  let extrema t ~data =
    match t with
    | Linear { min_value; max_value } | Logarithmic { min_value; max_value; base = _ } ->
      ( ~min_value:(Axis_range.Internal.resolve min_value data)
      , ~max_value:(Axis_range.Internal.resolve max_value data) )
    | Default ->
      ( ~min_value:(Axis_range.Internal.resolve Use_min_value data |> Float.min 0.)
      , ~max_value:(Axis_range.Internal.resolve Use_max_value data |> Float.max 0.) )
  ;;

  let resolve t ~data ~length =
    let ~min_value, ~max_value = extrema t ~data in
    match t with
    | Linear _ | Default -> Axis_scale.create_linear ~min_value ~max_value ~length
    | Logarithmic _ -> Axis_scale.create_logarithmic ~min_value ~max_value ~length
  ;;
end

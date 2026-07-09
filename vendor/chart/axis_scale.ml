open! Core

type t =
  | Linear of
      { y_intercept : float
      ; slope : float
      }
  | Logarithmic of
      { scale : float
      ; zero_value : float
      ; is_ascending : bool
      }
[@@deriving quickcheck, sexp_of]

let quickcheck_generator =
  let open Quickcheck.Generator.Let_syntax in
  let bound = 1_000_000. in
  let%bind make_linear = Bool.quickcheck_generator in
  if make_linear
  then (
    let%bind y_intercept = Float.gen_incl (-.bound) bound in
    let%bind slope =
      let%bind sign = Bool.quickcheck_generator in
      let%bind magnitude = Float.gen_incl 0.0001 bound in
      return (if sign then magnitude else -.magnitude)
    in
    return (Linear { y_intercept; slope }))
  else (
    let%bind scale = Float.gen_incl 0.1 bound in
    let%bind zero_value = Float.gen_incl (-.bound) bound in
    let%bind is_ascending = Bool.quickcheck_generator in
    return (Logarithmic { scale; zero_value; is_ascending }))
;;

let create_linear ~min_value ~max_value ~length =
  (* [min_value] doesn't actually need to be less than [max_value] - they actually
     represent the values at the bottom and top (or left and right) of a chart.

     When [min_value = max_value], we set their difference to 1. This results in a graph
     where the bottom or left value is equal to [min_value] and the top / right value is
     equal to [min_value + 1]. *)
  let dy =
    if Float.Robustly_comparable.( =. ) max_value min_value
    then 1.
    else max_value -. min_value
  in
  let slope = Float.of_int length /. dy in
  Linear { y_intercept = slope *. min_value *. -1.; slope }
;;

let create_logarithmic ~min_value ~max_value ~length =
  (* Similarly to above, [min_value] doesn't actually need to be less than [max_value] -
     they actually represent the values at the bottom and top (or left and right) of a
     chart.

     If [min_value > max_value], we imagine [max_value] as the zero value and
     logarithmically increase the value in terminal space as the input value shrinks from
     [max_value] to [min_value].

     When [min_value = max_value], we fall back to using a linear axis scale. *)
  match Float.Robustly_comparable.( =. ) max_value min_value with
  | true -> create_linear ~min_value ~max_value ~length
  | false ->
    let is_ascending = Float.( <= ) min_value max_value in
    let dy =
      if Float.Robustly_comparable.( =. ) max_value min_value
      then 1.
      else Float.abs (max_value -. min_value)
    in
    let scale = Float.of_int length /. Float.log dy in
    let zero_value = if is_ascending then min_value else Float.neg min_value in
    Logarithmic { zero_value; scale; is_ascending }
;;

module Internal = struct
  let apply t x =
    match t with
    | Linear { y_intercept; slope } -> y_intercept +. (slope *. x)
    | Logarithmic { scale; zero_value; is_ascending } ->
      let x = if is_ascending then x else Float.neg x in
      let x = x -. zero_value in
      (* If x < 1. (meaning that log(x) < 0), we switch to a linearly decreasing value.
         This is to make the function invertible for every value, which is primarily
         useful for the quickcheck test. *)
      if Float.( < ) x 1. then x -. 1. else scale *. Float.log x
  ;;

  let invert t y =
    match t with
    | Linear { y_intercept; slope } -> (y -. y_intercept) /. slope
    | Logarithmic { scale; zero_value; is_ascending } ->
      let x =
        if Float.( <= ) y 0.
        then y +. 1. +. zero_value
        else Float.exp (y /. scale) +. zero_value
      in
      if is_ascending then x else Float.neg x
  ;;
end

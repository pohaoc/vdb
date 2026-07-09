open! Core

type t = float -> string

let make_reasonable_linear ~max_length ~min_value ~max_value =
  let suffix, denominator =
    match Float.max (Float.abs min_value) (Float.abs max_value) with
    | x when Float.( < ) x 1_000.0 -> "", 1.0
    | x when Float.( < ) x 1_000_000.0 -> "K", 1_000.0
    | x when Float.( < ) x 1_000_000_000.0 -> "M", 1_000_000.0
    | x when Float.( < ) x 1_000_000_000_000.0 -> "B", 1_000_000_000.0
    | _ -> "T", 1_000_000_000_000.0
  in
  let units_per_row =
    (max_value -. min_value) /. (Float.of_int max_length *. denominator) |> Float.abs
  in
  let rec get_num_decimals acc x =
    if Float.Robustly_comparable.( =. ) units_per_row 0.
    then 0
    else if Float.( >= ) (Float.abs x) 1.0
    then acc
    else get_num_decimals (acc + 1) (x *. 10.0)
  in
  let decimals = get_num_decimals 0 units_per_row in
  fun x ->
    let x = x /. denominator in
    Float.to_string_hum ~strip_zero:true ~delimiter:',' ~decimals x ^ suffix
;;

let make_reasonable_logarithmic ~base =
  let format_power power =
    let superscript_digit = function
      | '0' -> "⁰"
      | '1' -> "¹"
      | '2' -> "²"
      | '3' -> "³"
      | '4' -> "⁴"
      | '5' -> "⁵"
      | '6' -> "⁶"
      | '7' -> "⁷"
      | '8' -> "⁸"
      | '9' -> "⁹"
      | '-' -> "⁻"
      | _ -> ""
    in
    if power = 1
    then ""
    else Int.to_string power |> String.concat_map ~f:superscript_digit
  in
  fun x ->
    if not (Float.is_positive x && Float.is_finite x)
    then Float.to_string_hum ~decimals:2 x ~strip_zero:true
    else (
      let power =
        Float.log x /. Float.log (Float.of_int base) |> Float.iround_exn ~dir:`Nearest
      in
      let coefficient =
        x /. Float.( ** ) (Float.of_int base) (Float.of_int power)
        |> Float.to_string_hum ~decimals:1 ~strip_zero:true
      in
      if power = 0
      then coefficient
      else if String.equal coefficient "1"
      then [%string "%{base#Int}%{format_power power}"]
      else [%string "%{coefficient}×%{base#Int}%{format_power power}"])
;;

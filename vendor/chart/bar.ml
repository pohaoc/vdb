open! Core
open Bonsai_term

let solid_block = Uchar.Utf8.of_string "█"
let down_arrow = Uchar.Utf8.of_string "↓"
let up_arrow = Uchar.Utf8.of_string "↑"

let block_eighths =
  Array.map [| " "; "▁"; "▂"; "▃"; "▄"; "▅"; "▆"; "▇"; "█" |] ~f:Uchar.Utf8.of_string
;;

let view ~bar_height ~max_bar_height ~bar_width ~color =
  let make_row c =
    View.text
      ~attrs:(Option.map color ~f:Attr.fg |> Option.to_list)
      (String.concat (List.create ~len:bar_width (Uchar.Utf8.to_string c)))
  in
  let make_block height =
    (* Fill the rectangle with a character so it shows up in tests. *)
    View.vcat @@ List.init height ~f:(fun _ -> make_row solid_block)
  in
  (* We can represent a precision of 1/8th of a unit. If the conversion to an integer
     fails, we set the height to 0 (effectively hiding the bar). *)
  let height_in_eighths =
    bar_height *. 8. |> Float.iround ~dir:`Down |> Option.value ~default:0
  in
  let bar =
    match height_in_eighths with
    | n when n < 0 -> make_row down_arrow
    | 0 -> View.transparent_rectangle ~width:bar_width ~height:1
    | n when n > max_bar_height * 8 ->
      View.vcat [ make_row up_arrow; make_block (max_bar_height - 1) ]
    | _ ->
      let whole_blocks = height_in_eighths / 8 in
      let extra_eighths = height_in_eighths % 8 in
      View.vcat [ make_row block_eighths.(extra_eighths); make_block whole_blocks ]
  in
  View.pad bar ~t:(max_bar_height - View.height bar)
;;

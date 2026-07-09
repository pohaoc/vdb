open Core
open Bonsai_term

let view ~labels ~direction ~length =
  let combine =
    match direction with
    | `Vertical ->
      fun ~prev_labels ~next_label ->
        let next_label_padding =
          Int.max 0 (View.width prev_labels - View.width next_label)
        in
        let prev_labels_padding =
          Int.max 0 (View.width next_label - View.width prev_labels)
        in
        View.vcat
          [ View.pad ~l:next_label_padding next_label
          ; View.pad ~l:prev_labels_padding prev_labels
          ]
    | `Horizontal -> fun ~prev_labels ~next_label -> View.hcat [ prev_labels; next_label ]
  in
  let make_desired_length =
    match direction with
    | `Vertical ->
      fun view ~length ->
        if View.height view <= length
        then View.pad ~t:(length - View.height view) view
        else View.crop ~t:(View.height view - length) view
    | `Horizontal ->
      fun view ~length ->
        if View.width view <= length
        then View.pad ~r:(length - View.width view) view
        else View.crop ~r:(View.width view - length) view
  in
  List.filter labels ~f:(fun ({ index; view = _ } : Label.t) ->
    Int.between index ~low:0 ~high:(length - 1))
  |> List.rev
     (* dedup and sort takes the last element that's equal but we want to take the first
        element that occurs at a certain position. *)
  |> List.dedup_and_sort
       ~compare:(Comparable.lift Int.compare ~f:(fun (t : Label.t) -> t.index))
  |> List.fold
       ~init:(make_desired_length (View.transparent_rectangle ~width:1 ~height:1) ~length)
       ~f:(fun labels ({ index = pos; view = next_label } : Label.t) ->
         let prev_labels = make_desired_length labels ~length:pos in
         combine ~prev_labels ~next_label)
  |> make_desired_length ~length
;;

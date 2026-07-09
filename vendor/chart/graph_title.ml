open! Core
open Bonsai_term

let get_fg_attr color = Option.map color ~f:Attr.fg |> Option.to_list

let split_text_into_lines_not_longer_than text ~max_width =
  let utf8 = String.Utf8.of_string text in
  let is_whitespace_uchar uchar =
    let whitespace_uchars = List.map [ ' '; '\n'; '\t'; '\r' ] ~f:Uchar.of_char in
    List.mem whitespace_uchars uchar ~equal:Uchar.equal
  in
  let utf8_string_width s =
    Bonsai_term_typography.Tty_width.string_width (String.Utf8.to_string s)
  in
  let words_not_longer_than_max_width =
    let reversed_words, last_word =
      String.Utf8.fold
        utf8
        ~init:([], String.Utf8.of_string "")
        ~f:(fun (acc, cur_word) uchar ->
          if is_whitespace_uchar uchar
          then
            if utf8_string_width cur_word > 0
            then cur_word :: acc, String.Utf8.of_string ""
            else acc, String.Utf8.of_string ""
          else (
            let word_with_next_uchar =
              String.Utf8.append cur_word (String.Utf8.of_list [ uchar ])
            in
            if utf8_string_width word_with_next_uchar > max_width
            then cur_word :: acc, String.Utf8.of_list [ uchar ]
            else acc, word_with_next_uchar))
    in
    List.rev
      (if utf8_string_width last_word > 0
       then last_word :: reversed_words
       else reversed_words)
  in
  let reversed_lines, last_line =
    List.fold
      words_not_longer_than_max_width
      ~init:([], String.Utf8.of_string "")
      ~f:(fun (acc, cur_line) chunk ->
        let line_with_next_chunk =
          if utf8_string_width cur_line = 0
          then chunk
          else String.Utf8.concat [ cur_line; String.Utf8.of_string " "; chunk ]
        in
        let width_with_next_chunk = utf8_string_width line_with_next_chunk in
        if width_with_next_chunk <= max_width
        then acc, line_with_next_chunk
        else cur_line :: acc, chunk)
  in
  List.rev
    (if utf8_string_width last_line > 0
     then last_line :: reversed_lines
     else reversed_lines)
  |> List.map ~f:String.Utf8.to_string
;;

let view title ~width ~text_color ~border_color =
  if width < 5
  then View.transparent_rectangle ~height:1 ~width
  else (
    let remove_zero_width_uchars s =
      String.Utf8.of_string s
      |> String.Utf8.filter ~f:(fun uchar -> View.uchar_tty_width uchar > 0)
      |> String.Utf8.to_string
    in
    (* If the title has a line break, we make sure to keep a line break in that spot in
       the title view. Each string in this list may have more line breaks inserted if it
       is longer than the max width. *)
    let title_with_line_breaks =
      String.split title ~on:'\n'
      |> List.map ~f:remove_zero_width_uchars
      |> List.filter ~f:(fun s -> Bonsai_term_typography.Tty_width.string_width s > 0)
    in
    (* We need two characters of space for the border. *)
    let width_for_text = width - 2 in
    let is_one_line =
      match title_with_line_breaks with
      | [] -> true
      | [ title ] -> Bonsai_term_typography.Tty_width.string_width title <= width_for_text
      | _ -> false
    in
    (* awkward special casing for one line titles so that the border box does not stretch
       the whole width if the title is skinnier than that. *)
    if is_one_line
    then (
      let title_text = List.hd title_with_line_breaks |> Option.value ~default:"" in
      View.text ~attrs:(get_fg_attr text_color) title_text
      |> Bonsai_term_border_box.view ~attrs:(get_fg_attr border_color)
      |> View.center ~within:{ Dimensions.height = 3; width })
    else
      (* center each line rather (as opposed to centering the whole view) *)
      List.map
        title_with_line_breaks
        ~f:(split_text_into_lines_not_longer_than ~max_width:width_for_text)
      |> List.join
      |> List.map ~f:(View.text ~attrs:(get_fg_attr text_color))
      |> List.map
           ~f:(View.center ~within:{ Dimensions.height = 1; width = width_for_text })
      |> View.vcat
      |> Bonsai_term_border_box.view ~attrs:(get_fg_attr text_color))
;;

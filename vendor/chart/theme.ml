open! Core
open Bonsai_term

type t =
  { data : Attr.Color.t option
  ; label_text : Attr.Color.t option
  ; title : Attr.Color.t option
  ; title_border : Attr.Color.t option
  ; border : Attr.Color.t option
  }
[@@deriving fields ~getters]

let empty =
  { data = None; label_text = None; title = None; title_border = None; border = None }
;;

let catppuccin ~flavor ~data_color =
  { data = Some (Bonsai_term_catppuccin.color ~flavor data_color)
  ; label_text = Some (Bonsai_term_catppuccin.color ~flavor Subtext0)
  ; title = Some (Bonsai_term_catppuccin.color ~flavor Text)
  ; title_border = Some (Bonsai_term_catppuccin.color ~flavor Text)
  ; border = Some (Bonsai_term_catppuccin.color ~flavor Overlay1)
  }
;;

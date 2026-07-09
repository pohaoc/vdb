open! Core
open Bonsai_term

(** Does not render if [width < 5]. *)
val view
  :  string
  -> width:int
  -> text_color:Attr.Color.t option
  -> border_color:Attr.Color.t option
  -> View.t

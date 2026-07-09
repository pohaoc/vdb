(** Theme to customize your plot. *)

open! Core
open Bonsai_term

type t =
  { data : Attr.Color.t option
  (** Default color for the core part of the plot. For example, the bars of a bar chart or
      points in a scatter plot. *)
  ; label_text : Attr.Color.t option (** x / y labels *)
  ; title : Attr.Color.t option
  ; title_border : Attr.Color.t option
  ; border : Attr.Color.t option
  }
[@@deriving fields ~getters]

val empty : t

val catppuccin
  :  flavor:Bonsai_term_catppuccin.Flavor.t
  -> data_color:Bonsai_term_catppuccin.t
  -> t

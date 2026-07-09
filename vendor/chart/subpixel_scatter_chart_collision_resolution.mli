open! Core
open Bonsai_term

(** What to do when two scatter chart points fall onto the same subpixel.

    We will use each field in the [Special] record if provided. Otherwise, we fall back to
    using the icon / color / background that would have been used if there was no
    collision. *)
type t =
  | Ignore_collision
  | Special of
      { icon : Scatter_chart_icon.t option
      ; color : Attr.Color.t option
      ; background : Attr.Color.t option
      }

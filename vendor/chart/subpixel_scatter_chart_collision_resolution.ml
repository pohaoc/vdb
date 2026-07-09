open! Core
open Bonsai_term

type t =
  | Ignore_collision
  | Special of
      { icon : Scatter_chart_icon.t option
      ; color : Attr.Color.t option
      ; background : Attr.Color.t option
      }

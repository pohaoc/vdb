open! Core
open Bonsai_term

type t =
  { background : Attr.Color.t option
  ; icon_color : [ `Take_first | `Special of Attr.Color.t ]
  ; icon : [ `Take_first | `Special of Scatter_chart_icon.t ]
  }

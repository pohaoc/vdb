open! Core
open Bonsai_term

type 'coordinate t =
  { coordinate : 'coordinate
  ; icon : Scatter_chart_icon.t
  ; color : Attr.Color.t option
  }
[@@deriving fields ~getters]

let create ?color ?(icon = Scatter_chart_icon.dot) coordinate =
  { coordinate; icon; color }
;;

open! Core
open Bonsai_term

type 'coordinate t =
  { coordinate : 'coordinate
  ; icon : Scatter_chart_icon.t
  ; color : Attr.Color.t option
  }
[@@deriving fields ~getters]

val create
  :  ?color:Attr.Color.t
  -> ?icon:Scatter_chart_icon.t (** Default: [Scatter_chart_icon.dot] *)
  -> 'coordinate
  -> 'coordinate t

val coordinate : 'coordinate t -> 'coordinate

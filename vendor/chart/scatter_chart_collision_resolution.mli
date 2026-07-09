(** Describes what to do when two scatter chart points fall on the same terminal
    [Position.t]. *)

open! Core
open Bonsai_term

type t =
  { background : Attr.Color.t option
  ; icon_color : [ `Take_first | `Special of Attr.Color.t ]
  (** [`Take_first] means that the color of the icon will be the color of the first
      [Scatter_chart_point.t] that was placed on the cell. [`Special color] means that if
      more than one [Scatter_chart_point.t] fall on the same [Position.t], the icon will
      be [color] regardless of the color of the points. *)
  ; icon : [ `Take_first | `Special of Scatter_chart_icon.t ]
  (** Similar logic to [icon_color]. *)
  }

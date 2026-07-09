open! Core
open Bonsai_term

(** The returned [View.t] will always have height [max_bar_height] and width [bar_width],
    even if [bar_height] is less than [max_bar_height].

    If [bar_height < 0.], we render a line of down arrows (↓).

    If the bar would be taller than [max_bar_height], we replace the top row with up
    arrows (↑). *)
val view
  :  bar_height:float
  -> max_bar_height:int
  -> bar_width:int
  -> color:Attr.Color.t option
  -> View.t

open! Core
open Bonsai_term

(** Create a [width]×[height] grid. Each cell is broken into a 2×4 grid, so the resolution
    is [2×width]×[4×height]. *)
val view
  :  (x:float * y:float) list
  -> color:Attr.Color.t option
  -> collision_resolution:Subpixel_scatter_chart_collision_resolution.t
  -> height:int
  -> width:int
  -> View.t

module For_testing : sig
  module Cell_info : sig
    type t

    val empty : t
    val add : t -> sub_cell_position:Position.t -> t
    val bit_for_dot : Position.t -> int
    val base_braille_codepoint : int

    val resolve
      :  t
      -> color:Attr.Color.t option
      -> collision_resolution:Subpixel_scatter_chart_collision_resolution.t
      -> text:string * fg_color:Attr.Color.t option * bg_color:Attr.Color.t option
  end
end

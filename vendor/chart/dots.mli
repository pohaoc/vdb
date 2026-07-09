open! Core
open Bonsai_term

(** Create a [width]×[height] grid and plot the given points. *)
val view
  :  Position.t Scatter_chart_point.t list
  -> collision_resolution:Scatter_chart_collision_resolution.t
  -> height:int
  -> width:int
  -> View.t

module For_testing : sig
  module Cell_info : sig
    type t [@@deriving sexp_of]
  end

  val position_map
    :  Position.t Scatter_chart_point.t list
    -> collision_resolution:Scatter_chart_collision_resolution.t
    -> height:int
    -> width:int
    -> Cell_info.t Map.M(Position).t
end

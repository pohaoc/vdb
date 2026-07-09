open! Core
open Bonsai_term

(** A binary tree of tiled panes. Pure data structure: the app keeps one of these in its
    state machine and derives per-pane screen regions from it each frame. *)

module Id : sig
  type t [@@deriving sexp, compare, equal]

  include Comparable.S with type t := t

  val zero : t
  val succ : t -> t
  val to_string : t -> string
end

module Split_dir : sig
  (** Vim-style naming: [Horizontal] stacks the two panes on top of each other (like
      [:split]); [Vertical] puts them side by side (like [:vsplit]). *)
  type t =
    | Horizontal
    | Vertical
  [@@deriving sexp, equal]
end

module Direction : sig
  type t =
    | Left
    | Down
    | Up
    | Right
  [@@deriving sexp, equal]
end

type t [@@deriving sexp, equal]

val leaf : Id.t -> t
val leaves : t -> Id.t list

(** Replace leaf [at] with a split containing [at] and [new_id]; the new pane goes below
    (for [Horizontal]) or to the right (for [Vertical]). Returns [t] unchanged if [at] is
    not in the tree. *)
val split : t -> at:Id.t -> dir:Split_dir.t -> new_id:Id.t -> t

(** Remove leaf [id] and collapse its parent split. [`Closed (t, focus)] suggests the
    pane that took over the freed space as the new focus. *)
val close : t -> id:Id.t -> [ `Closed of t * Id.t | `Empty | `Not_found ]

(** Swap the positions of two panes, leaving the split structure unchanged. *)
val swap : t -> a:Id.t -> b:Id.t -> t

(** Grow ([by] > 0) or shrink ([by] < 0) leaf [id]'s share of the nearest enclosing split
    with orientation [dir]. [by] is a fraction of that split's extent. *)
val resize : t -> id:Id.t -> dir:Split_dir.t -> by:float -> t

(** Tile [region] between all leaves. Every leaf gets a region and the regions tile
    [region] exactly. *)
val layout : t -> region:Region.t -> (Id.t, Region.t) List.Assoc.t

(** Given the regions from [layout], the pane you land on when moving from pane [of_]
    towards [direction] (nearest edge, then largest perpendicular overlap). *)
val neighbor
  :  (Id.t, Region.t) List.Assoc.t
  -> of_:Id.t
  -> direction:Direction.t
  -> Id.t option

(** The pane whose region contains the point [(x, y)], if any. *)
val find_at : (Id.t, Region.t) List.Assoc.t -> x:int -> y:int -> Id.t option

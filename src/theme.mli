open! Core
open Bonsai_term

(** vdb's color theme: the catppuccin Mocha palette. Views name colors by semantic role
    (catppuccin's role names) rather than using raw colors, so the palette stays
    swappable in one place. *)

val color : Bonsai_term_catppuccin.t -> Attr.Color.t

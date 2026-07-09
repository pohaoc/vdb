open! Core
open Bonsai_term

(** The returned view is guaranteed to have a height of [length] if the direction is
    [`Vertical] or a width of [length] if the direction is [`Horizontal].

    [labels] is filtered to labels where 0 <= [Label.index] < [length], and overlapping
    labels will be cropped. For example:

    [view ~labels: [ { Label.index = -1; view = View.text "aa" } ; { Label.index = 1; view = View.text "bb" } ; { Label.index = 2; view = View.text "cc" } ] ~direction:`Horizontal ~length:3]

    will result in a [View.t] that is identical to [View.text " bc"]. Note that the first
    "aa" is dropped completely (even though the second "a" would be in the first
    position), "bb" is cropped to "b" because the "cc" label overwrites the second "b",
    and "cc" is cropped to "c" because the length is capped at 3. *)
val view
  :  labels:Label.t list
  -> direction:[ `Vertical | `Horizontal ]
  -> length:int
  -> View.t

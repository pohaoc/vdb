open! Core
open Bonsai_term

let resize_x_axis_title view ~goal_width =
  let width = View.width view in
  match width <= goal_width with
  | true ->
    let within = { Dimensions.height = View.height view; width = goal_width } in
    View.center view ~within
  | false -> View.crop ~r:(width - goal_width) view
;;

let resize_y_axis_title view ~goal_height =
  let height = View.height view in
  match height <= goal_height with
  | true ->
    let within = { Dimensions.height = goal_height; width = View.width view } in
    View.center view ~within
  | false -> View.crop ~b:(height - goal_height) view
;;

open! Core
open Bonsai_term

module Id = struct
  include Int

  let zero = 0
  let succ = Int.succ
end

module Split_dir = struct
  type t =
    | Horizontal
    | Vertical
  [@@deriving sexp, equal]
end

module Direction = struct
  type t =
    | Left
    | Down
    | Up
    | Right
  [@@deriving sexp, equal]
end

type t =
  | Leaf of Id.t
  | Split of
      { dir : Split_dir.t
      ; ratio : float
      ; first : t
      ; second : t
      }
[@@deriving sexp, equal]

let leaf id = Leaf id

let rec leaves = function
  | Leaf id -> [ id ]
  | Split { first; second; _ } -> leaves first @ leaves second
;;

let rec first_leaf = function
  | Leaf id -> id
  | Split { first; _ } -> first_leaf first
;;

let rec split t ~at ~dir ~new_id =
  match t with
  | Leaf id when Id.equal id at ->
    Split { dir; ratio = 0.5; first = Leaf id; second = Leaf new_id }
  | Leaf _ -> t
  | Split s ->
    Split
      { s with
        first = split s.first ~at ~dir ~new_id
      ; second = split s.second ~at ~dir ~new_id
      }
;;

let rec close t ~id =
  match t with
  | Leaf leaf -> if Id.equal leaf id then `Empty else `Not_found
  | Split ({ first; second; _ } as s) ->
    (match close first ~id with
     | `Empty -> `Closed (second, first_leaf second)
     | `Closed (first, focus) -> `Closed (Split { s with first }, focus)
     | `Not_found ->
       (match close second ~id with
        | `Empty -> `Closed (first, first_leaf first)
        | `Closed (second, focus) -> `Closed (Split { s with second }, focus)
        | `Not_found -> `Not_found))
;;

let rec swap t ~a ~b =
  match t with
  | Leaf id when Id.equal id a -> Leaf b
  | Leaf id when Id.equal id b -> Leaf a
  | Leaf _ -> t
  | Split s -> Split { s with first = swap s.first ~a ~b; second = swap s.second ~a ~b }
;;

let rec mem t id =
  match t with
  | Leaf leaf -> Id.equal leaf id
  | Split { first; second; _ } -> mem first id || mem second id
;;

let clamp_ratio ratio = Float.clamp_exn ratio ~min:0.1 ~max:0.9

(* Adjust the ratio of the nearest enclosing split of orientation [dir]: walk down
   towards [id], and apply the adjustment to the deepest matching split on the way. *)
let resize t ~id ~dir ~by =
  let rec loop t =
    match t with
    | Leaf _ -> t, false
    | Split s ->
      let in_first = mem s.first id in
      let in_second = mem s.second id in
      if not (in_first || in_second)
      then t, false
      else (
        let child, resized_below =
          if in_first
          then (
            let first, r = loop s.first in
            Split { s with first }, r)
          else (
            let second, r = loop s.second in
            Split { s with second }, r)
        in
        if resized_below
        then child, true
        else if Split_dir.equal s.dir dir
        then (
          (* Growing the focused pane means growing whichever side it lives on. *)
          let by = if in_first then by else -.by in
          let ratio =
            match child with
            | Split s -> clamp_ratio (s.ratio +. by)
            | Leaf _ -> s.ratio
          in
          (match child with
           | Split s -> Split { s with ratio }, true
           | Leaf _ -> child, false))
        else child, false)
  in
  fst (loop t)
;;

(* Below this size a pane can't even show its border; layout stops shrinking a side. *)
let min_width = 4
let min_height = 3

let split_extent extent ~ratio ~min_size =
  if extent <= min_size * 2
  then extent / 2
  else (
    let first = Float.iround_nearest_exn (Float.of_int extent *. ratio) in
    Int.clamp_exn first ~min:min_size ~max:(extent - min_size))
;;

let rec layout t ~(region : Region.t) =
  match t with
  | Leaf id -> [ id, region ]
  | Split { dir; ratio; first; second } ->
    (match dir with
     | Vertical ->
       let w1 = split_extent region.width ~ratio ~min_size:min_width in
       layout first ~region:{ region with width = w1 }
       @ layout second ~region:{ region with x = region.x + w1; width = region.width - w1 }
     | Horizontal ->
       let h1 = split_extent region.height ~ratio ~min_size:min_height in
       layout first ~region:{ region with height = h1 }
       @ layout
           second
           ~region:{ region with y = region.y + h1; height = region.height - h1 })
;;

let overlap ~lo1 ~hi1 ~lo2 ~hi2 = Int.min hi1 hi2 - Int.max lo1 lo2

let neighbor lay ~of_ ~(direction : Direction.t) =
  match List.Assoc.find lay of_ ~equal:Id.equal with
  | None -> None
  | Some (r : Region.t) ->
    let candidates =
      List.filter_map lay ~f:(fun (id, (c : Region.t)) ->
        if Id.equal id of_
        then None
        else (
          (* Distance from our trailing edge to the candidate's leading edge, and how
             much the two panes overlap on the perpendicular axis. *)
          let distance, side_overlap =
            match direction with
            | Left ->
              ( r.x - (c.x + c.width)
              , overlap ~lo1:r.y ~hi1:(r.y + r.height) ~lo2:c.y ~hi2:(c.y + c.height) )
            | Right ->
              ( c.x - (r.x + r.width)
              , overlap ~lo1:r.y ~hi1:(r.y + r.height) ~lo2:c.y ~hi2:(c.y + c.height) )
            | Up ->
              ( r.y - (c.y + c.height)
              , overlap ~lo1:r.x ~hi1:(r.x + r.width) ~lo2:c.x ~hi2:(c.x + c.width) )
            | Down ->
              ( c.y - (r.y + r.height)
              , overlap ~lo1:r.x ~hi1:(r.x + r.width) ~lo2:c.x ~hi2:(c.x + c.width) )
          in
          if distance >= 0 && side_overlap > 0
          then Some (id, distance, side_overlap)
          else None))
    in
    List.min_elt
      candidates
      ~compare:(fun (_, d1, o1) (_, d2, o2) ->
        match Int.compare d1 d2 with
        | 0 -> Int.compare o2 o1
        | c -> c)
    |> Option.map ~f:(fun (id, _, _) -> id)
;;

let find_at lay ~x ~y =
  List.find_map lay ~f:(fun (id, (r : Region.t)) ->
    if x >= r.x && x < r.x + r.width && y >= r.y && y < r.y + r.height
    then Some id
    else None)
;;

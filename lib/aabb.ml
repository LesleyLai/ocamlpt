type t = { min: Vec3.t; max: Vec3.t }

let create min max = { min; max }

let hit (r: Ray.t) (tmin: float) (tmax: float) (aabb: t) =
  let helper get_axis old_tmin old_tmax =
    let invD = 1.0 /. (get_axis r.direction) in
    let (t0, t1) =
      let (t0', t1') =
        (((get_axis aabb.min) -. (get_axis r.origin)) *. invD,
         ((get_axis aabb.max) -. (get_axis r.origin)) *. invD) in
      if invD < 0. then
        (t1', t0')
      else
        (t0', t1')
    in
    let new_tmin = Float.max t0 old_tmin
    and new_tmax = Float.min t1 old_tmax in
    if new_tmax <= new_tmin then
      None
    else
      Some (new_tmin, new_tmax)
  in
  match helper (fun pt -> pt.x) tmin tmax with
  | None -> false
  | Some (tmin1, tmax1) ->
    match helper (fun pt -> pt.y) tmin1 tmax1 with
    | None -> false
    | Some (tmin2, tmax2) ->
      Option.is_some (helper (fun pt -> pt.z) tmin2 tmax2)

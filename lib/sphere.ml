open Base
open Vec3

type t = {center: Vec3.t; radius: float; material: Material.t}

let create center radius material =
  {center; radius; material}

(*
void get_sphere_uv(const vec3& p, double& u, double& v) {
    auto phi = atan2(p.z(), p.x());
    auto theta = asin(p.y());
    u = 1-(phi + pi) / (2*pi);
    v = (theta + pi/2) / pi;
}
*)

let get_sphere_uv (p: Vec3.t) =
  let open Float in
  let phi = atan2 p.z p.x
  and theta = asin p.y in
  let u = 1. - (phi + pi) / (2. * pi)
  and v = (theta + pi/2.) / pi in
  (u, v)

let hit (r: Ray.t) ({center; radius; material}: t): Material.hit_record option =
  let open Float in
  let t_min = 0.00001 in
  let oc = r.origin -| center in
  let a = Vec3.dot r.direction r.direction
  and half_b = Vec3.dot oc r.direction
  and c = (Vec3.dot oc oc) -. radius *. radius in

  let hit_record_from_t t face_direction: Material.hit_record option =
    if (t > t_min) then
      let p = Ray.at t r in
      let outward_normal = ((p -| center) /| radius) in
      let normal =
        match face_direction with
        | Material.FrontFace -> outward_normal
        | Material.BackFace -> (negate outward_normal)
      in
      let (u, v) = get_sphere_uv p in
      Some {t; p; normal; material; u; v; face_direction}
    else
      None
  in

  let quater_discriminant = half_b *. half_b -. a *. c in
  if quater_discriminant > 0. then
    let root = sqrt quater_discriminant in
    let t1 = (-.half_b -. root)/. a in
    hit_record_from_t t1 Material.FrontFace |>
    Option_ext.or_else ~f:(
      fun () ->
        let t2 = (-.half_b +. root)/. a in
        hit_record_from_t t2 Material.BackFace)
  else
    None

let bounding_box (sphere: t): Aabb.t =
  let open Vec3 in
  let r = (Vec3.create sphere.radius sphere.radius sphere.radius) in
  {min=sphere.center -| r; max=sphere.center +| r}

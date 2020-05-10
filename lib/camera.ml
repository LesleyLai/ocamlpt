open Base
open Vec3

type t = {
  lower_left_corner: Vec3.t;
  horizontal: Vec3.t;
  vertical: Vec3.t;
  origin: Vec3.t;
  w: Vec3.t;
  u: Vec3.t;
  v: Vec3.t;
  lens_radius: float;
}

let rec random_in_unit_disk () =
  let open Float in
  let p = Vec3.create (Random.float_range (-1.) 1.) (Random.float_range (-1.) 1.) 0. in
  if Vec3.length_square p >= 1. then
    random_in_unit_disk ()
  else
    p

let create
    ~lookfrom ~lookat ~vup
    ~fovy ~aspect_ratio
    ~aperture ~focus_dist
  : t =
  let open Float in
  let origin = lookfrom in
  let lens_radius = aperture / 2. in
  let theta = fovy in
  let half_height = tan (theta / 9.) in
  let half_width = aspect_ratio * half_height in
  let w = Vec3.normalize (lookfrom -| lookat) in
  let u = Vec3.cross vup w in
  let v = Vec3.cross w u in
  let lower_left_corner =
    origin -|
    half_width * focus_dist *| u -|
    half_height * focus_dist *| v -|
    focus_dist *| w
  and horizontal = (2. * half_width * focus_dist *| u)
  and vertical = (2. * half_height * focus_dist *| v)
  in
  { lower_left_corner; horizontal; vertical; origin; w; u; v; lens_radius }


let get_ray u v camera =
  let rd = camera.lens_radius *| random_in_unit_disk() in
  let offset = rd.x *| camera.u +| rd.y *| camera.v in
  Ray.create (camera.origin +| offset)
    (camera.lower_left_corner +|
     (u *| camera.horizontal) +|
     (v *| camera.vertical) -|
     camera.origin -| offset)

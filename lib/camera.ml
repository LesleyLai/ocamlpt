open Base
open Vec3

type t = {
  lower_left_corner: Vec3.t;
  horizontal: Vec3.t;
  vertical: Vec3.t;
  origin: Vec3.t;
}

let create
    ~lookfrom ~lookat ~vup
    ~fovy ~aspect_ratio : t =
  let open Float in
  let origin = lookfrom in
  let theta = fovy in
  let half_height = tan (theta / 9.) in
  let half_width = aspect_ratio * half_height in
  let w = Vec3.normalize (lookfrom -| lookat) in
  let u = Vec3.cross vup w in
  let v = Vec3.cross w u in
  let lower_left_corner = origin -| half_width *| u -| half_height *| v -| w
  and horizontal = (2. * half_width *| u)
  and vertical = (2. * half_height *| v)
  in
  { lower_left_corner; horizontal; vertical; origin }

let get_ray u v camera = Ray.create camera.origin
    (camera.lower_left_corner +|
     (u *| camera.horizontal) +|
     (v *| camera.vertical) -|
     camera.origin)

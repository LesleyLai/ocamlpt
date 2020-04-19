open Base

type t = { x: float; y: float; z: float }

let create x y z = { x; y; z }

let (+|) v1 v2 = {x= v1.x +.v2.x; y= v1.y +. v2.y; z= v1.z +. v2.z}

let (-|) v1 v2 = {x= v1.x -.v2.x; y= v1.y -. v2.y; z= v1.z -. v2.z}

let ( *| ) s v1 = {x= v1.x *. s; y= v1.y *. s; z= v1.z *. s}

let (/|) v1 s =
  let s_inv = 1. /. s in
  s_inv *| v1

let dot v1 v2 =
  v1.x *. v2.x +. v1.y *. v2.y +. v1.z *. v2.z

let length_square v1 =
  dot v1 v1

let length v1 =
  Float.sqrt (length_square v1)

let negate v =
  {x = (-.v.x); y = (-.v.y); z = (-.v.z)}

let cross u v =
  {
    x = u.y *. v.z -. u.z *. v.y;
    y = u.z *. v.x -. u.x *. v.z;
    z = u.x *. v.y -. u.y *. v.x;
  }

let normalize v =
  v /| (length v)

let lerp v1 v2 t =
  ((1. -. t) *| v1) +| (t *| v2)

let zero =
  create 0. 0. 0.

let elem_wise_product v1 v2 =
  { x = v1.x *. v2.x; y = v1.y *. v2.y; z = v1.z *. v2.z }

let reflect v n =
  v -| (2. *. (dot v n)) *| n

let refract uv n etai_over_etat =
  let open Float in
  let cos_theta = dot (negate uv) n in
  let r_out_parallel = etai_over_etat *| (uv +| cos_theta *| n) in
  let r_out_perp = (-sqrt (1.0 - length_square(r_out_parallel))) *| n in
  r_out_parallel +| r_out_perp

let to_string {x; y; z} =
  Printf.sprintf "{%f, %f, %f}" x y z

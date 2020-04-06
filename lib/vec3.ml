type t = { x: float; y: float; z: float }

let create x y z = { x; y; z }

let (+|) v1 v2 = {x= v1.x +.v2.x; y= v1.y +. v2.y; z= v1.z +. v2.z}

let (-|) v1 v2 = {x= v1.x -.v2.x; y= v1.y -. v2.y; z= v1.z -. v2.z}

let mult v1 s = {x= v1.x *. s; y= v1.y *. s; z= v1.z *. s}

let (/|) v1 s =
  let s_inv = 1. /. s in
  mult v1 s_inv

let dot v1 v2 =
  v1.x *. v2.x +. v1.y *. v2.y +. v1.z *. v2.z

let length_square v1 =
  dot v1 v1

let length v1 =
  sqrt (length_square v1)

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
  (mult v1 (1. -. t)) +| (mult v2 t)

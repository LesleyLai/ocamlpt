open Base
open Stdio

open Ocamlpt

open Vec3

let ray_color (r: Ray.t) =
  let sphere = Sphere.create (Vec3.create 0. 0. (-1.)) 0.5 in
  match Sphere.hit r sphere with
  | Some hit_record ->
    (hit_record.normal +| (Vec3.create 1. 1. 1.)) /| 2.
  | None ->
    let unit_direction = r.direction in
    let t = 0.5 *. (unit_direction.y +. 1.0) in
    Vec3.lerp (Vec3.create 1. 1. 1.) (Vec3.create 0.5 0.7 1.0) t

let color_255_from_float f =
  Float.to_int(255.999 *. f)

let () =
  let width = 200
  and height = 100 in
  let lower_left_corner = Vec3.create (-2.0) (-1.0) (-1.0)
  and horizontal = Vec3.create 4.0 0.0 0.0
  and vertical = Vec3.create 0.0 2.0 0.0
  and origin = Vec3.create 0.0 0.0 0.0 in
  let file = Out_channel.create "image.ppm" in
  let _ = Out_channel.fprintf file "P3\n%d %d\n255\n" width height in
  let _ =
  (Sequence.cartesian_product
     (Sequence.range ~stride:(-1) ~stop:`inclusive (height-1) 0)
     (Sequence.range 0 width)
  )
  |> Sequence.iter
    ~f:(fun (j, i) ->
        let u = Float.of_int(i) /. Float.of_int(width)
        and v = Float.of_int(j) /. Float.of_int(height) in
        let r = Ray.create origin
            (lower_left_corner +| (mult horizontal u) +| (mult vertical v)) in
        let color = ray_color r in
        let ir = color_255_from_float color.x
        and ig = color_255_from_float color.y
        and ib = color_255_from_float color.z in
        Out_channel.fprintf file "%d %d %d\n" ir ig ib) in
  printf "Done\n"

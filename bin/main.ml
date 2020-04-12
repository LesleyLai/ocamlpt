open Base
open Stdio

open Ocamlpt

open Vec3

let sphere1 = Sphere.create (Vec3.create 0. 0. (-1.)) 0.5
and sphere2 = Sphere.create (Vec3.create 0. (-100.5) (-1.)) 100.

let scene = Scene.create |> Scene.add sphere1 |> Scene.add sphere2

let random_unit_vector () =
  let open Float in
  let a = Random.float_range 0. (2. * pi)
  and z = Random.float_range (-1.) 1. in
  let r = sqrt(1. - z * z) in
  Vec3.create (r * cos(a)) (r * sin(a)) z

let to_gamma_space (color: Vec3.t) =
  let open Float in
  let gamma_inv = 1. / 2.2 in
  Vec3.create (color.x ** gamma_inv) (color.y ** gamma_inv) (color.z ** gamma_inv)

let ray_color (r: Ray.t) =
  let max_depth = 50 in
  let rec helper r depth =
    match Scene.hit r scene with
    | Some hit_record ->
      if depth >= max_depth then
        Vec3.zero
      else
        let target = hit_record.p +| hit_record.normal +| random_unit_vector() in
        Vec3.mult (helper (Ray.create hit_record.p (target -| hit_record.p)) (depth + 1)) 0.5
    | None ->
      let unit_direction = r.direction in
      let t = 0.5 *. (unit_direction.y +. 1.0) in
      Vec3.lerp (Vec3.create 1. 1. 1.) (Vec3.create 0.5 0.7 1.0) t in
  helper r 0

let color_255_from_float f =
  Float.to_int(255.999 *. f)

let () =
  let width = 200
  and height = 100
  and sample_per_pixel = 100 in
  let file = Out_channel.create "image.ppm" in
  let _ = Out_channel.fprintf file "P3\n%d %d\n255\n" width height in
  let _ =
  (Sequence.cartesian_product
     (Sequence.range ~stride:(-1) ~stop:`inclusive (height-1) 0)
     (Sequence.range 0 width)
  )
  |> Sequence.iter
    ~f:(fun (j, i) ->
        let color_acc = (Sequence.range 0 sample_per_pixel)
        |> Sequence.fold ~init:(Vec3.zero) ~f:(fun acc _ ->
            let u = (Float.of_int(i) +. Random.float(1.)) /. Float.of_int(width)
            and v = (Float.of_int(j) +. Random.float(1.)) /. Float.of_int(height) in
            let r = Camera.get_ray u v in
            ray_color r +| acc
                          ) in
        let color = color_acc /| (Int.to_float sample_per_pixel) |> to_gamma_space in
        let ir = color_255_from_float (Float.clamp_exn color.x ~min:0.0 ~max:1.0)
        and ig = color_255_from_float (Float.clamp_exn color.y ~min:0.0 ~max:1.0)
        and ib = color_255_from_float (Float.clamp_exn color.z ~min:0.0 ~max:1.0) in
        Out_channel.fprintf file "%d %d %d\n" ir ig ib) in
  printf "Done\n"

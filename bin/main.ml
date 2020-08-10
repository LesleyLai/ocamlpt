open Core
open Stdio

open Ocamlpt

open Vec3

let random_vec () =
  Vec3.create (Random.float 1.) (Random.float 1.) (Random.float 1.)

let scene =
  let open Float in
  let open Scene.Builder in
  let sphere1 = Sphere.create (Vec3.create 0. (-1000.) 0.) 1000.
      (Material.Lambertian { albedo = (Vec3.create 0.5 0.5 0.5) })
  and sphere2 = Sphere.create (Vec3.create 0. 1. 0.) 1.
      (Material.Dielectric { ref_index = 1.5 })
  and sphere3 = Sphere.create (Vec3.create (-4.) 1. 0.) 1.0
      (Material.Lambertian { albedo = (Vec3.create 0.4 0.2 0.1) })
  and sphere4 = Sphere.create (Vec3.create 4. 1. 0.) 1.0
      (Material.Metal { albedo = (Vec3.create 0.7 0.6 0.5); fuzzness = 0.0 }) in
  (Sequence.cartesian_product
     (Sequence.range (-11) 11)
     (Sequence.range (-11) 11)
  )|> Sequence.fold
    ~init:(create
           |> add (module Sphere) sphere1 |> add (module Sphere) sphere2
           |> add (module Sphere) sphere3 |> add (module Sphere) sphere4)
    ~f:(fun acc (b, a) ->
        let choose_mat = Random.float 1. in
        let center = Vec3.create
            (Int.to_float(a) +. 0.9 *. (Random.float 1.))
            0.2
            (Int.to_float(b) +. 0.9 *. (Random.float 1.)) in
        if ((Vec3.length (center -| (Vec3.create 4. 0.2 0.0))) > 0.9) then
          if choose_mat < 0.8 then
            let albedo =
              Vec3.elem_wise_product (random_vec()) (random_vec()) in
            acc |> add (module Sphere)
              (Sphere.create center 0.2 (Material.Lambertian { albedo }))
          else if choose_mat < 0.95 then
            let albedo = Vec3.create
                (Random.float_range 0.5 1.)
                (Random.float_range 0.5 1.)
                (Random.float_range 0.5 1.) in
            let fuzzness = Random.float_range 0. 0.5 in
            let center2 = center +| (Vec3.create 0. (Random.float 0.5) 0.) in
            acc |> add (module Moving_sphere)
              (Moving_sphere.create center center2 0.0 1.0 0.2 (Material.Metal { albedo; fuzzness }))
          else
            acc |> add (module Sphere)
              (Sphere.create center 0.2 (Material.Dielectric { ref_index = 1.5 }))
        else
          acc
      )
  |> build

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
        begin match Material.scatter r hit_record hit_record.material with
          | Some { scattered; attenuation } ->
            Vec3.elem_wise_product (helper scattered (depth + 1)) attenuation
          | None -> Vec3.zero
        end
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
  let lookfrom = Vec3.create 13. 2. 3.
  and lookat = Vec3.create 0. 0. 0.
  in
  let camera =
    Camera.create
      ~time0:0.
      ~time1:1.
      ~lookfrom:lookfrom
      ~lookat:lookat
      ~vup:(Vec3.create 0. 1. 0.)
      ~fovy:(Float.pi /. 2.)
      ~aspect_ratio:(Float.of_int(width) /. Float.of_int(height))
      ~aperture:0.1
      ~focus_dist:10.
      ()
  in
  let file = Out_channel.create "image.ppm" in
  let _ = Out_channel.fprintf file "P3\n%d %d\n255\n" width height in
  let starting_time = Unix.gettimeofday () in
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
            let r = camera |> Camera.get_ray u v in
            ray_color r +| acc
                          ) in
        let color = color_acc /| (Int.to_float sample_per_pixel) |> to_gamma_space in
        let ir = color_255_from_float (Float.clamp_exn color.x ~min:0.0 ~max:1.0)
        and ig = color_255_from_float (Float.clamp_exn color.y ~min:0.0 ~max:1.0)
        and ib = color_255_from_float (Float.clamp_exn color.z ~min:0.0 ~max:1.0) in
        Out_channel.fprintf file "%d %d %d\n" ir ig ib) in
  printf "Execution time: %fs\n" ((Unix.gettimeofday()) -. starting_time)

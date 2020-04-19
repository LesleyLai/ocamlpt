open Base
open Vec3

type t =
  | Lambertian of { albedo: Vec3.t }
  | Metal of { albedo: Vec3.t; fuzzness: float }
  | Dielectric of { ref_index: float }

type face_direction =
  | FrontFace
  | BackFace

type hit_record = {t: float; p: Vec3.t; normal: Vec3.t; material: t;
                   face_direction: face_direction}

let random_unit_vector () =
  let open Float in
  let a = Random.float_range 0. (2. * pi)
  and z = Random.float_range (-1.) 1. in
  let r = sqrt(1. - z * z) in
  Vec3.create (r * cos(a)) (r * sin(a)) z

type scatter_result = { scattered: Ray.t; attenuation: Vec3.t }

let scatter (r: Ray.t) (hit_record: hit_record) =
  let open Float in
  function
  | Lambertian { albedo } ->
    let scatter_direction = hit_record.normal +| random_unit_vector() in
    let scattered = Ray.create hit_record.p scatter_direction
    and attenuation = albedo in
    Some { scattered; attenuation }

  | Metal { albedo; fuzzness } ->
    let reflected = reflect (normalize r.direction) hit_record.normal in
    let scattered = Ray.create hit_record.p (reflected +| fuzzness *| random_unit_vector())
    and attenuation = albedo in
    Option.some_if ((Vec3.dot scattered.direction hit_record.normal) > 0.)
      { scattered; attenuation }

  | Dielectric { ref_index } ->
    let attenuation = Vec3.create 1.0 1.0 1.0 in
    let etai_over_etat =
        begin match hit_record.face_direction with
          | FrontFace -> 1.0 /. ref_index
          | BackFace -> ref_index
        end
    in
    let unit_direction = Vec3.normalize r.direction in
    let cos_theta = min (dot (negate unit_direction) hit_record.normal) 1.0 in
    let sin_theta = sqrt (1.0 - cos_theta * cos_theta) in
    if etai_over_etat * sin_theta > 1.0 then
      (* Total internal reflection, must reflect *)
      let reflected = reflect unit_direction hit_record.normal in
      let scattered = Ray.create hit_record.p reflected in
      Some { scattered; attenuation }
    else
      (* Can refract *)
      let refracted = refract unit_direction hit_record.normal etai_over_etat in
      let scattered = Ray.create hit_record.p refracted in
      Some { scattered; attenuation }

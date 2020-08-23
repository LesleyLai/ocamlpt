type t = u:float -> v:float -> p:Vec3.t -> Vec3.t

let solid_color (color: Vec3.t):t =
  (fun ~u:_ ~v:_ ~p:_ -> color)

let checker (even: t) (odd: t):t =
  let open Float in
  fun ~u ~v ~p ->
    let sines = sin(10.*.p.x) *. sin(10.*.p.y) *. sin(10.*.p.z) in
    if (sines < 0.) then
      odd ~u:u ~v:v ~p:p
    else
      even ~u:u ~v:v ~p:p

let noise:t =
  let open Vec3 in
  let perlin = Perlin.create () in
  fun ~u:_ ~v:_ ~p ->
  (Perlin.noise p perlin) *| Vec3.create 1. 1. 1.

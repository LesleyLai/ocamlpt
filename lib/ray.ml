open Vec3

type t = {origin: Vec3.t; direction: Vec3.t}

let create origin direction = {origin; direction}

let at (t: float) ({origin; direction}: t) : Vec3.t =
  origin +| (t *| direction)

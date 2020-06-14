open Vec3

type t = {origin: Vec3.t; direction: Vec3.t; time: float}

let create origin direction time = {origin; direction; time}

let at (t: float) ({origin; direction; _}: t) : Vec3.t =
  origin +| (t *| direction)

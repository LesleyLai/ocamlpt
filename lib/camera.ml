open Vec3

let lower_left_corner = Vec3.create (-2.0) (-1.0) (-1.0)
and horizontal = Vec3.create 4.0 0.0 0.0
and vertical = Vec3.create 0.0 2.0 0.0
and origin = Vec3.create 0.0 0.0 0.0

let get_ray u v = Ray.create origin
    (lower_left_corner +| (u *| horizontal) +| (v *| vertical))

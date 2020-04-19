open Base

type t = Sphere.t list

let create: t =
  []

let add (s: Sphere.t) (scene: t): t =
  s :: scene

let hit (r: Ray.t) (lst: t): Material.hit_record option =
  lst |>
  List.map ~f:(Sphere.hit r) |>
  List.bind ~f:Option.to_list |>
  List.min_elt~compare:(fun (rec_1: Material.hit_record) rec_2 ->
      Float.compare rec_1.t rec_2.t
    )

open Base

module type Shape = sig
  type t
  val hit: Ray.t -> t -> Material.hit_record option
end

module type Shape_instance = sig
  module S: Shape
  val this: S.t
end

let build_shape
    (type a)
    (module S : Shape with type t = a)
    (shape: a)
  =
  (module struct
    module S = S
    let this = shape
  end : Shape_instance
  )

type t = (module Shape_instance) list

let create: t =
  []

let add (type a) (module S: Shape with type t = a) (shape: a) (scene: t): t =
  (build_shape (module S) shape) :: scene

let hit (r: Ray.t) (lst: t): Material.hit_record option =
  lst |>
  List.map ~f:(fun shape ->
      let module Instance = (val shape: Shape_instance) in
      Instance.S.hit r Instance.this
    ) |>
  List.bind ~f:Option.to_list |>
  List.min_elt~compare:(fun (rec_1: Material.hit_record) rec_2 ->
      Float.compare rec_1.t rec_2.t
    )

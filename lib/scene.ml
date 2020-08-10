open Base

module type Shape = sig
  type t
  val hit: Ray.t -> t -> Material.hit_record option
  val bounding_box: t -> Aabb.t
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

type t = { root: (module Shape_instance) }

let hit (r: Ray.t) (scene: t): Material.hit_record option =
  let module RootInstance = (val scene.root: Shape_instance) in
  RootInstance.S.hit r RootInstance.this

module rec Bvh_node : sig
  include Shape
  val create: (module Shape_instance) list -> t
end = struct

type t = {
  left: (module Shape_instance);
  right: (module Shape_instance);
  aabb: Aabb.t;
}

let bounding_box (node: t) =
  node.aabb

let hit (r: Ray.t) (node: t): Material.hit_record option =
  let open Float in
  if Aabb.hit r 0.00001 Float.max_value node.aabb then
      let module LhsInstance = (val node.left: Shape_instance) in
      let module RhsInstance = (val node.right: Shape_instance) in
      let lhs_result = LhsInstance.S.hit r LhsInstance.this in
      let rhs_result = RhsInstance.S.hit r RhsInstance.this in
      match (lhs_result, rhs_result) with
      | (Some lhs, Some rhs) ->
        if lhs.t < rhs.t then lhs_result else rhs_result
      | (Some _, _) -> lhs_result
      | (_, Some _) -> rhs_result
      | _ -> None
  else
    None

let rec create (shapes: (module Shape_instance) list) =
  let axis_index = Random.int 3 in
  let get_axis =
    match axis_index with
    | 0 -> (fun (pt: Vec3.t) -> pt.x)
    | 1 -> (fun (pt: Vec3.t) -> pt.y)
    | _ -> (fun (pt: Vec3.t) -> pt.z)
  in
  let compare_shape_by_axis lhs rhs =
    let module LhsInstance = (val lhs: Shape_instance) in
    let module RhsInstance = (val rhs: Shape_instance) in
    let left_box = LhsInstance.S.bounding_box LhsInstance.this in
    let right_box = RhsInstance.S.bounding_box RhsInstance.this in
    Float.compare (get_axis left_box.min) (get_axis right_box.min)
  in
  let get_aabb shape =
    let module Instance = (val shape: Shape_instance) in
    Instance.S.bounding_box Instance.this
  in
  match shapes with
  | [] -> failwith "Cannot give bvh_node an empty list of shapes"
  | elm :: [] -> { left=elm; right=elm; aabb= get_aabb elm}
  | elm1 :: elm2 :: [] ->
    let aabb = Aabb.union (get_aabb elm1) (get_aabb elm2) in
    if compare_shape_by_axis elm1 elm2 < 0 then
      { left=elm1; right=elm2; aabb }
    else
      { left=elm2; right=elm1; aabb }
  | _ ->
    let sorted_shapes = shapes |>
    List.sort ~compare:compare_shape_by_axis in
    let (first_half, second_half) =
      List.split_n sorted_shapes ((List.length sorted_shapes) / 2) in
    let left = create first_half
    and right = create second_half in
    let aabb = Aabb.union left.aabb right.aabb in
    { left=(build_shape (module Bvh_node) left);
      right=(build_shape (module Bvh_node) right);
      aabb }
end

module Builder =
struct
  type t = (module Shape_instance) list

  let create: t =
    []

  let add (type a) (module S: Shape with type t = a) (shape: a) (scene: t): t =
    (build_shape (module S) shape) :: scene

  let build (shapes: t) =
    { root = build_shape (module Bvh_node) (Bvh_node.create (shapes))}
end

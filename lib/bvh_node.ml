open Base

type t = {
  left: (module Scene.Shape_instance);
  right: (module Scene.Shape_instance);
  aabb: Aabb.t;
}

let bounding_box (node: t) =
  node.aabb

let hit (_: Ray.t) (_: t): Material.hit_record option =
  None


let rec create (shapes: (module Scene.Shape_instance) list) =
  let axis_index = Random.int 3 in
  let get_axis =
    match axis_index with
    | 0 -> (fun (pt: Vec3.t) -> pt.x)
    | 1 -> (fun (pt: Vec3.t) -> pt.y)
    | 2 -> (fun (pt: Vec3.t) -> pt.z)
    | _ -> failwith "Bug"
  in
  let compare_shape_by_axis lhs rhs =
    let module LhsInstance = (val lhs: Scene.Shape_instance) in
    let module RhsInstance = (val rhs: Scene.Shape_instance) in
    let left_box = LhsInstance.S.bounding_box LhsInstance.this in
    let right_box = RhsInstance.S.bounding_box RhsInstance.this in
    Float.compare (get_axis left_box.min) (get_axis right_box.min)
  in
  let get_aabb shape =
    let module Instance = (val shape: Scene.Shape_instance) in
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
    { left=(Scene.build_shape (module Bvh_node) left);
      right=(Scene.build_shape (module Bvh_node) right);
      aabb }

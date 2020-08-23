open Base

let points_count = 256

type t = {
  ranfloat: float array;
  perm_x: int array;
  perm_y: int array;
  perm_z: int array;
}

let permute p n =
  Sequence.range ~start:`inclusive ~stop:`exclusive (n-1) 0
  |> Sequence.iter ~f:(fun i ->
      let target = Random.int i in
      Array.swap p i target
    )

let perlin_generate_perm () =
  let p = Array.create ~len:points_count 0 in
  let () = Sequence.range 0 points_count
  |> Sequence.iter ~f:(fun i ->
               let () = Array.unsafe_set p i i in
               permute p points_count
             ) in
  p

let create () =
  let ranfloat =
    Sequence.range 0 points_count
    |> Sequence.map ~f:(fun _ -> Random.float 1.)
    |> Sequence.to_array
  and perm_x = perlin_generate_perm ()
  and perm_y = perlin_generate_perm ()
  and perm_z = perlin_generate_perm ()
  in
  { ranfloat; perm_x; perm_y; perm_z }

let noise (p: Vec3.t) ({ranfloat; perm_x; perm_y; perm_z}: t): float =
  let open Float in
  (* let u = p.x - (round_down p.x)
   * and v = p.y - (round_down p.y)
   * and w = p.z - (round_down p.z)
   * in *)
  let i = Int.bit_and (to_int (4. * p.x)) 255
  and j = Int.bit_and (to_int (4. * p.y)) 255
  and k = Int.bit_and (to_int (4. * p.z)) 255
  in
  (Array.unsafe_get ranfloat
     (Int.bit_xor
        (Int.bit_xor
           (Array.unsafe_get perm_x i)
           (Array.unsafe_get perm_y j))
        (Array.unsafe_get perm_z k)))

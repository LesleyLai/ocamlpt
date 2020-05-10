open OUnit2
open Ocamlpt
open Ocamlpt.Vec3

let v1 = Vec3.create 1. 2. 3.
let v2 = Vec3.create 2. 4. 5.

let r = Ray.create v1 (Vec3.create 1. 0. 0.)

let dummy_mat = Material.Lambertian { albedo = (Vec3.create 0. 0. 0.) }

let sphere = Sphere.create (Vec3.create 0. 0. (-1.)) 0.5 dummy_mat
let sphere2 = Sphere.create (Vec3.create 0. 0. (-3.)) 0.5 dummy_mat
let scene = Scene.create |> Scene.add sphere |> Scene.add sphere2

let tests = "test suite" >::: [
    "Vec3 creating "  >:: (fun _ ->
        assert_equal v1.x 1.;
        assert_equal v1.y 2.;
        assert_equal v1.z 3.
      );

    "Vec3 negation "  >:: (fun _ ->
        assert_equal (Vec3.negate v1) (Vec3.create (-1.) (-2.) (-3.))
      );

    "Vec3 + "  >:: (fun _ ->
        assert_equal (v1 +| v2) (Vec3.create 3. 6. 8.)
      );

    "Vec3 - "  >:: (fun _ ->
        assert_equal (v1 -| v2) (Vec3.create (-1.) (-2.) (-2.))
      );

    "Vec3 * "  >:: (fun _ ->
        assert_equal (2. *| v1) (Vec3.create 2. 4. 6.)
      );

    "Vec3 / "  >:: (fun _ ->
        assert_equal (v1 /| 2.) (Vec3.create 0.5 1. 1.5)
      );

    "Vec3 dot" >:: (fun _ ->
        assert_equal (Vec3.dot v1 v2) 25.
      );

    "Vec3 length_square" >:: (fun _ ->
        assert_equal (Vec3.length_square v1) 14.
      );

    "Vec3 length" >:: (fun _ ->
        assert_equal (Vec3.length v1) (sqrt 14.)
      );

    "Vec3 cross" >:: (fun _ ->
        assert_equal (Vec3.cross v1 v2) (Vec3.create (-2.) 1. 0.)
      );

    "Vec3 normalize" >:: (fun _ ->
        assert_equal (Vec3.normalize v1)
          (Vec3.create (1. /. (sqrt 14.)) (2. /. (sqrt 14.)) (3. /. (sqrt 14.)))
      );

    "Vec3 leap" >:: (fun _ ->
        assert_equal (Vec3.lerp v1 v2 0.5) (Vec3.create 1.5 3. 4.)
      );

    "Vec3 component-wise product" >:: (fun _ ->
        assert_equal (Vec3.elem_wise_product v1 v2) (Vec3.create 2. 8. 15.)
      );

    "Vec3 reflect" >:: (fun _ ->
        assert_equal
          (Vec3.reflect (Vec3.create 1. 1. 0.) (Vec3.create 0. 1. 0.))
          (Vec3.create 1. (-1.) 0.)
      );

    "Vec3 refract" >:: (fun _ ->
        assert_equal
          (Vec3.refract (Vec3.create 0. 1. 0.) (Vec3.create 0. 1. 0.) 1.5)
          (Vec3.create 0. (-1.) 0.)
          ~printer:Vec3.to_string
      );

    "Ray at" >:: (fun _ ->
        assert_equal (r |> Ray.at 2.) (Vec3.create 3. 2. 3.)
      );

    "Sphere intersection intersect" >:: (fun _ ->
        assert_equal (Sphere.hit
                        (Ray.create
                           (Vec3.create 0. 0. 0.)
                           (Vec3.create 0. 0. (-1.))
                        ) sphere)
          (Some {t=0.5;
                 p=(Vec3.create 0. 0. (-0.5));
                 normal=(Vec3.create 0. 0. 1.);
                 material=dummy_mat;
                 face_direction=Material.FrontFace})
      );

    "Sphere intersection intersect inside" >:: (fun _ ->
        assert_equal (Sphere.hit
                        (Ray.create
                           (Vec3.create 0. 0. (-1.))
                           (Vec3.create 0. 0. (-1.))
                        ) sphere)
          (Some {t=0.5;
                 p=(Vec3.create 0. 0. (-1.5));
                 normal=(Vec3.create 0. 0. 1.);
                 material=dummy_mat;
                 face_direction=Material.BackFace})
      );

    "Sphere intersection out" >:: (fun _ ->
        assert_equal (Sphere.hit
                        (Ray.create
                           (Vec3.create 0. 1. 0.)
                           (Vec3.create 0. 0. (-1.))
                        ) sphere) None
      );

    "Sphere intersection reverse direction" >:: (fun _ ->
        assert_equal (Sphere.hit
                        (Ray.create
                           (Vec3.create 0. 0. 0.)
                           (Vec3.create 0. 0. 1.)
                        ) sphere) None
      );

    "Ray-Scene interscetion: first object" >:: (fun _ ->
        assert_equal (Scene.hit
                        (Ray.create
                           (Vec3.create 0. 0. 0.)
                           (Vec3.create 0. 0. (-1.))
                        ) scene)
          (Some {t=0.5;
                 p=(Vec3.create 0. 0. (-0.5));
                 normal=(Vec3.create 0. 0. 1.);
                 material=dummy_mat;
                 face_direction=Material.FrontFace
                })
      );

    "Ray-Scene interscetion: second object" >:: (fun _ ->
        assert_equal (Scene.hit
                        (Ray.create
                           (Vec3.create 0. 0. (-5.))
                           (Vec3.create 0. 0. 1.)
                        ) scene)
          (Some {t=1.5;
                 p=(Vec3.create 0. 0. (-3.5));
                 normal=(Vec3.create 0. 0. (-1.));
                 material=dummy_mat;
                 face_direction=Material.FrontFace})
      );

    "Ray-Scene interscetion: miss" >:: (fun _ ->
        assert_equal (Scene.hit
                        (Ray.create
                           (Vec3.create 0. 2. (-5.))
                           (Vec3.create 0. 0. 1.)
                        ) scene)
          None
      );

    "or_else None" >:: (fun _ ->
        assert_equal (Option_ext.or_else ~f:(fun () -> Some 1) None) (Some 1)
      );

    "or_else Some" >:: (fun _ ->
        assert_equal (Option_ext.or_else ~f:(fun () -> Some 1) (Some 42)) (Some 42)
      );
]

let _ = run_test_tt_main tests

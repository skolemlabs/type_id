let () =
  let open Alcotest in
  run "type_id"
    [
      ( "Basic functionality",
        let module Subject : sig
          (* These types are equal, but they're also opaque. *)
          type a
          type b

          (* This type is not equivalent to the above types. *)
          type c

          val a_id : a Type_id.t
          val b_id : b Type_id.t
          val b_id_2 : b Type_id.t
          val c_id : c Type_id.t
        end = struct
          type a = string
          type b = string
          type c = int

          let a_id = Type_id.make ~label:"string"
          let b_id = a_id
          let b_id_2 = Type_id.make ~label:"string-2"
          let c_id = Type_id.make ~label:"int"
        end in
        [
          ( test_case "Type IDs are labelled" `Quick @@ fun () ->
            (* Act *)
            let actual = Type_id.label Subject.a_id in

            (* Assert *)
            let () =
              let expected = "string" in
              check string "labels are equal" expected actual
            in
            () );
          (*
           *
           *)
          ( test_case "Equivalent type IDs can produce a type equality proof"
              `Quick
          @@ fun () ->
            (* Act *)
            let actual = Type_id.eq Subject.a_id Subject.b_id in

            (* Assert *)
            let () = check bool "is Some" true (Option.is_some actual) in
            () );
          (*
           *
           *)
          ( test_case "Different types cannot produce a type equality proof"
              `Quick
          @@ fun () ->
            (* Act *)
            let actual = Type_id.eq Subject.a_id Subject.c_id in

            (* Assert *)
            let () = check bool "is None" true (Option.is_none actual) in
            () );
          (*
           *
           *)
          ( test_case "Different type IDs cannot produce a type equality proof"
              `Quick
          @@ fun () ->
            (* Act *)
            let actual = Type_id.eq Subject.b_id Subject.b_id_2 in

            (* Assert *)
            let () = check bool "is None" true (Option.is_none actual) in
            () );
          (*
           *
           *)
          ( test_case "Type IDs of different types return Ids_not_equal error"
              `Quick
          @@ fun () ->
            (* Act *)
            let actual =
              match Type_id.eq_res Subject.b_id Subject.c_id with
              | Ok _ -> Ok ()
              | Error e -> Error e
            in

            (* Assert *)
            let () =
              let expected =
                Error (Type_id.Error.Ids_not_equal "(string, int)")
              in
              check
                (result unit Type_id.Error.(testable pp equal))
                "is equal" expected actual
            in
            () );
        ] );
    ]

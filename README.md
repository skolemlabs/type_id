# type_id

## What does this library do?

`type_id` provides a type -- `Type_id.t` -- which can be used to reference and
identify a type. This can then be used to construct _type proofs_ between two
types which share the same type ID, extending the usefulness of the `type_eq`
library.

```ocaml
module Types : sig
  (* These types are equal, but they're also opaque. *)
  type a
  type b

  (* This type is not equivalent to the above types. *)
  type c

  val a_of_int: int -> a
  val b_of_int: int -> b
  val c_of_int: int -> c

  val a_id : a Type_id.t
  val b_id : b Type_id.t
  val b_id_2 : b Type_id.t
  val c_id : c Type_id.t
end = struct
  type a = string
  type b = string
  type c = int

  let a_of_int = string_of_int
  let b_of_int = string_of_int
  let c_of_int x = x

  let a_id = Type_id.make ~label:"string"
  let b_id = a_id
  let b_id_2 = Type_id.make ~label:"string-2"
  let c_id = Type_id.make ~label:"int"
end

(* These types are different... *)
let a: Types.a = Types.a_of_int 123
let b: Types.b = Types.b_of_int 456

(* But, we can cast between them using their type IDs: *)
let a': Types.a =
  match Type_id.eq_res Types.a_id Types.b_id with
    | Error e -> failwith (* ... *)
    | Ok eq_a_b -> Type_eq.coerce eq_a_b b
```

## Why is this library useful?

Sometimes you'll be working with types which are opaque -- as in the example
above. This utility can be used to determine if two opaque types are indeed
equal, which can allow you to cast between them, among other operations.

See the `.mli` file for a fully-documented interface.

Note that a stripped-down version of this utility is included as `Type.eq` in
OCaml 5+: https://github.com/ocaml/ocaml/blob/c2d00ef67b4af1e6ba90e77e4106770bbdd88a01/stdlib/type.ml#L18

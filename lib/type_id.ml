module Error = struct
  type t = Ids_not_equal of string

  let equal (a : t) (b : t) : bool =
    match (a, b) with Ids_not_equal a, Ids_not_equal b -> a = b

  let pp (f : Format.formatter) (t : t) : unit =
    match t with
    | Ids_not_equal s ->
        Format.fprintf f "IDs do not reference an equivalent type: %s" s
end

type _ id = ..

module type T = sig
  type t
  type _ id += Unique : t id

  val label : string
end

module Make (M : sig
  type t

  val label : string
end) : T with type t = M.t = struct
  type t = M.t
  type _ id += Unique : t id

  let label = M.label
end

type 'a t = (module T with type t = 'a)

let make (type a) ~label : a t =
  (module Make (struct
    type t = a

    let label = label
  end) : T
    with type t = a)

let label (type a) (module M : T with type t = a) = M.label

let eq (type a b) (module A : T with type t = a) (module B : T with type t = b)
    : (a, b) Type_eq.t option =
  match A.Unique with B.Unique -> Some Type_eq.Eq | _ -> None

let eq_res (type a b) (module A : T with type t = a)
    (module B : T with type t = b) : ((a, b) Type_eq.t, Error.t) result =
  match eq (module A) (module B) with
  | Some x -> Ok x
  | None -> Error (Ids_not_equal (Format.sprintf "(%s, %s)" A.label B.label))

type 'a t
(** Serves as a unique identifier for some type ['a]. *)

(** Types of errors produced by this module. *)
module Error : sig
  (** The errors that this module may produce. *)
  type t =
    | Ids_not_equal of string
        (** Produced by [eq_res] if its arguments do not identify an equivalent type. More details \
            are contained in the string error message. *)

  val equal : t -> t -> bool
  (** [equal a b] checks if [a] and [b] are exactly equal. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] is a pretty-printer for error types. *)
end

val make : label:string -> 'a t
(** [make ~label] creates an identifier with [label] for some type [t] *)

val label : 'a t -> string
(** [label id] gets the label of a given [t], which was given to [make] during construction. *)

val eq : 'a t -> 'b t -> ('a, 'b) Type_eq.t option
(** [eq t_a t_b] produces an equality proof if [t_a] and [t_b] are the same ID *)

val eq_res : 'a t -> 'b t -> (('a, 'b) Type_eq.t, Error.t) result
(** Similar to [!eq] except returns a [result] *)

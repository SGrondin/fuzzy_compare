open! Core_kernel

type t [@@deriving sexp]

val create : max_edits:int -> t

module type Intf = sig
  (** The type being compared *)
  type t

  (** Type of the individual parts *)
  type cmp [@@deriving equal]

  (** Type optimized for quick access *)
  type index

  (** Convert from [t] to [index] *)
  val index : t -> index

  (** Length of [index] *)
  val length : index -> int

  (** Quick access into [index] *)
  val get : index -> int -> cmp

  (** Fold over [index] *)
  val fold : index -> init:'a -> f:('a -> cmp -> 'a) -> 'a
end

module type S = sig
  type u

  val eval : t -> u -> u -> bool
end

module Make : functor (M : Intf) -> S with type u := M.t

module String : S with type u := string

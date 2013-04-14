(** Randomised Property Checking *)

(**{1 Randomised Property Checking}

   FIXME: intro

   FIXME: explain relationship between Generators and Domains.
*)

(** Generators for random test data. *)
module Generator : sig
  type 'a t

  val return : 'a -> 'a t
  val bind   : 'a t -> ('a -> 'b t) -> 'b t

  val lift : ('a -> 'b) -> 'a t -> 'b t
  val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  val bits : int t
  val int : int -> int t
  val int32 : int32 -> int32 t
  val nativeint : nativeint -> nativeint t
  val int64 : int64 -> int64 t
  val float : float -> float t
  val bool : bool t
end

(** Domains of randomly generated test data. *)
module Domain : sig
  type 'a t

  val make :
    generate:('a Generator.t) ->
    ?shrink:('a -> 'a list) ->
    to_string:('a -> string) ->
    description:string ->
    unit ->
    'a t

  val generator   : 'a t -> 'a Generator.t
  val shrinker    : 'a t -> 'a -> 'a list
  val description : 'a t -> string
  val to_string   : 'a t-> 'a -> string
end

(** Universally quantified properties for randomised testing. *)
module Property : sig
  type t

  val forall : 'a Domain.t -> ('a -> t) -> t

  val check : bool -> t

  val check_equal : to_string:('a -> string) -> 'a -> 'a -> t

  val check_raises : exn -> (unit -> 'a) -> t

  (** Tests the given property 10000 times with random data for each
      universally quantified variable. If a counterexample is
      discovered, then an attempt to shrink it to produce a smaller
      refutation of the property. If no counterexample is found, then
      [`OkAsFarAsIKnow] is returned. *)
  val test_property : t ->
    [ `OkAsFarAsIKnow
    | `CounterExample of ((string * string) list * string) ]

  (** Turns a property into an OUnit test case. {!check_property} is
      invoked on the property. If a counterexample is discovered then the
      test fails. *)
  val test_of_property : t -> unit -> unit

  (** A useful combinator for writing properties. *)
  val ( ^$ ) : ('a -> 'b) -> 'a -> 'b
end

(** Domains for some standard types. *)
module PrimitiveDomains : sig
  val unit : unit Domain.t

  val bool : bool Domain.t

  val int_range : int -> int -> int Domain.t

  val float_range : float -> float -> float Domain.t

  val list : 'a Domain.t -> 'a list Domain.t

  val char : char Domain.t

  val printable_ascii_char : char Domain.t

  val string : string Domain.t

  val printable_ascii_string : string Domain.t

  val array : 'a Domain.t -> 'a array Domain.t

  val option : 'a Domain.t -> 'a option Domain.t
end

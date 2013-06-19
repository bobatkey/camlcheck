(** Randomised Property Checking *)

(**{1 Randomised Property Checking}

   FIXME: intro
*)

(** Generators for random test data. *)
module Generator : sig
  type 'a t

  val return : 'a -> 'a t
  val (>>=)  : 'a t -> ('a -> 'b t) -> 'b t
  val (<$>)  : ('a -> 'b) -> 'a t -> 'b t
  val (<*>)  : ('a -> 'b) t -> 'a t -> 'b t


  val bits : int t
  val int : int -> int t
  val int32 : int32 -> int32 t
  val nativeint : nativeint -> nativeint t
  val int64 : int64 -> int64 t
  val float : float -> float t
  val bool : bool t
  val array : int -> (int -> 'a t) -> 'a array t

  val run : 'a t -> Random.State.t -> 'a
end

(** Domains of randomly generated test data. *)
module Arbitrary : sig
  type 'a t

  (** Construct a new domain of arbitrary values. *)
  val make :
    generator:('a Generator.t) ->
    ?shrink:('a -> 'a list) ->
    to_string:('a -> string) ->
    description:string ->
    unit ->
    'a t

  (** {2 Accessing parts of an {!Arbitrary.t} domain. *)

  (** Get the underlying generator of a domain. *)
  val to_generator   : 'a t -> 'a Generator.t

  (** Get the shrinking function associated with a domain. *)
  val to_shrinker    : 'a t -> 'a -> 'a list

  (** Get the human-readable description of a domain. *)
  val to_description : 'a t -> string

  (** Get the [to_string] function associated with a domain. *)
  val to_string   : 'a t-> 'a -> string

  (**{2 Arbitrary values for some primitive types} *)

  (** The domain of {!unit} values. Always picks [()], obviously. *)
  val unit : unit t

  (** The domain of {!bool} values. Uniformly selects between [true]
      and [false]. *)
  val bool : bool t


  val int_range : int -> int -> int t

  val float_range : float -> float -> float t

  val list : 'a t -> 'a list t

  val char : char t

  val printable_ascii_char : char t

  val string : string t

  val printable_ascii_string : string t

  val array : 'a t -> 'a array t

  val option : 'a t -> 'a option t
end

(** Universally quantified properties for randomised testing. *)
module Property : sig
  (** The type of properties checkable with randomised testing. *)
  type t

  (** Construct a universally quantified property over a given
      {!Arbitrary.t} domain. *)
  val forall : 'a Arbitrary.t -> ('a -> t) -> t

  (** Property that is true if the given {!bool} is true. *)
  val true_that : bool -> t

  (** Atomic property of two values being equal. The [~to_string]
      argument is used for printing out values to report back in the
      case when the equality check fails. The optional [~cmp] argument
      is used to compare the two arguments. If it is not present, then
      {!Pervasives.(=)} is used. *)
  val equal : to_string:('a -> string) -> ?cmp:('a -> 'a -> bool) -> 'a -> 'a -> t

  (** A property that evaluates to true if the given raises the named
      exception, up to the built-in polymorphic equality. *)
  val raises : exn -> (unit -> 'a) -> t

  (** Attach a precondition to a property. *)
  val (==>) : bool -> t -> t

  (** Tests the given property 10000 times with random data for each
      universally quantified variable. If a counterexample is
      discovered, then an attempt to shrink it to produce a smaller
      refutation of the property is made. If no counterexample is
      found, then [`OkAsFarAsIKnow] is returned. *)
  val check : t ->
    [ `OkAsFarAsIKnow
    | `CounterExample of ((string * string) list * string)
    | `GivenUp of int * int
    ]

  (** Turns a property into an OUnit test case. {!check_property} is
      invoked on the property. If a counterexample is discovered then the
      test fails. *)
  val to_test : t -> OUnit.test
end

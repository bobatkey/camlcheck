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

  val fail : 'a t
  val bits : int t
  val int : int -> int t
  val int32 : int32 -> int32 t
  val nativeint : nativeint -> nativeint t
  val int64 : int64 -> int64 t
  val float : float -> float t
  val bool : bool t
  val array : int -> (int -> 'a t) -> 'a array t

  val get_size : int t

  val run : 'a t -> Random.State.t -> int -> 'a option
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

  (** {2 Accessing parts of an {!Arbitrary.t} domain} *)

  (** Get the underlying generator of a domain. *)
  val to_generator   : 'a t -> 'a Generator.t

  (** Get the shrinking function associated with a domain. *)
  val to_shrinker    : 'a t -> 'a -> 'a list

  (** Get the human-readable description of a domain. *)
  val to_description : 'a t -> string

  (** Get the [to_string] function associated with a domain. *)
  val to_string   : 'a t-> 'a -> string

  (**{2 Arbitrary values for some primitive types} *)

  (** The domain of the [unit] value, [()]. *)
  val unit : unit t

  (** The domain of [bool] values. Uniformly selects between [true]
      and [false]. *)
  val bool : bool t

  (** Domain of [int] values within a certain range. *)
  val int_range : int -> int -> int t

  (** Domain of [int]s. *)
  val int : int t

  (** Domain of [float] values within a certain range. *)
  val float_range : float -> float -> float t

  (** Domain of [list]s, with elements taken from the given
      domain. *)
  val list : 'a t -> 'a list t

  (** Domain of [char] values, in the full range [Char.chr
      0] to [Char.chr 255]. *)
  val char : char t

  (** Domain of printable ascii [char]s. This is the range
      [Char.chr 32] to [Char.chr 127]. *)
  val printable_ascii_char : char t

  (** Domain of [string]s. These strings may contain any [char] value
      (see {!char} above). *)
  val string : string t

  (** Domain of [string]s with only printable ascii
      characters. See {!printable_ascii_char} above. *)
  val printable_ascii_string : string t

  (** Domain of [array]s, with elements taken from the
      given domain. *)
  val array : 'a t -> 'a array t

  (** Domain of [option] values, either [None] or [Some x], where [x]
      is taken from the given domain. *)
  val option : 'a t -> 'a option t

  (** Domain consisting of the elements of the given list. The
      [~to_string] function is used for reporting purposes. *)
  val element_of : to_string:('a -> string) -> 'a list -> 'a t
end

(** Universally quantified properties for randomised testing. *)
module Property : sig
  (** The type of properties checkable with randomised testing. *)
  type t

  (** Construct a universally quantified property over a given
      {!Arbitrary.t} domain. *)
  val forall : 'a Arbitrary.t -> ('a -> t) -> t

  (** Property that is true if the given [bool] is true. *)
  val true_that : bool -> t

  (** Atomic property of two values being equal. The [~to_string]
      argument is used for printing out values to report back in the
      case when the equality check fails. The optional [~cmp] argument
      is used to compare the two arguments. If it is not present, then
      the built-in polymorphic equality is used. *)
  val equal : to_string:('a -> string) -> ?cmp:('a -> 'a -> bool) -> 'a -> 'a -> t

  (** A property that evaluates to true if the given raises the named
      exception, up to the built-in polymorphic equality. *)
  val raises : exn -> (unit -> 'a) -> t

  (** Attach a precondition to a property. *)
  val (==>) : bool -> t -> t

  (** Configuration parameters for randomised checking. *)
  type configuration =
      { num_trials               : int
      ; max_failed_preconditions : int
      ; size_parameter           : int
      }

  (** Tests the given property 10000 times with random data for each
      universally quantified variable. If a counterexample is
      discovered, then an attempt to shrink it to produce a smaller
      refutation of the property is made. If no counterexample is
      found, then [`OkAsFarAsIKnow] is returned. *)
  val check : configuration -> t ->
    [ `OkAsFarAsIKnow
    | `CounterExample of ((string * string) list * string)
    | `GivenUp of int * int
    ]

  (** Turns a property into an OUnit test case. The main testing
      function {!check} is invoked on the property. If a
      counterexample is discovered then the test fails. *)
  val to_test : t -> OUnit.test
end

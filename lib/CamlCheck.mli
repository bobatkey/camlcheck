type 'a arbitrary

type property

val forall : 'a arbitrary -> ('a -> property) -> property

val check_property : property -> [ `Ok | `CounterExample of string list ]

val check : bool -> property

val ( ^$ ) : ('a -> 'b) -> 'a -> 'b

module Arbitrary : sig
  val unit : unit arbitrary

  val bool : bool arbitrary
  val int_range : int -> int -> int arbitrary

  val list : 'a arbitrary -> 'a list arbitrary

  val char : char arbitrary
  val printable_ascii_char : char arbitrary

  val string : string arbitrary
  val printable_ascii_string : string arbitrary

  val array : 'a arbitrary -> 'a array arbitrary

  val option : 'a arbitrary -> 'a option arbitrary
end

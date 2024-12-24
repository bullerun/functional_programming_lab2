module type StringableKey = sig
  type t

  val to_string : t -> string
  val of_string : string -> t
end

module Dict (Key : StringableKey) : sig
  type 'a t

  val empty : 'a t
  val init : unit -> 'a t
  val insert : Key.t -> 'a -> 'a t -> 'a t
  val find : Key.t -> 'a t -> 'a option
  val remove : Key.t -> 'a t -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val to_string : ('a -> string) -> 'a t -> string
  val filter : ('a -> bool) -> 'a t -> 'a t
  val merge : 'a t -> 'a t -> 'a t
  val equal : 'a t -> 'a t -> bool
end

(*
 * This file is part of Kaputt.
 * Copyright (C) 2008-2009 Xavier Clerc.
 *
 * Kaputt is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * Kaputt is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

(** This module provides abbreviations of modules and functions for easier use. *)


(** {6 Shorthands for modules} *)

(** Shorthand for [Assertion] module. *)
module Assert :
    sig
      val fail : string -> string -> string -> 'a
      val fail_msg : string -> 'a
      val default_printer : 'a -> string
      val assert_equal : ?eq:('a -> 'a -> bool) -> ?prn:('a -> string) -> ?msg:string -> 'a -> 'a -> unit
      val assert_not_equal : ?eq:('a -> 'a -> bool) -> ?prn:('a -> string) -> ?msg:string -> 'a -> 'a -> unit
      val assert_same : ?prn:('a -> string) -> ?msg:string -> 'a -> 'a -> unit
      val assert_not_same : ?prn:('a -> string) -> ?msg:string -> 'a -> 'a -> unit
      val make_equal : ('a -> 'a -> bool) -> ('a -> string) -> ?msg:string -> 'a -> 'a -> unit
      val make_not_equal : ('a -> 'a -> bool) -> ('a -> string) -> ?msg:string -> 'a -> 'a -> unit
      val assert_equal_bool : ?msg:string -> bool -> bool -> unit
      val assert_not_equal_bool : ?msg:string -> bool -> bool -> unit
      val assert_equal_int : ?msg:string -> int -> int -> unit
      val assert_not_equal_int : ?msg:string -> int -> int -> unit
      val assert_equal_int32 : ?msg:string -> int32 -> int32 -> unit
      val assert_not_equal_int32 : ?msg:string -> int32 -> int32 -> unit
      val assert_equal_int64 : ?msg:string -> int64 -> int64 -> unit
      val assert_not_equal_int64 : ?msg:string -> int64 -> int64 -> unit
      val assert_equal_nativeint : ?msg:string -> nativeint -> nativeint -> unit
      val assert_not_equal_nativeint : ?msg:string -> nativeint -> nativeint -> unit
      val assert_equal_char : ?msg:string -> char -> char -> unit
      val assert_not_equal_char : ?msg:string -> char -> char -> unit
      val assert_equal_string : ?msg:string -> string -> string -> unit
      val assert_not_equal_string : ?msg:string -> string -> string -> unit
      val assert_equal_float : ?eps:float -> ?msg:string -> float -> float -> unit
      val assert_not_equal_float : ?eps:float -> ?msg:string -> float -> float -> unit
      val assert_equal_complex : ?eps:float -> ?msg:string -> Complex.t -> Complex.t -> unit
      val assert_not_equal_complex : ?eps:float -> ?msg:string -> Complex.t -> Complex.t -> unit
      val assert_true : ?msg:string -> bool -> unit
      val assert_false : ?msg:string -> bool -> unit
      val assert_raises : ?msg:string -> (unit -> 'a) -> unit
    end

(** Shorthand for [Generator] module. *)
module Gen :
    sig
      type random = Generator.random
      val make_random : unit -> random
      type 'a t = (random -> 'a) * ('a -> string)
      val unit : unit t
      val bool : bool t
      val make_bool : int -> int -> bool t
      val int : int t
      val pos_int : int t
      val neg_int : int t
      val make_int : int -> int -> int t
      val int32 : int32 t
      val pos_int32 : int32 t
      val neg_int32 : int32 t
      val make_int32 : int32 -> int32 -> int32 t
      val int64 : int64 t
      val pos_int64 : int64 t
      val neg_int64 : int64 t
      val make_int64 : int64 -> int64 -> int64 t
      val nativeint : nativeint t
      val pos_nativeint : nativeint t
      val neg_nativeint : nativeint t
      val make_nativeint : nativeint -> nativeint -> nativeint t
      val char : char t
      val digit : char t
      val letter : char t
      val alphanum : char t
      val string : int t -> char t -> string t
      val strings : string -> int t -> string t -> string t
      val number : int t -> string t
      val word : int t -> string t
      val words : int t -> int t -> string t
      val float : float t
      val make_float : float -> float -> float t
      val complex : float t -> float t -> Complex.t t
      val array : int t -> 'a t -> ('a array) t
      val list : int t -> 'a t -> ('a list) t
      val option : bool t -> 'a t -> ('a option) t
      val ref : 'a t -> ('a ref) t
      val buffer : string t -> Buffer.t t
      val lift : 'a -> string -> 'a t
      val select_list : 'a list -> ('a -> string) -> 'a t
      val select_list_weighted : ('a * int) list -> ('a -> string) -> 'a t
      val select_array : 'a array -> ('a -> string) -> 'a t
      val select_array_weighted : ('a * int) array -> ('a -> string) -> 'a t
      val choose_list : ('a t) list -> 'a t
      val choose_list_weighted : (('a t) * int) list -> 'a t
      val choose_array : ('a t) array -> 'a t
      val choose_array_weighted : (('a t) * int) array -> 'a t
      val filter : ('a -> bool) -> 'a t -> 'a t
      val transform : ('a -> 'a) -> 'a t -> 'a t
      val map1 : ('a -> 'b) -> ('b -> string) -> 'a t -> 'b t
      val map2 : ('a -> 'b -> 'c) -> ('c -> string) -> (('a t) * ('b t)) -> 'c t
      val map3 : ('a -> 'b -> 'c -> 'd) -> ('d -> string) -> (('a t) * ('b t) * ('c t)) -> 'd t
      val map4 : ('a -> 'b -> 'c -> 'd -> 'e) -> ('e -> string) -> (('a t) * ('b t) * ('c t) * ('d t)) -> 'e t
      val map5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> ('f -> string) -> (('a t) * ('b t) * ('c t) * ('d t) * ('e t)) -> 'f t
      val zip1 : 'a t -> 'a t
      val zip2 : 'a t -> 'b t -> ('a * 'b) t
      val zip3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
      val zip4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
      val zip5 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> ('a * 'b * 'c * 'd * 'e) t
      val apply1 : ('a -> 'b) -> 'a -> 'b
      val apply2 : ('a -> 'b -> 'c) -> ('a * 'b) -> 'c
      val apply3 : ('a -> 'b -> 'c -> 'd) -> ('a * 'b * 'c) -> 'd
      val apply4 : ('a -> 'b -> 'c -> 'd -> 'e) -> ('a * 'b * 'c * 'd) -> 'e
      val apply5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> ('a * 'b * 'c * 'd * 'e) -> 'f
    end

(** Shorthand for [Specification] module. *)
module Spec :
    sig
      type 'a predicate = 'a -> bool
      type ('a, 'b) t = ('a, 'b) Specification.t = { precond : 'a predicate; postcond : ('a * 'b) predicate }
      val implies : 'a predicate -> ('a * 'b) predicate -> ('a, 'b) t
      val (=>) : 'a predicate -> ('a * 'b) predicate -> ('a, 'b) t
      val implies' : 'a predicate -> 'b predicate -> ('a, 'b) t
      val (==>) : 'a predicate -> 'b predicate -> ('a, 'b) t
      val always : 'a predicate
      val never : 'a predicate
      val is_pos_int : int predicate
      val is_neg_int : int predicate
      val is_zero_int : int predicate
      val is_nonzero_int : int predicate
      val is_even_int : int predicate
      val is_odd_int : int predicate
      val is_pos_int32 : int32 predicate
      val is_neg_int32 : int32 predicate
      val is_zero_int32 : int32 predicate
      val is_nonzero_int32 : int32 predicate
      val is_even_int32 : int32 predicate
      val is_odd_int32 : int32 predicate
      val is_pos_int64 : int64 predicate
      val is_neg_int64 : int64 predicate
      val is_zero_int64 : int64 predicate
      val is_nonzero_int64 : int64 predicate
      val is_even_int64 : int64 predicate
      val is_odd_int64 : int64 predicate
      val is_pos_nativeint : nativeint predicate
      val is_neg_nativeint : nativeint predicate
      val is_zero_nativeint : nativeint predicate
      val is_nonzero_nativeint : nativeint predicate
      val is_even_nativeint : nativeint predicate
      val is_odd_nativeint : nativeint predicate
      val is_pos_float : float predicate
      val is_neg_float : float predicate
      val is_zero_float_eps : float -> float predicate
      val is_nonzero_float_eps : float -> float predicate
      val is_zero_float : float predicate
      val is_nonzero_float : float predicate
      val is_nan_float : float predicate
      val is_nonnan_float : float predicate
      val is_letter_char : char predicate
      val is_digit_char : char predicate
      val is_space_char : char predicate
      val is_alphanum_char : char predicate
      val is_empty_string : string predicate
      val is_nonempty_string : string predicate
      val is_empty_list : ('a list) predicate
      val is_nonempty_list : ('a list) predicate
      val is_empty_array : ('a array) predicate
      val is_nonempty_array : ('a array) predicate
      val is_none_option : ('a option) predicate
      val is_some_option : ('a option) predicate
      val exists_string : char predicate -> string predicate
      val for_all_string : char predicate -> string predicate
      val exists_list : 'a predicate -> ('a list) predicate
      val for_all_list : 'a predicate -> ('a list) predicate
      val exists_array : 'a predicate -> ('a array) predicate
      val for_all_array : 'a predicate -> ('a array) predicate
      val logand : 'a predicate -> 'a predicate -> 'a predicate
      val (&&&) : 'a predicate -> 'a predicate -> 'a predicate
      val logor : 'a predicate -> 'a predicate -> 'a predicate
      val (|||) : 'a predicate -> 'a predicate -> 'a predicate
      val logxor : 'a predicate -> 'a predicate -> 'a predicate
      val (^^^) : 'a predicate -> 'a predicate -> 'a predicate
      val not : 'a predicate -> 'a predicate
      val zip1 : 'a predicate -> 'a predicate
      val zip2 : 'a predicate -> 'b predicate -> ('a * 'b) predicate
      val zip3 : 'a predicate -> 'b predicate -> 'c predicate -> ('a * 'b * 'c) predicate
      val zip4 : 'a predicate -> 'b predicate -> 'c predicate -> 'd predicate -> ('a * 'b * 'c * 'd) predicate
      val zip5 : 'a predicate -> 'b predicate -> 'c predicate -> 'd predicate -> 'e predicate -> ('a * 'b * 'c * 'd * 'e) predicate
    end

(** Bare alias for [Test] module. *)
module Test :
    sig
      type result = Test.result =
        | Passed
        | Failed of string * string * string
        | Report of int * int * int * (string list) * ((string * int) list)
        | Exit_code of int
      type 'a classifier = 'a -> string
      type t
      val return : 'a -> (unit -> 'a)
      val make_assert_test : ?title:string -> (unit -> 'a) -> ('a -> 'b) -> ('b -> unit) -> t
      val make_simple_test : ?title:string -> (unit -> unit) -> t
      val default_classifier : 'a classifier
      val make_random_test : ?title:string -> ?nb_runs:int -> ?classifier:'a classifier -> ?random_src:Generator.random -> 'a Generator.t -> ('a -> 'b) -> (('a, 'b) Specification.t) list -> t
      val exec_test : t -> result
      val exec_tests : t list -> result list
      val run_test : t -> unit
      val run_tests : t list -> unit
      val check : ?title:string -> ?nb_runs:int -> ?classifier:('a -> string) -> ?random_src:Generator.random -> 'a Generator.t -> ('a -> 'b) -> (('a, 'b) Specification.t) list -> unit
    end


(** {6 Shorthands for functions} *)

val (=>) : 'a Specification.predicate -> ('a * 'b) Specification.predicate -> ('a, 'b) Specification.t
(** Shorthand for [Specification.implies] function. *)

val (==>) : 'a Specification.predicate -> 'b Specification.predicate -> ('a, 'b) Specification.t
(** Shorthand for [Specification.implies'] function. *)

val (&&&) : 'a Specification.predicate -> 'a Specification.predicate -> 'a Specification.predicate
(** Shorthand for [Specification.logand] function. *)

val (|||) : 'a Specification.predicate -> 'a Specification.predicate -> 'a Specification.predicate
(** Shorthand for [Specification.logor] function. *)

val (^^^) : 'a Specification.predicate -> 'a Specification.predicate -> 'a Specification.predicate
(** Shorthand for [Specification.logxor] function. *)

val check : ?title:string -> ?nb_runs:int -> ?classifier:'a Test.classifier -> ?random_src:Generator.random -> 'a Generator.t -> ('a -> 'b) -> (('a, 'b) Specification.t) list -> unit
(** Shorthand for [Test.check] function. *)

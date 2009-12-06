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

(** This module provides functions for the 'nums' library. *)


(** {6 Assertion functions} *)

module Assertion : sig

  val equal_big_int : ?msg:string -> Big_int.big_int -> Big_int.big_int -> unit
  (** Same as [equal], but specialized for [Big_int.big_int] values. *)

  val not_equal_big_int : ?msg:string -> Big_int.big_int -> Big_int.big_int -> unit
  (** Same as [not_equal], but specialized for [Big_int.big_int] values. *)

  val equal_num : ?msg:string -> Num.num -> Num.num -> unit
  (** Same as [equal], but specialized for [Num.num] values. *)

  val not_equal_num : ?msg:string -> Num.num -> Num.num -> unit
  (** Same as [not_equal], but specialized for [Num.num] values. *)

end


(** {6 Generators} *)

module Generator : sig

  val big_int : int Kaputt.Generator.t -> Big_int.big_int Kaputt.Generator.t
  (** [big_int l] constructs a generator for [Big_int.big_int] values.
      [l] is used to determine the number of (decimal) digits of the value. *)

  val num : Big_int.big_int Kaputt.Generator.t -> Big_int.big_int Kaputt.Generator.t -> Num.num Kaputt.Generator.t
  (** [num n d] constructs a generator for [Num.num] values.
      [n] is used to generate the numerator, while [d] is used to generate the
      denominator. *)

end


(** {6 Enumerators} *)

module Enumerator : sig

  val big_int : Big_int.big_int -> Big_int.big_int -> Big_int.big_int Kaputt.Enumerator.t
  (** [big_int x y] constructs an enumerator for [Big_int.big_int] values
      between [x] and [y] (both inclusive). *)

  val num : Big_int.big_int Kaputt.Enumerator.t -> Big_int.big_int Kaputt.Enumerator.t -> Num.num Kaputt.Enumerator.t
  (** [num n d] constructs an enumerator for [Num.num] values.
      [n] is used to enumerate the numerators, while [d] is used to enumerate the
      denominators. *)

end

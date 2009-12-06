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

(** This module provides functions for the 'bigarray' library. *)


(** {6 Generator} *)

module Generator : sig

  val bigarray : ('a, 'b) Bigarray.kind -> 'c Bigarray.layout -> int array Kaputt.Generator.t -> 'a Kaputt.Generator.t -> ('a, 'b, 'c) Bigarray.Genarray.t Kaputt.Generator.t
  (** [bigarray k l d e] constructs a generator for [Bigarray.Genarray.t] values.
      [k] is the kind of generated arrays and [l] is the layout of generated arrays.
      [d] is used to determine the dimensions of the array, while [e] is used to
      generate elements. *)

end


(** {6 Enumerator} *)

module Enumerator : sig

  val bigarray : ('a, 'b) Bigarray.kind -> 'c Bigarray.layout -> int array -> 'a Kaputt.Enumerator.t -> ('a, 'b, 'c) Bigarray.Genarray.t Kaputt.Enumerator.t
  (** [bigarray k l dims e] constructs an enumerator for [Bigarray.Genarray.t]
      values of kind [k], layout [l], and dimensions [dims]. [e] is used to
      enumerate elements. *)

end

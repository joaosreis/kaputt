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

(** This module provides utility functions for the other modules. *)


(** {6 Conversion utilities} *)

val string_of_string : string -> string
(** Converts a string into an escaped string, for display. *)

val string_of_complex : Complex.t -> string
(** Converts a complex into a string. *)


(** {6 Bigarray utilities} *)

val bigarray_iter : ('a -> unit) -> ('a, 'b, 'c) Bigarray.Genarray.t -> unit
(** [bigarray_iter f a] applies [f] in turn to all elements of [a]. *)

val bigarray_iteri : (int array -> 'a -> unit) -> ('a, 'b, 'c) Bigarray.Genarray.t -> unit
(** Same as [bigarray_iter], except that the function also reveices the coordinates of the
    elements as the first argument. *)

val string_of_bigarray : ('a -> string) -> ('a, 'b, 'c) Bigarray.Genarray.t -> string
(** [string_of_bigarray f a] converts [a] into a string, using [f] to convert each element. *) 

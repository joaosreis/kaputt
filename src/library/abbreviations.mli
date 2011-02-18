(*
 * This file is part of Kaputt.
 * Copyright (C) 2008-2011 Xavier Clerc.
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

(** Shorthand for [Assertion] module.
    {b The {i assert_xyz} functions are deprecated.} *)
module Assert : module type of Assertion

(** Shorthand for [Generator] module. *)
module Gen : module type of Generator

(** Shorthand for [Enumerator] module. *)
module Enum : module type of Enumerator

(** Shorthand for [Specification] module. *)
module Spec : module type of Specification

(** Shorthand for [Shell] module. *)
module Shell : module type of Shell

(** Bare alias for [Test] module. *)
module Test : module type of Test


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

val check : ?title:string -> ?nb_runs:int -> ?nb_tries:int -> ?classifier:'a Test.classifier -> ?random_src:Generator.random -> 'a Generator.t -> ('a -> 'b) -> (('a, 'b) Specification.t) list -> unit
(** Shorthand for [Test.check] function. *)

val (|>) : ('a, [`Output], 'c1) Shell.command -> ([`Input], 'b , 'c2) Shell.command -> ('a, 'b, 'c2) Shell.command
(** Shorthand for [Shell.pipe] function. *)

val (>>) : ('a, [`Output], 'c) Shell.command -> string -> ('a, [`No_output], 'c) Shell.command
(** Shorthand for [Shell.redirect_output] function. *)

val (>>>) : ('a, [`Output], 'c) Shell.command -> string -> ('a, [`No_output], 'c) Shell.command
(** Shorthand for [Shell.redirect_append] function. *)

val (>>>>) : ('a, 'b, [`Error]) Shell.command -> string -> ('a, 'b, [`No_error]) Shell.command
(** Shorthand for [Shell.redirect_error] function. *)

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

(** This module provides functions evaluating assertions. *)


(** {6 Exception} *)

exception Failed of string * string * string
(** The exception raised when an assertion fails.
    The parameters are:
    - the waited value (as a string);
    - the actual value (as a string);
    - the message associated with the failure. *)

val fail : string -> string -> string -> 'a
(** Raises [Failed] with the passed parameters
    (waited value, actual value, and message). *)

val fail_msg : string -> 'a
(** [fail_msg m] is equivalent to [fail "" "" m]. *)


(** {6 Generic functions} *)

val default_printer : 'a -> string
(** Default printer, always returning [""]. *)

val assert_equal : ?eq:('a -> 'a -> bool) -> ?prn:('a -> string) -> ?msg:string -> 'a -> 'a -> unit
(** [assert_equal ~eq:e ~prn:p ~msg:m x y] raises [Failed] if [x] and [y] are
    not equal, relatively to the equality function [e]. [p] is used to convert
    [x] and [y] to strings (used only upon failure), and [m] is the message
    associated with the assertion.
    Default parameter values:
    - [e] defaults to [(=)];
    - [p] defaults to [default_printer];
    - [m] defaults to [""]. *)

val assert_not_equal : ?eq:('a -> 'a -> bool) -> ?prn:('a -> string) -> ?msg:string -> 'a -> 'a -> unit
(** [assert_not_equal ~eq:e ~prn:p ~msg:m x y] raises [Failed] if [x] and [y] are
    equal, relatively to the equality function [e]. [p] is used to convert [x]
    and [y] to strings (used only upon failure), and [m] is the message associated
    with the assertion.
    Default parameter values:
    - [e] defaults to [(=)];
    - [p] defaults to [default_printer];
    - [m] defaults to [""]. *)

val assert_same : ?prn:('a -> string) -> ?msg:string -> 'a -> 'a -> unit
(** Same as [assert_equal], but based on physical equality. *)

val assert_not_same : ?prn:('a -> string) -> ?msg:string -> 'a -> 'a -> unit
(** Same as [assert_not_equal], but based on physical equality. *)


(** {6 Function builders} *)

val make_equal : ('a -> 'a -> bool) -> ('a -> string) -> ?msg:string -> 'a -> 'a -> unit
(** [make_equal e p] is equivalent to [assert_equal ~eq:e ~prn:p]. *)

val make_not_equal : ('a -> 'a -> bool) -> ('a -> string) -> ?msg:string -> 'a -> 'a -> unit
(** [make_not_equal e p] is equivalent to [assert_not_equal ~eq:e ~prn:p]. *)


(** {6 Specialized functions} *)

val assert_equal_bool : ?msg:string -> bool -> bool -> unit
(** Same as [assert_equal], but specialized for [bool] values. *)

val assert_not_equal_bool : ?msg:string -> bool -> bool -> unit
(** Same as [assert_not_equal], but specialized for [bool] values. *)

val assert_equal_int : ?msg:string -> int -> int -> unit
(** Same as [assert_equal], but specialized for [int] values. *)

val assert_not_equal_int : ?msg:string -> int -> int -> unit
(** Same as [assert_not_equal], but specialized for [int] values. *)

val assert_equal_int32 : ?msg:string -> int32 -> int32 -> unit
(** Same as [assert_equal], but specialized for [int32] values. *)

val assert_not_equal_int32 : ?msg:string -> int32 -> int32 -> unit
(** Same as [assert_not_equal], but specialized for [int32] values. *)

val assert_equal_int64 : ?msg:string -> int64 -> int64 -> unit
(** Same as [assert_equal], but specialized for [int64] values. *)

val assert_not_equal_int64 : ?msg:string -> int64 -> int64 -> unit
(** Same as [assert_not_equal], but specialized for [int64] values. *)

val assert_equal_nativeint : ?msg:string -> nativeint -> nativeint -> unit
(** Same as [assert_equal], but specialized for [nativeint] values. *)

val assert_not_equal_nativeint : ?msg:string -> nativeint -> nativeint -> unit
(** Same as [assert_not_equal], but specialized for [nativeint] values. *)

val assert_equal_char : ?msg:string -> char -> char -> unit
(** Same as [assert_equal], but specialized for [char] values. *)

val assert_not_equal_char : ?msg:string -> char -> char -> unit
(** Same as [assert_not_equal], but specialized for [char] values. *)

val assert_equal_string : ?msg:string -> string -> string -> unit
(** Same as [assert_equal], but specialized for [string] values. *)

val assert_not_equal_string : ?msg:string -> string -> string -> unit
(** Same as [assert_not_equal], but specialized for [string] values. *)

val assert_equal_float : ?eps:float -> ?msg:string -> float -> float -> unit
(** Same as [assert_equal], but specialized for [float] values. *)

val assert_not_equal_float : ?eps:float -> ?msg:string -> float -> float -> unit
(** Same as [assert_not_equal], but specialized for [float] values. *)

val assert_equal_complex : ?eps:float -> ?msg:string -> Complex.t -> Complex.t -> unit
(** Same as [assert_equal], but specialized for [Complex.t] values. *)

val assert_not_equal_complex : ?eps:float -> ?msg:string -> Complex.t -> Complex.t -> unit
(** Same as [assert_not_equal], but specialized for [Complex.t] values. *)


(** {6 Miscellaneous} *)

val assert_true : ?msg:string -> bool -> unit
(** [assert_true ~msg:m x] raises [Failed] if [x] false.
    The default value for [m] is [""]. *)

val assert_false : ?msg:string -> bool -> unit
(** [assert_false ~msg:m x] raises [Failed] if [x] true.
    The default value for [m] is [""]. *)

val assert_raises : ?msg:string -> (unit -> 'a) -> unit
(** [assert_raises ~msg:m f] raises [Failed] if [f ()] evaluates without raising
    an exception. The default value for [m] is [""]. *)

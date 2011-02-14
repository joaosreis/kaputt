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

(** This module provides functions evaluating assertions. *)


(** {6 Exception} *)

type failure = {
    expected_value : string; (** Expected value converted to string. *)
    actual_value : string; (** Actual value converted to string. *)
    message : string; (** Short message associated with failure. *)
  }
(** Description of an assertion failure. *)

exception Failed of failure
(** The exception raised when an assertion fails. *)

val fail : string -> string -> string -> 'a
(** Raises [Failed] with the passed parameters
    (expected value, actual value, and message). *)

val fail_msg : string -> 'a
(** [fail_msg m] is equivalent to [fail "" "" m]. *)


(** {6 Generic functions} *)

val default_printer : 'a -> string
(** Default printer, always returning [""]. *)

val equal : ?eq:('a -> 'a -> bool) -> ?prn:('a -> string) -> ?msg:string -> 'a -> 'a -> unit
(** [equal ~eq:e ~prn:p ~msg:m x y] raises [Failed] if [x] and [y] are
    not equal, relatively to the equality function [e]. [p] is used to convert
    [x] and [y] to strings (used only upon failure), and [m] is the message
    associated with the assertion.
    Default parameter values:
    - [e] defaults to [(=)];
    - [p] defaults to [default_printer];
    - [m] defaults to [""]. *)

val not_equal : ?eq:('a -> 'a -> bool) -> ?prn:('a -> string) -> ?msg:string -> 'a -> 'a -> unit
(** [not_equal ~eq:e ~prn:p ~msg:m x y] raises [Failed] if [x] and [y] are
    equal, relatively to the equality function [e]. [p] is used to convert [x]
    and [y] to strings (used only upon failure), and [m] is the message associated
    with the assertion.
    Default parameter values:
    - [e] defaults to [(=)];
    - [p] defaults to [default_printer];
    - [m] defaults to [""]. *)

val same : ?prn:('a -> string) -> ?msg:string -> 'a -> 'a -> unit
(** Same as [equal], but based on physical equality. *)

val not_same : ?prn:('a -> string) -> ?msg:string -> 'a -> 'a -> unit
(** Same as [not_equal], but based on physical equality. *)


(** {6 Function builders} *)

val make_equal : ('a -> 'a -> bool) -> ('a -> string) -> ?msg:string -> 'a -> 'a -> unit
(** [make_equal e p] is equivalent to [equal ~eq:e ~prn:p]. *)

val make_not_equal : ('a -> 'a -> bool) -> ('a -> string) -> ?msg:string -> 'a -> 'a -> unit
(** [make_not_equal e p] is equivalent to [not_equal ~eq:e ~prn:p]. *)


(** {6 Specialized functions} *)

val equal_bool : ?msg:string -> bool -> bool -> unit
(** Same as [equal], but specialized for [bool] values. *)

val not_equal_bool : ?msg:string -> bool -> bool -> unit
(** Same as [not_equal], but specialized for [bool] values. *)

val equal_int : ?msg:string -> int -> int -> unit
(** Same as [equal], but specialized for [int] values. *)

val not_equal_int : ?msg:string -> int -> int -> unit
(** Same as [not_equal], but specialized for [int] values. *)

val equal_int32 : ?msg:string -> int32 -> int32 -> unit
(** Same as [equal], but specialized for [int32] values. *)

val not_equal_int32 : ?msg:string -> int32 -> int32 -> unit
(** Same as [not_equal], but specialized for [int32] values. *)

val equal_int64 : ?msg:string -> int64 -> int64 -> unit
(** Same as [equal], but specialized for [int64] values. *)

val not_equal_int64 : ?msg:string -> int64 -> int64 -> unit
(** Same as [not_equal], but specialized for [int64] values. *)

val equal_nativeint : ?msg:string -> nativeint -> nativeint -> unit
(** Same as [equal], but specialized for [nativeint] values. *)

val not_equal_nativeint : ?msg:string -> nativeint -> nativeint -> unit
(** Same as [not_equal], but specialized for [nativeint] values. *)

val equal_char : ?msg:string -> char -> char -> unit
(** Same as [equal], but specialized for [char] values. *)

val not_equal_char : ?msg:string -> char -> char -> unit
(** Same as [not_equal], but specialized for [char] values. *)

val equal_string : ?msg:string -> string -> string -> unit
(** Same as [equal], but specialized for [string] values. *)

val not_equal_string : ?msg:string -> string -> string -> unit
(** Same as [not_equal], but specialized for [string] values. *)

val equal_float : ?eps:float -> ?msg:string -> float -> float -> unit
(** Same as [equal], but specialized for [float] values.
    [eps] is the epsilon used for float comparison, defaulting to [epsilon_float]. *)

val not_equal_float : ?eps:float -> ?msg:string -> float -> float -> unit
(** Same as [not_equal], but specialized for [float] values.
    [eps] is the epsilon used for float comparison, defaulting to [epsilon_float]. *)

val equal_complex : ?eps:float -> ?msg:string -> Complex.t -> Complex.t -> unit
(** Same as [equal], but specialized for [Complex.t] values.
    [eps] is the epsilon used for float comparison, defaulting to [epsilon_float]. *)

val not_equal_complex : ?eps:float -> ?msg:string -> Complex.t -> Complex.t -> unit
(** Same as [not_equal], but specialized for [Complex.t] values.
    [eps] is the epsilon used for float comparison, defaulting to [epsilon_float]. *)


(** {6 Miscellaneous} *)

val is_true : ?msg:string -> bool -> unit
(** [is_true ~msg:m x] raises [Failed] if [x] false.
    The default value for [m] is [""]. *)

val is_false : ?msg:string -> bool -> unit
(** [is_false ~msg:m x] raises [Failed] if [x] true.
    The default value for [m] is [""]. *)

val is_some : ?msg:string -> 'a option -> unit
(** [is_some ~msg:m x] raises [Failed] if [x] is equal to [None].
    The default value for [m] is [""]. *)

val is_none : ?msg:string -> 'a option -> unit
(** [is_none ~msg:m x] raises [Failed] if [x] is different from [None].
    The default value for [m] is [""]. *)

val raises : ?msg:string -> (unit -> 'a) -> unit
(** [raises ~msg:m f] raises [Failed] if [f ()] evaluates without raising
    an exception. The default value for [m] is [""]. *)

val no_raise : ?msg:string -> (unit -> 'a) -> unit
(** [no_raise ~msg:m f] raises [Failed] if [f ()] raises an exception.
    The default value for [m] is [""]. *)

val make_raises : (exn -> bool) -> (exn -> string) -> ?msg:string -> (unit -> 'a) -> unit
(** [make_raises eq p ~msg:m f] raises [Failed] if [f ()] evaluates without
    raising an exception [e] that makes [eq e] evaluates to [true]. [p] is used
    to convert [e] into a string (used only upon failure), and [m] is the
    message associated with the assertion (defaulting to [""]). *)


(** {6 Deprecated functions} *)

val assert_equal : ?eq:('a -> 'a -> bool) -> ?prn:('a -> string) -> ?msg:string -> 'a -> 'a -> unit
(** @deprecated Use [equal] instead. *)

val assert_not_equal : ?eq:('a -> 'a -> bool) -> ?prn:('a -> string) -> ?msg:string -> 'a -> 'a -> unit
(** @deprecated Use [not_equal] instead. *)

val assert_same : ?prn:('a -> string) -> ?msg:string -> 'a -> 'a -> unit
(** @deprecated Use [same] instead. *)

val assert_not_same : ?prn:('a -> string) -> ?msg:string -> 'a -> 'a -> unit
(** @deprecated Use [not_same] instead. *)

val assert_equal_bool : ?msg:string -> bool -> bool -> unit
(** @deprecated Use [_equal_bool] instead. *)

val assert_not_equal_bool : ?msg:string -> bool -> bool -> unit
(** @deprecated Use [not_equal_bool] instead. *)

val assert_equal_int : ?msg:string -> int -> int -> unit
(** @deprecated Use [equal_int] instead. *)

val assert_not_equal_int : ?msg:string -> int -> int -> unit
(** @deprecated Use [not_equal_int] instead. *)

val assert_equal_int32 : ?msg:string -> int32 -> int32 -> unit
(** @deprecated Use [equal_int32] instead. *)

val assert_not_equal_int32 : ?msg:string -> int32 -> int32 -> unit
(** @deprecated Use [not_equal_int32] instead. *)

val assert_equal_int64 : ?msg:string -> int64 -> int64 -> unit
(** @deprecated Use [equal_int64] instead. *)

val assert_not_equal_int64 : ?msg:string -> int64 -> int64 -> unit
(** @deprecated Use [not_equal_int64] instead. *)

val assert_equal_nativeint : ?msg:string -> nativeint -> nativeint -> unit
(** @deprecated Use [equal_nativeint] instead. *)

val assert_not_equal_nativeint : ?msg:string -> nativeint -> nativeint -> unit
(** @deprecated Use [not_equal_nativeint] instead. *)

val assert_equal_char : ?msg:string -> char -> char -> unit
(** @deprecated Use [equal_char] instead. *)

val assert_not_equal_char : ?msg:string -> char -> char -> unit
(** @deprecated Use [not_equal_char] instead. *)

val assert_equal_string : ?msg:string -> string -> string -> unit
(** @deprecated Use [equal_string] instead. *)

val assert_not_equal_string : ?msg:string -> string -> string -> unit
(** @deprecated Use [not_equal_string] instead. *)

val assert_equal_float : ?eps:float -> ?msg:string -> float -> float -> unit
(** @deprecated Use [equal_float] instead. *)

val assert_not_equal_float : ?eps:float -> ?msg:string -> float -> float -> unit
(** @deprecated Use [not_equal_float] instead. *)

val assert_equal_complex : ?eps:float -> ?msg:string -> Complex.t -> Complex.t -> unit
(** @deprecated Use [equal_complex] instead. *)

val assert_not_equal_complex : ?eps:float -> ?msg:string -> Complex.t -> Complex.t -> unit
(** @deprecated Use [not_equal_complex] instead. *)

val assert_equal_big_int : ?msg:string -> Big_int.big_int -> Big_int.big_int -> unit
(** @deprecated Use [equal_big_int] instead. *)

val assert_not_equal_big_int : ?msg:string -> Big_int.big_int -> Big_int.big_int -> unit
(** @deprecated Use [not_equal_big_int] instead. *)

val assert_equal_num : ?msg:string -> Num.num -> Num.num -> unit
(** @deprecated Use [equal_num] instead. *)

val assert_not_equal_num : ?msg:string -> Num.num -> Num.num -> unit
(** @deprecated Use [not_equal_num] instead. *)

val assert_true : ?msg:string -> bool -> unit
(** @deprecated Use [is_true] instead. *)

val assert_false : ?msg:string -> bool -> unit
(** @deprecated Use [is_false] instead. *)

val assert_raises : ?msg:string -> (unit -> 'a) -> unit
(** @deprecated Use [raises] instead. *)

val assert_no_raise : ?msg:string -> (unit -> 'a) -> unit
(** @deprecated Use [no_raise] instead. *)

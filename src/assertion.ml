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


(* Exception *)

exception Failed of string * string * string

let fail x y z = raise (Failed (x, y, z))

let fail_msg = fail "" ""


(* Generic functions *)

let default_printer _ = ""

let equal ?(eq=(=)) ?(prn=default_printer) ?(msg="") x y =
  if not (eq x y) then fail (prn x) (prn y) msg

let not_equal ?(eq=(=)) ?(prn=default_printer) ?(msg="") x y =
  if eq x y then fail (prn x) (prn y) msg

let same ?(prn=default_printer) ?(msg="") x y =
  if not (x == y) then fail (prn x) (prn y) msg

let not_same ?(prn=default_printer) ?(msg="") x y =
  if x == y then fail (prn x) (prn y) msg


(* Function builders *)

let make_equal e p = equal ~eq:e ~prn:p

let make_not_equal e p = not_equal ~eq:e ~prn:p


(* Specialized functions *)

let equal_bool = make_equal (=) string_of_bool

let not_equal_bool = make_not_equal (=) string_of_bool

let equal_int = make_equal (=) string_of_int

let not_equal_int = make_not_equal (=) string_of_int

let equal_int32 = make_equal (=) Int32.to_string

let not_equal_int32 = make_not_equal (=) Int32.to_string

let equal_int64 = make_equal (=) Int64.to_string

let not_equal_int64 = make_not_equal (=) Int64.to_string

let equal_nativeint = make_equal (=) Nativeint.to_string

let not_equal_nativeint = make_not_equal (=) Nativeint.to_string

let string_of_char c = "'" ^ (Char.escaped c) ^ "'"

let equal_char = make_equal (=) string_of_char

let not_equal_char = make_not_equal (=) string_of_char

let string_of_string s = "\"" ^ (String.escaped s) ^ "\""

let equal_string = make_equal (=) string_of_string

let not_equal_string = make_not_equal (=) string_of_string

let make_float_eq eps =
  fun x y -> let delta = y -. x in (abs_float delta) <= eps

let equal_float ?(eps=epsilon_float) =
  equal ~eq:(make_float_eq eps) ~prn:string_of_float

let not_equal_float ?(eps=epsilon_float) =
  not_equal ~eq:(make_float_eq eps) ~prn:string_of_float

let make_complex_eq eps =
  fun x y ->
    let delta = y.Complex.re -. x.Complex.re in
    if (delta > eps) then
      false
    else
      let delta' = y.Complex.im -. x.Complex.im in
      (abs_float delta') <= eps

let string_of_complex x = Printf.sprintf "%f+%fi" x.Complex.re x.Complex.im

let equal_complex ?(eps=epsilon_float) =
  equal ~eq:(make_complex_eq eps) ~prn:string_of_complex

let not_equal_complex ?(eps=epsilon_float) =
  not_equal ~eq:(make_complex_eq eps) ~prn:string_of_complex

let equal_big_int = make_equal Big_int.eq_big_int Big_int.string_of_big_int

let not_equal_big_int = make_not_equal Big_int.eq_big_int Big_int.string_of_big_int

let equal_num = make_equal Num.eq_num Num.string_of_num

let not_equal_num = make_not_equal Num.eq_num Num.string_of_num


(* Miscellaneous *)

let is_true ?(msg="") x =
  if not x then fail_msg msg

let is_false ?(msg="") x =
  if x then fail_msg msg

let raises ?(msg="") f =
  let exn = try ignore (f ()); false with _ -> true in
  if not exn then fail_msg msg

let no_raise ?(msg="") f =
  let exn = try ignore (f ()); false with _ -> true in
  if exn then fail_msg msg


(* Deprecated functions *)

let assert_equal = equal

let assert_not_equal = not_equal

let assert_same = same

let assert_not_same = not_same

let assert_equal_bool = equal_bool

let assert_not_equal_bool = not_equal_bool

let assert_equal_int = equal_int

let assert_not_equal_int = not_equal_int

let assert_equal_int32 = equal_int32

let assert_not_equal_int32 = not_equal_int32

let assert_equal_int64 = equal_int64

let assert_not_equal_int64 = not_equal_int64

let assert_equal_nativeint = equal_nativeint

let assert_not_equal_nativeint = not_equal_nativeint

let assert_equal_char = equal_char

let assert_not_equal_char = not_equal_char

let assert_equal_string = equal_string

let assert_not_equal_string = not_equal_string

let assert_equal_float = equal_float

let assert_not_equal_float = not_equal_float

let assert_equal_complex = equal_complex

let assert_not_equal_complex = not_equal_complex

let assert_equal_big_int = equal_big_int

let assert_not_equal_big_int = not_equal_big_int

let assert_equal_num = equal_num

let assert_not_equal_num = not_equal_num

let assert_true = is_true

let assert_false = is_false

let assert_raises = raises

let assert_no_raise = no_raise

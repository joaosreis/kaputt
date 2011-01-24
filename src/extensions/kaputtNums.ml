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


(* Assertion functions *)

module Assertion = struct

  let equal_big_int = Kaputt.Assertion.make_equal Big_int.eq_big_int Big_int.string_of_big_int

  let not_equal_big_int = Kaputt.Assertion.make_not_equal Big_int.eq_big_int Big_int.string_of_big_int

  let equal_num = Kaputt.Assertion.make_equal Num.eq_num Num.string_of_num

  let not_equal_num = Kaputt.Assertion.make_not_equal Num.eq_num Num.string_of_num

end


(* Generators *)

module Generator = struct

  let gen_big_int_digit, _ = Kaputt.Generator.make_int 0 10

  let big_int (gen_l, _) =
    (fun r ->
      let s = Random.State.bool r in
      let len = gen_l r in
      let res = ref Big_int.zero_big_int in
      for i = 1 to len do
        res := Big_int.add_int_big_int
            (gen_big_int_digit r)
            (Big_int.mult_int_big_int 10 !res)
      done;
      if s then !res else Big_int.minus_big_int !res),
    Big_int.string_of_big_int

  let num (gen_a, _) (gen_b, _) =
    (fun r ->
      let a = gen_a r in
      let b = gen_b r in
      Num.div_num (Num.Big_int a) (Num.Big_int b)),
    Num.string_of_num

end


(* Enumerators *)

module Enumerator = struct

  let big_int =
    Kaputt.Enumerator.create_int_functions
      Big_int.succ_big_int
      Big_int.string_of_big_int

  let num n d =
    Kaputt.Enumerator.create_state_based
      (fun () -> [| n; d |])
      (fun s ->
        Num.div_num
          (Num.Big_int (Kaputt.Enumerator.State.get s 0))
          (Num.Big_int (Kaputt.Enumerator.State.get s 1)))
      Num.string_of_num

end

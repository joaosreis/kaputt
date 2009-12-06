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

open Kaputt.Abbreviations

let () =
  Test.add_simple_test
    ~title:"specialized functions"
    (fun () ->
      Assert.equal_bool true true;
      Assert.not_equal_bool false true;
      Assert.equal_int 3 3;
      Assert.not_equal_int 3 5;
      Assert.equal_int32 3l 3l;
      Assert.not_equal_int32 3l 5l;
      Assert.equal_int64 3L 3L;
      Assert.not_equal_int64 3L 5L;
      Assert.equal_nativeint 3n 3n;
      Assert.not_equal_nativeint 3n 5n;
      Assert.equal_char 'e' 'e';
      Assert.not_equal_char 'e' 'f';
      Assert.equal_string "azerty" "azerty";
      Assert.not_equal_string "azerty" "azerty2";
      Assert.equal_float 1. 1.;
      Assert.not_equal_float 1. 2.)

let () =
  Test.add_simple_test
    ~title:"equal_bool"
    (fun () -> Assert.equal_bool true false)

let () =
  Test.add_simple_test
    ~title:"not_equal_bool"
    (fun () -> Assert.not_equal_bool false false)

let () =
  Test.add_simple_test
    ~title:"equal_int"
    (fun () -> Assert.equal_int 3 7)

let () =
  Test.add_simple_test
    ~title:"not_equal_int"
    (fun () -> Assert.not_equal_int 3 3)

let () =
  Test.add_simple_test
    ~title:"equal_int32"
    (fun () -> Assert.equal_int32 3l 7l)

let () =
  Test.add_simple_test
    ~title:"not_equal_int32"
    (fun () -> Assert.not_equal_int32 3l 3l)

let () =
  Test.add_simple_test
    ~title:"equal_int64"
    (fun () -> Assert.equal_int64 3L 7L)

let () =
  Test.add_simple_test
    ~title:"not_equal_int64"
    (fun () -> Assert.not_equal_int64 3L 3L)

let () =
  Test.add_simple_test
    ~title:"equal_nativeint"
    (fun () -> Assert.equal_nativeint 3n 7n)

let () =
  Test.add_simple_test
    ~title:"not_equal_nativeint"
    (fun () -> Assert.not_equal_nativeint 3n 3n)

let () =
  Test.add_simple_test
    ~title:"equal_char"
    (fun () -> Assert.equal_char 'a' 'z')

let () =
  Test.add_simple_test
    ~title:"not_equal_char"
    (fun () -> Assert.not_equal_char 'e' 'e')

let () =
  Test.add_simple_test
    ~title:"equal_string"
    (fun () -> Assert.equal_string "azerty" "zertyu")

let () =
  Test.add_simple_test
    ~title:"not_equal_string"
    (fun () -> Assert.not_equal_string "azerty" "azerty")

let () =
  Test.add_simple_test
    ~title:"equal_float"
    (fun () -> Assert.equal_float 1. 2.)

let () =
  Test.add_simple_test
    ~title:"not_equal_float"
    (fun () -> Assert.not_equal_float 1. 1.)

let () =
  Test.add_simple_test
    ~title:"miscellaneous"
    (fun () ->
      Assert.is_true true;
      Assert.is_false false;
      Assert.is_some (Some 3);
      Assert.is_none None;
      Assert.raises (fun () -> failwith "msg");
      Assert.no_raise ignore;
      Assert.make_raises
        (function Not_found -> true | _ -> false)
        Printexc.to_string
        (fun () -> raise Not_found))

let () =
  Test.add_simple_test
    ~title:"is_true"
    (fun () -> Assert.is_true false)

let () =
  Test.add_simple_test
    ~title:"is_false"
    (fun () -> Assert.is_false true)

let () =
  Test.add_simple_test
    ~title:"is_some"
    (fun () -> Assert.is_some None)

let () =
  Test.add_simple_test
    ~title:"is_none"
    (fun () -> Assert.is_none (Some ""))

let () =
  Test.add_simple_test
    ~title:"raises"
    (fun () -> Assert.raises (fun () -> ()))

let () =
  Test.add_simple_test
    ~title:"no_raise"
    (fun () -> Assert.no_raise (fun () -> failwith "msg"))

let () =
  Test.add_simple_test
    ~title:"make_raises"
    (fun () ->
      Assert.make_raises
        (function Not_found -> false | _ -> true)
        Printexc.to_string
        (fun () -> raise Not_found))


let () =
  Test.launch_tests ~output:(Test.Text_output (open_out "result")) ();
  let diff = Shell.run (Shell.diff ~options:["-q"] "reference" "result") in
  if diff <> 0 then begin
    print_endline "failed";
    exit 1
  end else begin
    ignore (Shell.run (Shell.rm ~options:["-f"] ["result"]));
    exit 0
  end

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


(* Type definitions *)

type result =
  | Passed
  | Failed of string * string * string
  | Report of int * int * int * (string list) * ((string * int) list)

type 'a classifier = 'a -> string

type t = string * (unit -> result)


(* Generation of titles *)

let counter = ref 0

let get_title () =
  incr counter;
  "untitled no " ^ (string_of_int !counter)


(* Assertion-based tests *)

let return x = fun () -> x

let make_assert_test ?(title=get_title ()) set_up f tear_down =
  title,
  (fun () ->
    try
      tear_down (f (set_up ()));
      Passed
    with
    | Assertion.Failed (x, y, z) -> Failed (x, y, z)
    | e -> Failed ("", "", ("uncaught exception: " ^ (Printexc.to_string e))))

let return_unit = return ()

let make_simple_test ?(title=get_title ()) f =
  make_assert_test ~title:title return_unit f ignore


(** {6 Generator-based tests} *)

let default_classifier _ = ""

let make_random_test ?(title=get_title ()) ?(nb_runs=100) ?(classifier=default_classifier) ?(random_src=Generator.make_random ()) (gen, prn) f spec =
  if nb_runs <= 0 then invalid_arg "Kaputt.Test.make_random_test";
  title,
  (fun () ->
    let valid = ref 0 in
    let uncaught = ref 0 in
    let counterexamples = ref [] in
    let categories = Hashtbl.create 16 in
    for i = 1 to nb_runs do
      let x = ref (gen random_src) in
      while List.for_all (fun s -> not (s.Specification.precond !x)) spec do
        x := gen random_src
      done;
      try
        let y = f !x in
        let cat = classifier !x in
        let curr = try Hashtbl.find categories cat with _ -> 0 in
        Hashtbl.replace categories cat (succ curr);
        let elem = List.find (fun s -> s.Specification.precond !x) spec in
        if elem.Specification.postcond (!x, y) then
          incr valid
        else
          let x' = prn !x in
          if not (List.mem x' !counterexamples) then
            counterexamples := x' :: !counterexamples
      with _ -> incr uncaught
    done;
    let categories' = Hashtbl.fold (fun k v acc -> (k, v) :: acc) categories [] in
    Report (!valid, nb_runs, !uncaught, (List.rev !counterexamples), categories'))


(* Test runners *)

let exec_test (_, func) =
  func ()

let exec_tests = List.map exec_test

let run_test (name, func) =
  match func () with
  | Passed ->
      Printf.printf "Test '%s' ... passed\n" name
  | Failed (waited, actual, "") ->
      Printf.printf "Test '%s' ... failed\n  waited `%s` but received `%s`\n" name waited actual
  | Failed (waited, actual, message) ->
      Printf.printf "Test '%s' ... failed\n  %s (waited `%s` but received `%s`)\n" name message waited actual
  | Report (valid, total, uncaught, counterexamples, categories) ->
      Printf.printf "Test '%s' ... %d/%d case%s passed %s\n"
        name
        valid
        total
        (if valid > 1 then "s" else "")
        (match uncaught with
        | 0 -> ""
        | 1 -> "(1 uncaught exception)"
        | n -> "(" ^ (string_of_int n) ^ " uncaught exceptions)");
      if counterexamples <> [] then
        Printf.printf "  counterexample%s: %s\n"
          (if (List.length counterexamples) > 1 then "s" else "")
          (String.concat ", " counterexamples);
      if (List.length categories) > 1 then begin
        print_endline "  categories:";
        List.iter
          (fun (c, n) ->
            Printf.printf "    %s -> %d occurrence%s\n" c n (if n > 1 then "s" else ""))
          categories
      end

let run_tests = List.iter run_test

let check ?(title=get_title ()) ?(nb_runs=100) ?(classifier=default_classifier) ?(random_src=Generator.make_random ()) generator f spec =
  run_test
    (make_random_test
       ~title:title
       ~nb_runs:nb_runs
       ~classifier:classifier
       ~random_src:random_src
       generator
       f
       spec)

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

(** This module provides the functions for creating and running tests.

    Kaputt features two kinds of tests:
    - {i assertion-based} tests, inspired by the {i x}Unit tools,
    are basic unit tests where the developer explicitly writes input
    values and checks that output values satisfy given asssertions;
    - {i generator-based} tests, inspired by the QuickCheck tool,
    are unit tests where the developer builds specifications (using
    combinators) and then requests the framework to generate random
    values to be tested against the specification. *)


(** {6 Type definitions} *)

type result =
  | Passed
      (** Indicates that the assertion-based test passed. *)
  | Failed of string * string * string
      (** Indicates that the assertion-based test failed
          (parameters are expected value, actual value, and message). *)
  | Uncaught of exn * string
      (** Indicates that the assertion-based test raised a exception
          (parameters are exception, and associated backtrace). *)
  | Report of int * int * int * (string list) * ((string * int) list)
      (** Indicates how the generator-based execution performed
          Parameters are:
          - number of cases that passed;
          - total number of cases;
          - number of uncaught exceptions;
          - list of counterexamples found;
          - map from categories to occurrences (as an association list). *)
  | Exit_code of int
      (** Indicates how the shell-based execution performed
          (parameter being the exit code of the executed command). *)
(** The type of test outcomes. *)

type 'a classifier = 'a -> string
(** The type of classifying functions, used to categorize the generated elements. *)

type t
(** The type of tests. *)

type output_mode =
  | Text_output of out_channel
      (** Classical output, parameter being destination. *)
  | Html_output of out_channel
      (** XML output, parameter being destination. *)
  | Xml_output of out_channel
      (** XML output, parameter being destination. *)
  | Xml_junit_output of out_channel
      (** XML output (in JUnit-compatible format - {i http://www.junit.org}),
	  parameter being destination. *)
  | Csv_output of out_channel * string
      (** CSV output, parameters being destination and separator. *)
(** The type of output modes, that is how tests result are written.*)


(** {6 Assertion-based tests} *)

val return : 'a -> (unit -> 'a)
(** [return x] returns the function always returning [x]. *)

val make_assert_test : ?title:string -> (unit -> 'a) -> ('a -> 'b) -> ('b -> unit) -> t
(** [make_assert_test ~title:t set_up f tear_down] constructs a test running
    the function [f]. The function is feed by the result of a call to [set_up],
    and the value returned by the function is passed to [tear_down].
    The function is intended to use functions from the [Assertion] module to
    check if computed values are equal to waited ones. *)

val make_simple_test : ?title:string -> (unit -> unit) -> t
(** [make_simple_test ~title:t f] constructs a test running the function [f].
    It is equivalent to [make_assert_test ~title:t (return ()) f ignore].
    The function is intended to use functions from the [Assertion] module to
    check if computed values are equal to waited ones. *)


(** {6 Generator-based tests} *)

val default_classifier : 'a classifier
(** The default classifier, always returning [""]. *)

val make_random_test : ?title:string -> ?nb_runs:int -> ?classifier:'a classifier -> ?random_src:Generator.random -> 'a Generator.t -> ('a -> 'b) -> (('a, 'b) Specification.t) list -> t
(** [make_random_test ~title:t ~nb_runs:nb ~classifier:c ~random_src:rnd gen f spec]
    constructs a random test. [f] is the function to be tested; when run, the
    framework will generate [nb] input values for [f] satisfying [spec] using
    [gen] (an input value satisfies [spec] if it makes one of the preconditions
    evaluate to [true]). The function [f] is then feed with those values,
    and the output is passed to the associated postcondition to check it
    evaluates to [true] (otherwise, a counterexample has been found).
    The classifier [c] is used to group generated elements into categories in
    order to have a better understanding of what the random elements actually
    tested.

    The default values are:
    - a auto-generated ["untitled no X"] string
      ("X" being the value of a counter) for [title];
    - [100] for [nb_runs];
    - [default_classifier] for [classifier];
    - [Generator.make_random ()] for [random_src].

    Raises [Invalid_arg] if [nb] is not strictly positive. *)


(** {6 Shell-based tests} *)

val make_shell_test : ?title:string -> ('a, 'b, 'c) Shell.command list -> t
(** [make_shell_test ~title:t l] constructs a test running the commands from
    [l] using [Shell.run_list]. *)


(** {6 Test runners} *)

val exec_test : t -> result
(** Executes the passed test, returning its outcome. *)

val exec_tests : t list -> result list
(** [exec_tests l] is equivalent to [List.map exec_test l]. *)

val run_test : ?output:output_mode -> t -> unit
(** Runs the passed test, printing result according to passed output mode
    (by default bare text on the standard output).

    The output channel if closed only iff it is neither [stdout], nor [stderr]. *)

val run_tests : ?output:output_mode -> t list -> unit
(** Runs the passed test list, printing result according to passed output mode
    (by default bare text on the standard output).

    The output channel if closed only iff it is neither [stdout], nor [stderr]. *)

val check : ?title:string -> ?nb_runs:int -> ?classifier:'a classifier -> ?random_src:Generator.random -> 'a Generator.t -> ('a -> 'b) -> (('a, 'b) Specification.t) list -> unit
(** [check ...] is equivalent to [run_test (make_random_test ...)]. *)

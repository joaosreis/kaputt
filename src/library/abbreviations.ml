(*
 * This file is part of Kaputt.
 * Copyright (C) 2008-2012 Xavier Clerc.
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


(* Shorthands for modules *)

module Assert = Assertion

module Gen = Generator

module Red = Reducer

module Enum = Enumerator

module Spec = Specification

module Shell = Shell

module Test = Test


(* Shorthands for functions *)

let (=>) = Specification.implies

let (==>) = Specification.implies'

let (&&&) = Specification.logand

let (&&&&) = Specification.logand_list

let (|||) = Specification.logor

let (||||) = Specification.logor_list

let (^^^) = Specification.logxor

let (^^^^) = Specification.logxor_list

let check = Test.check

let check_partial = Test.check_partial

let (|>) = Shell.pipe

let (>>) = Shell.redirect_output

let (>>>) = Shell.redirect_append

let (>>>>) = Shell.redirect_error

let (>>>>>) = Shell.redirect_append_error

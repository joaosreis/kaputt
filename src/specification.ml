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

type 'a predicate = 'a -> bool

type ('a, 'b) t = {
    precond : 'a predicate;
    postcond : ('a * 'b) predicate
  }

let implies p1 p2 =
  { precond = p1; postcond = p2 }

let (=>) = implies

let implies' p1 p2 =
  { precond = p1; postcond = (fun (_, x) -> p2 x) }

let (==>) = implies'


(* Predifined predicates *)

let always _ = true

let never _ = false

let is_pos_int x = x >= 0

let is_neg_int x = x <= 0

let is_zero_int x = x = 0

let is_nonzero_int x = x <> 0

let is_even_int x = (x mod 2) = 0

let is_odd_int x = (x mod 2) <> 0

let is_pos_int32 x = (Int32.compare x Int32.zero) >= 0

let is_neg_int32 x = (Int32.compare x Int32.zero) <= 0

let is_zero_int32 x = x = Int32.zero

let is_nonzero_int32 x = x <> Int32.zero

let is_even_int32 x = (Int32.rem x 2l) = Int32.zero

let is_odd_int32 x = (Int32.rem x 2l) <> Int32.zero

let is_pos_int64 x = (Int64.compare x Int64.zero) >= 0

let is_neg_int64 x = (Int64.compare x Int64.zero) <= 0

let is_zero_int64 x = x = Int64.zero

let is_nonzero_int64 x = x <> Int64.zero

let is_even_int64 x = (Int64.rem x 2L) = Int64.zero

let is_odd_int64 x = (Int64.rem x 2L) <> Int64.zero

let is_pos_nativeint x = (Nativeint.compare x Nativeint.zero) >= 0

let is_neg_nativeint x = (Nativeint.compare x Nativeint.zero) <= 0

let is_zero_nativeint x = x = Nativeint.zero

let is_nonzero_nativeint x = x <> Nativeint.zero

let is_even_nativeint x = (Nativeint.rem x 2n) = Nativeint.zero

let is_odd_nativeint x = (Nativeint.rem x 2n) <> Nativeint.zero

let is_pos_float x = x >= 0.

let is_neg_float x = x <= 0.

let is_zero_float_eps eps x = (abs_float x) <= eps

let is_nonzero_float_eps eps x = (abs_float x) > eps

let is_zero_float = is_zero_float_eps epsilon_float

let is_nonzero_float = is_nonzero_float_eps epsilon_float

let is_nan_float x = x == nan

let is_nonnan_float x = x != nan

let is_letter_char = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false

let is_digit_char = function
  | '0' .. '9' -> true
  | _ -> false

let is_space_char = function
  | ' ' | '\t' -> true
  | _ -> false

let is_alphanum_char x =
  (x = '_') || (is_letter_char x) || (is_digit_char x)

let is_empty_string x = x = ""

let is_nonempty_string x = x <> ""

let is_empty_list x = x = []

let is_nonempty_list x = x <> []

let is_empty_array x = x = [||]

let is_nonempty_array x = x <> [||]

let is_none_option x = x = None

let is_some_option x = x <> None


(* Predicates over containers *)

let exists_string p x =
  let i = ref 0 in
  let l = String.length x in
  while (!i < l) && not (p x.[!i]) do
    incr i
  done;
  !i < l

let for_all_string p x =
  let i = ref 0 in
  let l = String.length x in
  while (!i < l) && (p x.[!i]) do
    incr i
  done;
  !i = l

let exists_list = List.exists

let for_all_list = List.for_all

let exists_array p x =
  let i = ref 0 in
  let l = Array.length x in
  while (!i < l) && not (p x.(!i)) do
    incr i
  done;
  !i < l

let for_all_array p x =
  let i = ref 0 in
  let l = Array.length x in
  while (!i < l) && (p x.(!i)) do
    incr i
  done;
  !i = l

exception Return

module type Pred = sig
  type p
  val p : p predicate
end

module Map (M : Map.S) (P : Pred with type p = M.key) = struct
  let exists p m =
    try
      M.iter (fun k v -> if (P.p k) && (p v) then raise Return) m;
      false
    with Return -> true
  let for_all p m =
    try
      M.iter (fun k v -> if not ((P.p k) && (p v)) then raise Return) m;
      true
    with Return -> false
end

module Set (S : Set.S) (P : Pred with type p = S.elt) = struct
  let exists s =
    try
      S.iter (fun e -> if P.p e then raise Return) s;
      false
    with Return -> true
  let for_all s =
    try
      S.iter (fun e -> if not (P.p e) then raise Return) s;
      true
    with Return -> false
end

let exists_hashtbl p x =
  try
    Hashtbl.iter (fun k v -> if p (k, v) then raise Return) x;
    false
  with Return -> true

let for_all_hashtbl p x =
  try
    Hashtbl.iter (fun k v -> if not (p (k, v)) then raise Return) x;
    true
  with Return -> false

let exists_queue p x =
  try
    Queue.iter (fun e -> if p e then raise Return) x;
    false
  with Return -> true

let for_all_queue p x =
  try
    Queue.iter (fun e-> if not (p e) then raise Return) x;
    true
  with Return -> false

let exists_stack p x =
  try
    Stack.iter (fun e -> if p e then raise Return) x;
    false
  with Return -> true

let for_all_stack p x =
  try
    Stack.iter (fun e-> if not (p e) then raise Return) x;
    true
  with Return -> false

let exists_weak p x =
  let i = ref 0 in
  let l = Weak.length x in
  while (!i < l) && not (p (Weak.get x !i)) do
    incr i
  done;
  !i < l

let for_all_weak p x =
  let i = ref 0 in
  let l = Weak.length x in
  while (!i < l) && (p (Weak.get x !i)) do
    incr i
  done;
  !i = l

module Weak (W : Weak.S) (P : Pred with type p = W.data) = struct
  let exists s =
    try
      W.iter (fun e -> if P.p e then raise Return) s;
      false
    with Return -> true
  let for_all s =
    try
      W.iter (fun e -> if not (P.p e) then raise Return) s;
      true
    with Return -> false
end


(* Combinators over predicates *)

let logand p1 p2 =
  fun x -> (p1 x) && (p2 x)

let (&&&) = logand

let logor p1 p2 =
  fun x -> (p1 x) || (p2 x)

let (|||) = logor

let logxor p1 p2 =
    fun x -> let r1 = p1 x and r2 = p2 x in (r1 && (not r2)) || ((not r1) && r2)

let (^^^) = logxor

let not p =
  fun x -> not (p x)

external zip1 : 'a predicate -> 'a predicate = "%identity"

let zip2 p1 p2 =
  fun (x, y) -> (p1 x) && (p2 y)

let zip3 p1 p2 p3 =
  fun (x, y, z) -> (p1 x) && (p2 y) && (p3 z)

let zip4 p1 p2 p3 p4 =
  fun (x, y, z, t) -> (p1 x) && (p2 y) && (p3 z) && (p4 t)

let zip5 p1 p2 p3 p4 p5 =
  fun (x, y, z, t, u) -> (p1 x) && (p2 y) && (p3 z) && (p4 t) && (p5 u)

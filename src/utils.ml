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


(* Conversion utilities *)

let string_of_string s =
  "\"" ^ (String.escaped s) ^ "\""

let string_of_complex x =
  Printf.sprintf "%f+%fi" x.Complex.re x.Complex.im


(* Bigarray utilities *)

exception End_of_array

let iterator ba =
  let is_c_layout = Bigarray.Genarray.layout ba = (Obj.magic Bigarray.c_layout) in
  let dims = Bigarray.Genarray.dims ba in
  let nb_dims = Array.length dims in
  let coords =
    if is_c_layout then
      Array.make nb_dims 0
    else
      Array.make nb_dims 1 in
  let next () =
    if is_c_layout then begin
      let res = ref 0 in
      let i = ref (pred nb_dims) in
      while (!i >= 0) && (coords.(!i) = (pred dims.(!i))) do
        coords.(!i) <- 0;
        incr res;
        decr i
      done;
      if !i >= 0 then begin
        coords.(!i) <- succ coords.(!i);
        !res
      end else
        raise End_of_array
    end else begin
      let res = ref 0 in
      let i = ref 0 in
      while (!i < nb_dims) && (coords.(!i) = dims.(!i)) do
        coords.(!i) <- 1;
        incr res;
        incr i
      done;
      if !i < nb_dims then begin
        coords.(!i) <- succ coords.(!i);
        !res
      end else
        raise End_of_array
    end in
  coords, next

let bigarray_iter f ba =
  let coords, next = iterator ba in
  try
    while true do
      f (Bigarray.Genarray.get ba coords);
      ignore (next ())
    done
  with End_of_array -> ()

let bigarray_iteri f ba =
  let coords, next = iterator ba in
  try
    while true do
      f (Array.copy coords) (Bigarray.Genarray.get ba coords);
      ignore (next ())
    done
  with End_of_array -> ()

let string_of_bigarray f ba =
  let buf = Buffer.create 16 in
  let nb_dims = Bigarray.Genarray.num_dims ba in
  let last = ref nb_dims in
  let print s =
    for i = 1 to !last do
      Buffer.add_string buf s
    done in
  let coords, next = iterator ba in
  try
    while true do
      print "[| ";
      let x = Bigarray.Genarray.get ba coords in
      Buffer.add_string buf (try f x with _ -> "?");
      Buffer.add_string buf "; ";
      last := next ();
      print "|]; "
    done;
    assert false
  with End_of_array ->
    last := nb_dims;
    print "|]; ";
    let len = Buffer.length buf in
    Buffer.sub buf 0 (len - 2)

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


(* Bigarray utilities *)

exception End_of_array

(* returns a couple where the first component is the coordinates of the first
   element of the array, while the second component is a function that will
   update the coordinates to point to the next element of the array (raising
   End_of_array if the coordinates already designate the last element of the
   array). *)
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

(* [bigarray_iter f a] applies [f] in turn to all elements of [a]. *)
let bigarray_iter f ba =
  let coords, next = iterator ba in
  try
    while true do
      f (Bigarray.Genarray.get ba coords);
      ignore (next ())
    done
  with End_of_array -> ()

(* Same as [bigarray_iter], except that the function also receives the coordinates of the
   elements as the first argument. *)
let bigarray_iteri f ba =
  let coords, next = iterator ba in
  try
    while true do
      f (Array.copy coords) (Bigarray.Genarray.get ba coords);
      ignore (next ())
    done
  with End_of_array -> ()

(* [string_of_bigarray f a] converts [a] into a string, using [f] to convert each element. *)
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


(* Generator *)

module Generator = struct

  let bigarray k l (gen_dims, _) (gen_e, prn_e) =
    (fun r ->
      let dims = gen_dims r in
      let res = Bigarray.Genarray.create k l dims in
      bigarray_iteri
        (fun c _ ->
          let e = gen_e r in
          Bigarray.Genarray.set res c e)
        res;
      res),
    (string_of_bigarray prn_e)

end


(* Enumerator *)

module Enumerator = struct

  let bigarray k l dims elem =
    let sz =
      Array.fold_left
        (fun acc elem ->
          if elem < 0  then
            invalid_arg "KaputtBigarray.Enumerator.bigarray"
          else
            acc * elem)
        1
        dims in
    Kaputt.Enumerator.create_state_based
      (fun () -> Array.init sz (fun _ -> elem))
      (fun s ->
        let res = Bigarray.Genarray.create k l dims in
        let i = ref 0 in
        bigarray_iteri
          (fun c _ ->
            Bigarray.Genarray.set res c (Kaputt.Enumerator.State.get s !i);
            incr i)
          res;
        res)
      (string_of_bigarray (snd elem))

end

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


(* Random source *)

type random = Random.State.t

let make_random () = Random.State.make_self_init ()


(* Generator definition *)

type 'a t = (random -> 'a) * ('a -> string)


(* Predefined generators *)

let unit =
  (fun r -> ignore (Random.State.bool r)),
  (fun () -> String.copy "()")

let bool =
  (fun r -> Random.State.bool r),
  string_of_bool

let make_bool w1 w2 =
  if w1 < 0 then invalid_arg "Kaputt.Generator.make_bool";
  if w2 < 0 then invalid_arg "Kaputt.Generator.make_bool";
  (fun r ->
    let w = Random.State.int r (w1 + w2) in
    w < w1),
  string_of_bool

let int_sign = 0b1000000000000000000000000000000

let int =
  (fun r ->
    let s = Random.State.bool r in
    let x = Random.State.bits r in
    if s then x else (x lor int_sign)),
  string_of_int

let pos_int =
  (fun r -> Random.State.bits r),
  string_of_int

let neg_int =
  (fun r -> (Random.State.bits r) lor int_sign),
  string_of_int

let make_int x y =
  if (compare x y) >= 0 then invalid_arg "Kaputt.Generator.make_int";
  (fun r -> let d = y - x in (Random.State.int r d) + x),
  string_of_int

let int32 =
  (fun r ->
    let s = Random.State.bool r in
    let x = Random.State.int32 r Int32.max_int in
    if s then x else Int32.sub (Int32.neg x) Int32.one),
  Int32.to_string

let pos_int32 =
  (fun r -> Random.State.int32 r Int32.max_int),
  Int32.to_string

let neg_int32 =
  (fun r -> Int32.sub (Int32.neg (Random.State.int32 r Int32.max_int)) Int32.one),
  Int32.to_string

let make_int32 x y =
  if (compare x y) >= 0 then invalid_arg "Kaputt.Generator.make_int32";
  (fun r -> let d = Int32.sub y x in Int32.add (Random.State.int32 r d) x),
  Int32.to_string

let int64 =
  (fun r ->
    let s = Random.State.bool r in
    let x = Random.State.int64 r Int64.max_int in
    if s then x else Int64.sub (Int64.neg x) Int64.one),
  Int64.to_string

let pos_int64 =
  (fun r -> Random.State.int64 r Int64.max_int),
  Int64.to_string

let neg_int64 =
  (fun r -> Int64.sub (Int64.neg (Random.State.int64 r Int64.max_int)) Int64.one),
  Int64.to_string

let make_int64 x y =
  if (compare x y) >= 0 then invalid_arg "Kaputt.Generator.make_int64";
  (fun r -> let d = Int64.sub y x in Int64.add (Random.State.int64 r d) x),
  Int64.to_string

let nativeint =
  (fun r ->
    let s = Random.State.bool r in
    let x = Random.State.nativeint r Nativeint.max_int in
    if s then x else Nativeint.neg x),
  Nativeint.to_string

let pos_nativeint =
  (fun r -> Random.State.nativeint r Nativeint.max_int),
  Nativeint.to_string

let neg_nativeint =
  (fun r -> Nativeint.neg (Random.State.nativeint r Nativeint.max_int)),
  Nativeint.to_string

let make_nativeint x y =
  if (compare x y) >= 0 then invalid_arg "Kaputt.Generator.make_nativeint";
  (fun r -> let d = Nativeint.sub y x in Nativeint.add (Random.State.nativeint r d) x),
  Nativeint.to_string

let char =
  (fun r -> Char.chr (Random.State.int r 256)),
  Char.escaped

let digit =
  (fun r -> Char.chr ((Random.State.int r 10) + (Char.code '0'))),
  Char.escaped

let letter =
  (fun r ->
    let c = Random.State.bool r in
    let l = Random.State.int r 26 in
    let x = Char.chr (l + (Char.code 'a')) in
    if c then Char.uppercase x else x),
  Char.escaped

let alphanum =
  (fun r ->
    let x = Random.State.int r 63 in
    if x < 10 then
      Char.chr (x + (Char.code '0'))
    else if x = 10 then
      '_'
    else if (x >= 11) && (x <= 36) then
      Char.chr ((x - 11) + (Char.code 'a'))
    else
      Char.chr ((x - 37) + (Char.code 'A'))),
  Char.escaped

let string (gen_l, _) (gen_c, _) =
  (fun r ->
    let len = gen_l r in
    let res = String.create len in
    for i = 0 to pred len do
      res.[i] <- gen_c r
    done;
    res),
  (fun x -> x)

let strings sep (gen_l, _) (gen_s, _) =
  (fun r ->
    let len = gen_l r in
    let lst = ref [] in
    for i = 1 to len do
      lst := (gen_s r) :: !lst
    done;
    String.concat sep (List.rev !lst)),
  (fun x -> x)

let number l =
  string l digit

let word l =
  string l letter

let words l l' =
  strings " " l (word l')

let float =
  (fun r ->
    let s = Random.State.bool r in
    let x = Random.State.float r max_float in
    if s then x else -.x),
  string_of_float

let make_float x y =
  if (compare x y) >= 0 then invalid_arg "Kaputt.Generator.make_float";
  (fun r -> let d = y -. x in (Random.State.float r d) +. x),
  string_of_float

let complex (gen_re, _) (gen_im, _) =
  (fun r ->
    let re_val = gen_re r in
    let im_val = gen_im r in
    { Complex.re = re_val; Complex.im = im_val }),
  (fun x -> Printf.sprintf "%f+%fi" x.Complex.re x.Complex.im)

let gen_big_int_digit, _ = make_int 0 10

let big_int (gen_l, _) =
  (fun r ->
    let s = Random.State.bool r in
    let len = gen_l r in
    let res = ref Big_int.zero_big_int in
    for i = 1 to len do
      res := Big_int.add_int_big_int
          (gen_big_int_digit r)
          (Big_int.mult_int_big_int  10 !res)
    done;
    if s then !res else Big_int.minus_big_int !res),
  Big_int.string_of_big_int

let num (gen_a, _) (gen_b, _) =
  (fun r ->
    let a = gen_a r in
    let b = gen_b r in
    Num.div_num (Num.Big_int a) (Num.Big_int b)),
  Num.string_of_num


(* Generators for containers *)

let array (gen_l, _) (gen_e, prn_e) =
  (fun r ->
    let len = gen_l r in
    Array.init len (fun _ -> gen_e r)),
  (fun a ->
    let buf = Buffer.create 16 in
    Buffer.add_string buf "[| ";
    Array.iter
      (fun x ->
        Buffer.add_string buf (prn_e x);
        Buffer.add_string buf "; ")
      a;
    Buffer.add_string buf "|]";
    Buffer.contents buf)

let list (gen_l, _) (gen_e, prn_e) =
  (fun r ->
    let len = gen_l r in
    let res = ref [] in
    for i = 1 to len do
      res := (gen_e r) :: !res;
    done;
    List.rev !res),
  (fun l ->
    let buf = Buffer.create 16 in
    Buffer.add_string buf "[ ";
    List.iter
      (fun x ->
        Buffer.add_string buf (prn_e x);
        Buffer.add_string buf "; ")
      l;
    Buffer.add_string buf "]";
    Buffer.contents buf)

let option (gen_k, _) (gen_e, prn_e) =
  (fun r ->
    let b = gen_k r in
    if b then
      Some (gen_e r)
    else
      None),
  (function
  | None -> "None"
  | Some v -> "Some (" ^ (prn_e v) ^ ")")

let ref (gen_e, prn_e) =
  (fun r -> ref (gen_e r)),
  (fun x -> "ref (" ^ (prn_e !x) ^ ")")    

let buffer (gen_e, _) =
  (fun r ->
    let buf = Buffer.create 16 in
    Buffer.add_string buf (gen_e r);
    buf),
  (fun x -> Buffer.contents x)

module type Gen = sig
  type g
  val g : g t
end

module Map (M : Map.S) (G : Gen with type g = M.key) = struct
  let gen (gen_l, _) (gen_v, prn_v) =
    let (gen_k, prn_k) = G.g in
    (fun r ->
      let len = gen_l r in
      let res = Pervasives.ref M.empty in
      let size = Pervasives.ref 0 in
      while !size < len do
        let k = gen_k r in
        if not (M.mem k !res) then begin
          let v = gen_v r in
          res := M.add k v !res;
          incr size
        end
      done;
      !res),
    (fun m ->
      let l = M.fold
          (fun k v acc -> (Printf.sprintf "%s -> %s" (prn_k k) (prn_v v)) :: acc)
          m
          [] in
      String.concat "; " (List.rev l))
end

module Set (S : Set.S) (G : Gen with type g = S.elt) = struct
  let gen (gen_l, _) =
    let (gen_e, prn_e) = G.g in
    (fun r ->
      let len = gen_l r in
      let res = Pervasives.ref S.empty in
      let size = Pervasives.ref 0 in
      while !size < len do
        let e = gen_e r in
        if not (S.mem e !res) then begin
          res := S.add e !res;
          incr size
        end
      done;
      !res),
    (fun s ->
      let l = S.fold (fun e acc -> (prn_e e) :: acc) s [] in
      String.concat "; " (List.rev l))
end

let hashtbl (gen_l, _) (gen_k, prn_k) (gen_v, prn_v) =
  (fun r ->
    let len = gen_l r in
    let res = Hashtbl.create len in
    let size = Pervasives.ref 0 in
    while !size < len do
      let k = gen_k r in
      if not (Hashtbl.mem res k) then begin
        let v = gen_v r in
        Hashtbl.add res k v;
        incr size
      end
    done;
    res),
  (fun h ->
    let l = Hashtbl.fold
        (fun k v acc -> (Printf.sprintf "%s -> %s" (prn_k k) (prn_v v)) :: acc)
        h
        [] in
    String.concat "; " (List.rev l))

let queue (gen_l, _) (gen_e, prn_e) =
  (fun r ->
    let len = gen_l r in
    let res = Queue.create () in
    for i = 1 to len do
      let e = gen_e r in
      Queue.push e res
    done;
    res),
  (fun s ->
    let buf = Buffer.create 16 in
    Queue.iter (fun e -> Buffer.add_string buf (prn_e e)) s;
    Buffer.contents buf)

let stack (gen_l, _) (gen_e, prn_e) =
  (fun r ->
    let len = gen_l r in
    let res = Stack.create () in
    for i = 1 to len do
      let e = gen_e r in
      Stack.push e res
    done;
    res),
  (fun s ->
    let buf = Buffer.create 16 in
    Stack.iter (fun e -> Buffer.add_string buf (prn_e e)) s;
    Buffer.contents buf)

let weak (gen_l, _) (gen_e, prn_e) =
  (fun r ->
    let len = gen_l r in
    let res = Weak.create len in
    for i = 0 to (pred len) do
      let e = gen_e r in
      Weak.set res i e
    done;
    res),
  (fun w ->
    let buf = Buffer.create 16 in
    let len = Weak.length w in
    Buffer.add_string buf "[|| ";
    for i = 0 to (pred len) do
      Buffer.add_string buf (prn_e (Weak.get w i));
      Buffer.add_string buf "; ";
    done;
    Buffer.add_string buf "||]";
    Buffer.contents buf)

module Weak (W : Weak.S) (G : Gen with type g = W.data) = struct
  let gen (gen_l, _) =
    let (gen_e, prn_e) = G.g in
    (fun r ->
      let len = gen_l r in
      let res = W.create len in
      let size = Pervasives.ref 0 in
      while !size < len do
        let e = gen_e r in
        if not (W.mem res e) then begin
          W.add res e;
          incr size
        end
      done;
      res),
    (fun w ->
      let l = W.fold (fun e acc -> (prn_e e) :: acc) w [] in
      String.concat "; " (List.rev l))
end

let bigarray k l (gen_dims, _) (gen_e, prn_e) =
  (fun r ->
    let dims = gen_dims r in
    let res = Bigarray.Genarray.create k l dims in
    Utils.bigarray_iteri
      (fun c _ ->
        let e = gen_e r in
        Bigarray.Genarray.set res c e)
      res;
    res),
  (fun b -> Utils.string_of_bigarray prn_e b)


(* Combinators over generators *)

let sum_list = List.fold_left
    (fun acc elem ->
      if elem >= 0 then
        acc + elem
      else
        invalid_arg "negative weight")
    0

let sum_array = Array.fold_left
    (fun acc elem ->
      if elem >= 0 then
        acc + elem
      else
        invalid_arg "negative weight")
    0

let lift x s =
  let s' = String.copy s in
  (fun _ -> x),
  (fun _ -> String.copy s')

let select_list l f =
  if l = [] then invalid_arg "Kaputt.Generator.select_list";
  (fun r ->
    let len = List.length l in
    let i = Random.State.int r len in
    List.nth l i),
  f

let rec get_list n = function
  | [(v, _)] -> v
  | (v, w) :: _ when n < w -> v
  | (_, w) :: tl -> get_list (n - w) tl
  | [] -> failwith "internal error"

let select_list_weighted l f =
  if l = [] then invalid_arg "Kaputt.Generator.select_list_weighted";
  let total = sum_list (List.map snd l) in
  (fun r ->
    let w = Random.State.int r total in
    get_list w l),
  f

let select_array a f =
  if a = [||] then invalid_arg "Kaputt.Generator.select_array";
  (fun r ->
    let len = Array.length a in
    let i = Random.State.int r len in
    a.(i)),
  f

let rec get_array n a i =
  let w = snd a.(i) in
  if (n < w) || (i = pred (Array.length a)) then
    fst a.(i)
  else
    get_array (n - w) a (succ i)

let select_array_weighted a f =
  if a = [||] then invalid_arg "Kaputt.Generator.select_array_weighted";
  let total = sum_array (Array.map snd a) in
  (fun r ->
    let w = Random.State.int r total in
    get_array w a 0),
  f

let choose_list l =
  if l = [] then invalid_arg "Kaputt.Generator.choose_list";
  (fun r ->
    let len = List.length l in
    let i = Random.State.int r len in
    let (gen, _) = List.nth l i in
    gen r),
  (snd (List.hd l))

let choose_list_weighted l =
  if l = [] then invalid_arg "Kaputt.Generator.choose_list_weighted";
  let total = sum_list (List.map snd l) in
  (fun r ->
    let w = Random.State.int r total in
    let (gen, _) = get_list w l in
    gen r),
  (snd (fst (List.hd l)))

let choose_array a =
  if a = [||] then invalid_arg "Kaputt.Generator.choose_array";
  (fun r ->
    let len = Array.length a in
    let i = Random.State.int r len in
    let (gen, _) = a.(i) in
    gen r),
  (snd a.(0))

let choose_array_weighted a =
  if a = [||] then invalid_arg "Kaputt.Generator.choose_array_weighted";
    let total = sum_array (Array.map snd a) in
  (fun r ->
    let w = Random.State.int r total in
    let (gen, _) = get_array w a 0 in
    gen r),
  (snd (fst a.(0)))

let filter p (gen_g, prn_g) =
  (fun r ->
    let rec get () =
      let x = gen_g r in
      if p x then x else get () in
    get ()),
  prn_g

let transform f (gen_g, prn_g) =
  (fun r -> f (gen_g r)),
  prn_g

let map1 f p (gen_g, _) =
  (fun r -> f (gen_g r)),
  p

let map2 f p ((gen_g1, _), (gen_g2, _)) =
  (fun r -> f (gen_g1 r) (gen_g2 r)),
  p

let map3 f p ((gen_g1, _), (gen_g2, _), (gen_g3, _)) =
  (fun r -> f (gen_g1 r) (gen_g2 r) (gen_g3 r)),
  p

let map4 f p ((gen_g1, _), (gen_g2, _), (gen_g3, _), (gen_g4, _)) =
  (fun r -> f (gen_g1 r) (gen_g2 r) (gen_g3 r) (gen_g4 r)),
  p

let map5 f p ((gen_g1, _), (gen_g2, _), (gen_g3, _), (gen_g4, _), (gen_g5, _)) =
  (fun r -> f (gen_g1 r) (gen_g2 r) (gen_g3 r) (gen_g4 r) (gen_g5 r)),
  p

external zip1 : 'a t -> 'a t = "%identity"

let zip2 (f1, c1) (f2, c2) =
  (fun r -> (f1 r), (f2 r)),
  (fun (x, y) -> Printf.sprintf "(%s, %s)" (c1 x) (c2 y))

let zip3 (f1, c1) (f2, c2) (f3, c3) =
  (fun r -> (f1 r), (f2 r), (f3 r)),
  (fun (x, y, z) -> Printf.sprintf "(%s, %s, %s)" (c1 x) (c2 y) (c3 z))

let zip4 (f1, c1) (f2, c2) (f3, c3) (f4, c4) =
  (fun r -> (f1 r), (f2 r), (f3 r), (f4 r)),
  (fun (x, y, z, t) -> Printf.sprintf "(%s, %s, %s, %s)" (c1 x) (c2 y) (c3 z) (c4 t))

let zip5 (f1, c1) (f2, c2) (f3, c3) (f4, c4) (f5, c5) =
  (fun r -> (f1 r), (f2 r), (f3 r), (f4 r), (f5 r)),
  (fun (x, y, z, t, u) -> Printf.sprintf "(%s, %s, %s, %s, %s)" (c1 x) (c2 y) (c3 z) (c4 t) (c5 u))


(* Currying functions *)

let apply1 f x = f x

let apply2 f (x, y) = f x y

let apply3 f (x, y, z) = f x y z

let apply4 f (x, y, z, t) = f x y z t

let apply5 f (x, y, z, t, u) = f x y z t u

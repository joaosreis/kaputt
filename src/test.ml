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
  | Uncaught of exn * string
  | Report of int * int * int * (string list) * ((string * int) list)
  | Exit_code of int

type 'a classifier = 'a -> string

type t = string * (unit -> result)

type output_mode =
  | Text_output of out_channel
  | Html_output of out_channel
  | Xml_output of out_channel
  | Csv_output of out_channel * string


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
    | e -> Uncaught (e, Printexc.get_backtrace ()))

let return_unit = return ()

let make_simple_test ?(title=get_title ()) f =
  make_assert_test ~title:title return_unit f ignore


(** {6 Generator-based tests} *)

let default_classifier _ = ""

let make_random_test ?(title=get_title ()) ?(nb_runs=100) ?(classifier=default_classifier) ?(random_src=Generator.make_random ()) (gen, prn) f spec =
  let dummy_post _ = false in
  let rec extract x = function
    | hd :: tl ->
        if hd.Specification.precond x then
          hd.Specification.postcond
        else
          extract x tl
    | [] -> dummy_post in
  if nb_runs <= 0 then invalid_arg "Kaputt.Test.make_random_test";
  title,
  (fun () ->
    let valid = ref 0 in
    let uncaught = ref 0 in
    let counterexamples = ref [] in
    let categories = Hashtbl.create 16 in
    for i = 1 to nb_runs do
      let x = ref (gen random_src) in
      let post = ref (extract !x spec) in
      while !post == dummy_post do
        let tmp = gen random_src in
        x := tmp;
        post := extract tmp spec
      done;
      try
        let y = f !x in
        let cat = classifier !x in
        let curr = try Hashtbl.find categories cat with _ -> 0 in
        Hashtbl.replace categories cat (succ curr);
        if !post (!x, y) then
          incr valid
        else
          let x' = prn !x in
          if not (List.mem x' !counterexamples) then
            counterexamples := x' :: !counterexamples
      with _ -> incr uncaught
    done;
    let categories' = Hashtbl.fold (fun k v acc -> (k, v) :: acc) categories [] in
    Report (!valid, nb_runs, !uncaught, (List.rev !counterexamples), categories'))


(* Shell-based tests *)

let make_shell_test ?(title=get_title ()) l =
  title,
  (fun () ->
    Exit_code (try Shell.run_list l with _ -> min_int))


(* Test runners *)

let exec_test (_, func) =
  func ()

let exec_tests = List.map exec_test

let escape s =
  let buff = Buffer.create (String.length s) in
  String.iter
    (function
      | '<' -> Buffer.add_string buff "&lt;"
      | '>' -> Buffer.add_string buff "&gt;"
      | '\"' -> Buffer.add_string buff "&quot;"
      | '&' -> Buffer.add_string buff "&amp;"
      | '\t' -> for i = 1 to 4 do Buffer.add_string buff "&nbsp;" done
      | ch -> Buffer.add_char buff ch)
    s;
  Buffer.contents buff

let version = "1.0-beta"

let safe_close out =
  if (out != stdout) && (out != stderr) then close_out_noerr out

let make_output kind =
  let now = Unix.localtime (Unix.time ()) in
  let timestamp =
    Printf.sprintf "%d-%02d-%02d %02d:%02d:%02d"
      (1900 + now.Unix.tm_year)
      (succ now.Unix.tm_mon)
      now.Unix.tm_mday
      now.Unix.tm_hour
      now.Unix.tm_min
      now.Unix.tm_sec in
  match kind with
  | Text_output out ->
      object
        method header _ _ _ _ = ()
        method footer = ()
        method result name res =
          match res with
          | Passed ->
              Printf.fprintf out "Test '%s' ... passed\n" name
          | Failed (expected, actual, "") ->
              Printf.fprintf out "Test '%s' ... failed\n  expected `%s` but received `%s`\n" name expected actual
          | Failed (expected, actual, message) ->
              Printf.fprintf out "Test '%s' ... failed\n  %s (expected `%s` but received `%s`)\n" name message expected actual
          | Uncaught (e, bt) ->
              Printf.fprintf out "Test '%s' ... raised an exception\n  %s\n%s\n" name (Printexc.to_string e) bt
          | Report (valid, total, uncaught, counterexamples, categories) ->
              Printf.fprintf out "Test '%s' ... %d/%d case%s passed%s\n"
                name
                valid
                total
                (if valid > 1 then "s" else "")
                (match uncaught with
                | 0 -> ""
                | 1 -> " (1 uncaught exception)"
                | n -> " (" ^ (string_of_int n) ^ " uncaught exceptions)");
              if counterexamples <> [] then
                Printf.fprintf out "  counterexample%s: %s\n"
                  (if (List.length counterexamples) > 1 then "s" else "")
                  (String.concat ", " counterexamples);
              if (List.length categories) > 1 then begin
                Printf.fprintf out "  categories:\n";
                List.iter
                  (fun (c, n) ->
                    Printf.fprintf out "    %s -> %d occurrence%s\n" c n (if n > 1 then "s" else ""))
                  categories
              end
          | Exit_code c ->
              Printf.fprintf out "Test '%s' ... returned code %d\n" name c
        method close = safe_close out
      end
  | Html_output out ->
      let output_lines =
        List.iter
          (fun x ->
            output_string out x;
            output_char out '\n') in
      object
        method header _ _ _ _ =
          output_lines
            [ "<html>";
              "<head>";
              "<title>Kaputt report</title>";
              "<style type=\"text/css\">";
              "table.sample {";
              "  border-width: 1px;";
              "  border-style: solid;";
              "  border-color: black;";
              "  border-collapse: collapse;";
              "  background-color: white;";
              "}";
              "table.sample th {";
              "  border-width: 1px;";
              "  padding: 3px;";
              "  border-style: solid;";
              "  border-color: black;";
              "  background-color: white;";
              "}";
              "table.sample td {";
              "  border-width: 1px;";
              "  padding: 3px;";
              "  border-style: solid;";
              "  border-color: black;";
              "  background-color: white;";
              "}";
              "</style>";
              "</head>";
              "<body>";
              "<p align=\"center\">";
              "<table class=\"sample\" width=\"85%\">";
              ("<tr>" ^
               "<th width=\"25%\">Test kind</th>" ^
               "<th width=\"25%\">Test name</th>" ^
               "<th>Test outcome</th>" ^
               "</tr>") ]
        method footer =
          output_lines
            [  "</table>";
               "<p style=\"font-size: smaller; text-align: center;\">Generated by <a href=\"http://kaputt.x9c.fr\">Kaputt</a> " ^ version ^ " on " ^ timestamp ^ "</p>";
              "</body>";
              "</html>" ]
        method result name res =
          let output_strings = List.iter (output_string out) in
          match res with
          | Passed ->
              output_strings
                [ "<tr style=\"font-family: monospace;\">";
                  "<td align=\"center\">Assertion-based</td>";
                  "<td align=\"center\">"; name; "</td>";
                  "<td>passed</td>";
                  "</tr>\n" ]
          | Failed (expected, actual, message) ->
              output_strings
                [ "<tr style=\"font-family: monospace;\">";
                  "<td align=\"center\">Assertion-based</td>";
                  "<td align=\"center\">"; name; "</td>" ];
              flush out;
              Printf.fprintf out "<td>failed - expected '%s' but received '%s'%s"
                (escape expected)
                (escape actual)
                (if message = "" then "" else ("<br/>" ^ (escape message)));
              flush out;
              output_strings [ "</td></tr>\n" ]
          | Uncaught (e, bt) ->
              output_strings
                [ "<tr style=\"font-family: monospace;\">";
                  "<td align=\"center\">Assertion-based</td>";
                  "<td align=\"center\">"; name; "</td>" ];
              flush out;
              Printf.fprintf out "<td>raised an exception: %s%s"
                (escape (Printexc.to_string e))
                (if bt = "" then "" else ("<br/>" ^ (escape bt)));
              flush out;
              output_strings [ "</td></tr>\n" ]
          | Report (valid, total, uncaught, counterexamples, categories) ->
              output_strings
                [ "<tr style=\"font-family: monospace;\">";
                  "<td align=\"center\">Random-based</td>";
                  "<td align=\"center\">"; name; "</td>";
                  "<td>" ];
              flush out;
              Printf.fprintf out "%d/%d case%s passed%s\n"
                valid
                total
                (if valid > 1 then "s" else "")
                (match uncaught with
                | 0 -> ""
                | 1 -> " (1 uncaught exception)"
                | n -> " (" ^ (string_of_int n) ^ " uncaught exceptions)");
              if counterexamples <> [] then
                Printf.fprintf out "<br/>&nbsp;&nbsp;counterexample%s: %s\n"
                  (if (List.length counterexamples) > 1 then "s" else "")
                  (String.concat ", " counterexamples);
              if (List.length categories) > 1 then begin
                output_strings ["<br/>&nbsp;&nbsp;categories:"];
                flush out;
                List.iter
                  (fun (c, n) ->
                    Printf.fprintf out "<br/>&nbsp;&nbsp;&nbsp;&nbsp;%s -> %d occurrence%s\n" c n (if n > 1 then "s" else ""))
                  categories
              end;
              flush out;
              output_strings [ "</td></tr>\n" ]
          | Exit_code c ->
              output_strings
                [ "<tr style=\"font-family: monospace;\">";
                  "<td align=\"center\">Shell-based</td>";
                  "<td align=\"center\">"; name; "</td>";
                  "<td>return code: " ^ (string_of_int c) ^ "</td>";
                  "</tr>\n" ]
        method close = safe_close out
      end
  | Xml_output out ->
      object
        method header _ _ _ _ =
          output_string out "<kaputt-report>\n"
        method footer =
          output_string out "</kaputt-report>\n"
        method result name res =
          match res with
          | Passed ->
              Printf.fprintf out "  <passed-test name=\"%s\"/>\n" (escape name)
          | Failed (expected, actual, "") ->
              Printf.fprintf out "  <failed-test name=\"%s\" expected=\"%s\" actual=\"%s\"/>\n"
                (escape name)
                (escape expected)
                (escape actual)
          | Failed (expected, actual, message) ->
              Printf.fprintf out "  <failed-test name=\"%s\" expected=\"%s\" actual=\"%s\" message=\"%s\"/>\n"
                (escape name)
                (escape expected)
                (escape actual)
                (escape message)
          | Uncaught (e, bt) ->
              Printf.fprintf out "  <uncaught-exception name=\"%s\" exception=\"%s\">\n"
                (escape name)
                (escape (Printexc.to_string e));
              output_string out (escape bt);
              output_string out "  </uncaught-exception>"
          | Report (valid, total, uncaught, counterexamples, categories) ->
              Printf.fprintf out "  <random-test name=\"%s\" valid=\"%d\" total=\"%d\" uncaught=\"%d\">\n"
                (escape name)
                valid
                total
                uncaught;
              if counterexamples <> [] then begin
                output_string out "    <counterexamples>\n";
                List.iter
                  (fun x -> Printf.fprintf out "      <counterexample value=\"%s\"/>\n" (escape x))
                  counterexamples;
                output_string out "    </counterexamples>\n"
              end;
              if (List.length categories) > 1 then begin
                output_string out "    <categories>\n";
                List.iter
                  (fun (c, n) -> Printf.fprintf out "      <category name=\"%s\" total=\"%d\"/>\n" (escape c) n)
                  categories;
                output_string out "    </categories>\n"
              end;
              output_string out "  </random-test>\n";
          | Exit_code c ->
              Printf.fprintf out "  <shell-test name=\"%s\" exit-code=\"%d\"/>\n" (escape name) c
        method close = safe_close out
      end
  | Csv_output (out, sep) ->
      object
        method header _ _ _ _ = ()
        method footer = ()
        method result name res =
          let output_strings = List.iter (output_string out) in
          match res with
          | Passed ->
              output_strings ["passed-test"; sep; name; "\n"]
          | Failed (expected, actual, message) ->
              output_strings [ "failed-test"; sep;
                               name; sep;
                               expected; sep;
                               actual; "\n" ];
              if message <> "" then  output_strings [sep; message]
          | Uncaught (e, _) ->
              output_strings ["uncaught-exception"; sep; name; sep; (Printexc.to_string e); "\n"]
          | Report (valid, total, uncaught, counterexamples, categories) ->
              output_strings [ "random-test (stats)"; sep;
                               name; sep;
                               (string_of_int valid); sep;
                               (string_of_int total); sep;
                               (string_of_int uncaught); "\n" ];
              if counterexamples <> [] then
                output_strings [ "random-test (counterexamples)"; sep;
                                 name; sep;
                                 (String.concat sep counterexamples); "\n" ];
              if (List.length categories) > 1 then begin
                output_strings [ "random-test (categories)"; sep; name];
                List.iter
                  (fun (c, n) ->
                    output_strings [sep; c; (string_of_int n)])
                  categories;
                output_string out "\n"
              end
          | Exit_code c ->
              output_strings ["shell-test"; sep; name; sep; (string_of_int c); "\n"]
        method close = safe_close out
      end

let run_tests ?(output=(Text_output stdout)) l =
  let out = make_output output in
  let passed = ref 0 in
  let failed = ref 0 in
  let uncaught = ref 0 in
  let total = ref 0 in
  let l' =
    List.map
      (fun (n, f) ->
        let res = f () in
        (match res with
        | Passed ->
            incr passed;
            incr total
        | Failed _ ->
            incr failed;
            incr total
        | Uncaught _ ->
            incr uncaught;
            incr total
        | Report (pass, tot, unc, _, _) ->
            passed := !passed + pass;
            failed := !failed + (tot - pass -unc);
            uncaught := !uncaught + unc;
            total := !total + tot
        | Exit_code c ->
            incr (if c = 0 then passed else failed);
            incr total);
        (n, res))
      l in
  out#header !passed !failed !uncaught !total;
  List.iter (fun (n, r) -> out#result n (r)) l';
  out#footer;
  out#close

let run_test ?(output=(Text_output stdout)) x =
  run_tests ~output:output [x]

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

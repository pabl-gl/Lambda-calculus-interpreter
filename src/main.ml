open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

let is_the_end text =
  let len = String.length text in
  if len < 2 then false
  else String.sub text (len - 2) 2 = ";;"
;;

let read_command () =
  print_string ">> ";
  flush stdout;

  let rec aux acc =
    let line = read_line () |> String.trim in
    if line = "quit" || line = "clear" || line ="exit" then
      line
    else
      let new_text =
        if acc = "" then line else acc ^ " " ^ line
      in

      if is_the_end new_text then
        (* Quitar el ';;' final *)
        String.sub new_text 0 (String.length new_text - 2)
      else
        aux new_text
  in
  aux ""
;;

let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop ctx =
    try
      let c = s token (from_string (read_command())) in
      loop (execute ctx c)
    with
       Lexical_error ->
         print_endline "lexical error";
         loop ctx
     | Parse_error ->
         print_endline "syntax error";
         loop ctx
     | Type_error e ->
         print_endline ("type error: " ^ e);
         loop ctx
     | End_of_file ->
         print_endline "...bye!!!"
  in
    loop emptyctx
  ;;

top_level_loop ()
;;

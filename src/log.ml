(*
 * Original version:
 * https://github.com/el2724/note-hashtag/blob/master/src/log.ml
 *)

type log_level = Error | Warn | Info | Debug

let min_level = Debug


let int_of_level = function
  | Error -> 4
  | Warn -> 3
  | Info -> 2
  | Debug -> 1

let string_of_level (level : log_level) =
  match level with
  | Error -> "error"
  | Warn -> "warning"
  | Info -> "info"
  | Debug -> "debug"

type color = Bold | Reset | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

(* let color_of_level = function *)
let color_of_level (level : log_level) =
  match level with
  | Error -> Red
  | Warn -> Yellow
  | Info -> Blue
  | Debug -> Cyan

let string_of_color color =
  let escape_of_color = function
    | Reset   ->  0
    | Bold    ->  1
    | Black   -> 30
    | Red     -> 31
    | Green   -> 32
    | Yellow  -> 33
    | Blue    -> 34
    | Magenta -> 35
    | Cyan    -> 36
    | White   -> 37
  in
  Printf.sprintf "\027[%dm" (escape_of_color color)

let print (level : log_level) (fmt : string) =
  let prefix = (string_of_color (color_of_level level)) ^ (string_of_color Bold) ^
               (string_of_level level) ^ ":" ^ (string_of_color Reset) in
  let printer =
    if int_of_level level >= int_of_level min_level
    then Printf.eprintf else (Printf.ifprintf stderr) in
  printer "%s %s\n%!" prefix fmt
  (* some info about format 6:
     [Gagallium : The 6 parameters of (’a, ’b, ’c, ’d, ’e, ’f) format6](http://gallium.inria.fr/blog/format6/)
  *)

let error fmt = print Error fmt
let warn fmt = print Warn fmt
let info fmt = print Info fmt
let debug fmt = print Debug fmt

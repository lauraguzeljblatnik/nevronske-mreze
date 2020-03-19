(*learning to read csv im ocaml*)

#use "topfind";;
#require "csv";;

  #load "str.cma";; 
  #load "csv.cma";;

  open Printf

(* let split_lane = Str.split (Str.regexp ",")
 *)
(* let read_file filename = 
let lines = ref [] in
let chan = open_in filename in
let header = input_line chan in
try
 while true; do
  let line = input_line chan in
    lines := split_lane line :: !lines
  done; List.rev !lines
with End_of_file ->
  close_in chan;
  (List.rev !lines );; *)
  
  
(*   
 let a = read_file "day.csv";;
 
 let num_of_examples = List.length a
 let num_of_input = List.length (List.hd a)
  *)
 
 
(*  let import file_name separator =
  let reg_separator = Str.regexp separator in
  let value_array = Array.make_matrix 1600 12 0. in
  let i = ref 0 in
  try
    let ic = open_in file_name in
    (* Skip the first line, columns headers *)
    let _ = input_line ic in
    try
      while true; do
        (* Create a list of values from a line *)
        let line_list = Str.split reg_separator (input_line ic) in
        List.iteri
		(fun j elem -> value_array.(!i).(j) <- float_of_string elem)
		line_list;
        i := !i + 1;
      done;
      value_array
    with 
      | End_of_file -> close_in ic; value_array
    with
      | e -> raise e;; *)
 
 (* let vsebina = import "day.csv" ",";; *)
 
 
(* let make_reader file_name = *)
  (* let in_channel = open_in file_name in *)
  (* let closed = ref false in *)
  (* let read_next_line = fun () -> *)
    (* if !closed then *)
      (* None *)
    (* else *)
      (* try *)
        (* Some (Scanf.fscanf in_channel "%[^\r\n]\n" (fun x -> x)) *)
      (* with *)
        (* End_of_file -> *)
          (* let _ = close_in_noerr in_channel in *)
          (* let _ = closed := true in *)
          (* None in *)
  (* read_next_line;; *)

  
  
  (* let read_next = make_reader "day.csv";; *)
(* let next_line = read_next ();; *)
(* let first_lane = read_next ();; *)
(* let pom = first_lane;; *)
(* (* let split_lane = Str.split (Str.regexp ",")  *) *)
(* let lane = split_lane pom *)


(*let file = Csv.load "day.csv";;*)

(* open Csv *)
(* open List *)

(* let load f = *)
    (* let g = fun r -> (hd r,  *)
                     (* (map float_of_string (tl r))) *)
    (* in *)
    (* map g (load f) *)
	
	
(* let t = load "day.csv";;	 *)

(* open Printf *)

let matrix_string_to_float a =
	let b =  Array.make_matrix (Array.length a) (Array.length a.(0)) 0. in
	for i = 0 to Array.length a - 1 do
		for j = 0 to Array.length a.(0) -1 do
			b.(i).(j) <- float_of_string a.(i).(j);
		done;
	done;
	b
	

(* let csvs = Csv.load "day.csv" *)

let csvs = Csv.load "identiteta.csv" 

  let lines = (Csv.lines csvs) - 1 
  
  
(* let input_rows = Csv.sub 1 2 600 11 csvs *)


(* let input1 = Csv.to_array (Csv.sub 1 2 600 1 csvs) 
 *)

(*let output_rows = Csv.sub 1 13 600 3 csvs *)

(* let output1 = Csv.to_array (Csv.sub 1 2 600 1 csvs) 
 *)

(* let input = matrix_string_to_float (Csv.to_array (Csv.sub 1 2 600 11 csvs)) *)
 
 
(* let out = matrix_string_to_float (Csv.to_array (Csv.sub 1 13 600 3 csvs)) *)

let input = matrix_string_to_float (Csv.to_array (Csv.sub 1 1 6000 1 csvs))
 
let out = matrix_string_to_float (Csv.to_array (Csv.sub 1 1 6000 1 csvs))

let test_examples = matrix_string_to_float (Csv.to_array (Csv.sub 6000 1 (lines - 6000-2) 1 csvs))

let test_out = matrix_string_to_float (Csv.to_array (Csv.sub 6000 1 (lines - 6000-2) 1 csvs))

(*let test_rows = Csv.sub 600 2 (lines - 600-2) 11 csvs *)

(* let test_examples1 = Csv.to_array Csv.sub 600 2 (lines - 600-2) 11 csvs *)

(* let test_examples = matrix_string_to_float (Csv.to_array (Csv.sub 600 2 (lines - 600-2) 11 csvs)) *)

(* let test_out = matrix_string_to_float (Csv.to_array (Csv.sub 600 13 (lines - 600-2) 3 csvs)) *)

(* let test = Csv.to_array (Csv.sub 1 13 lines 3 csvs);;


let out1 = matrix_string_to_float test;;   *)
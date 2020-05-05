(*learning to read csv im ocaml*)

#use "topfind";;
#require "csv";;

  #load "str.cma";; 
  #load "csv.cma";;

  open Printf


let matrix_string_to_float a =
	let b =  Array.make_matrix (Array.length a) (Array.length a.(0)) 0. in
	for i = 0 to Array.length a - 1 do
		for j = 0 to Array.length a.(0) -1 do
			b.(i).(j) <- float_of_string a.(i).(j);
		done;
	done;
	b
	

let csvs = Csv.load "day.csv"

(* let csvs = Csv.load "identiteta.csv"  *)

let lines = (Csv.lines csvs) - 1 
  
  
(* let input_rows = Csv.sub 1 2 600 11 csvs *)


(* let input1 = Csv.to_array (Csv.sub 1 2 600 1 csvs) 
 *)

(*let output_rows = Csv.sub 1 13 600 3 csvs *)

(* let output1 = Csv.to_array (Csv.sub 1 2 600 1 csvs) 
 *)

let input = matrix_string_to_float (Csv.to_array (Csv.sub 1 2 600 11 csvs))
  
let out = matrix_string_to_float (Csv.to_array (Csv.sub 1 13 600 3 csvs))

let test_examples = matrix_string_to_float (Csv.to_array (Csv.sub 600 2 (lines - 600-2) 11 csvs))

let test_out = matrix_string_to_float (Csv.to_array (Csv.sub 600 13 (lines - 600-2) 3 csvs))

(* (*id_test*) *)
(* let input = matrix_string_to_float (Csv.to_array (Csv.sub 1 1 6000 1 csvs)) *)
 
(* let out = matrix_string_to_float (Csv.to_array (Csv.sub 1 1 6000 1 csvs)) *)

(* let test_examples = matrix_string_to_float (Csv.to_array (Csv.sub 6000 1 (lines - 6000-2) 1 csvs)) *)

(* let test_out = matrix_string_to_float (Csv.to_array (Csv.sub 6000 1 (lines - 6000-2) 1 csvs)) *)

(*let test_rows = Csv.sub 600 2 (lines - 600-2) 11 csvs *)

(* let test_examples1 = Csv.to_array Csv.sub 600 2 (lines - 600-2) 11 csvs *)

(* let test_examples = matrix_string_to_float (Csv.to_array (Csv.sub 600 2 (lines - 600-2) 11 csvs)) *)

(* let test_out = matrix_string_to_float (Csv.to_array (Csv.sub 600 13 (lines - 600-2) 3 csvs)) *)

(* let test = Csv.to_array (Csv.sub 1 13 lines 3 csvs);;


let out1 = matrix_string_to_float test;;   *)
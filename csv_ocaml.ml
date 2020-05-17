(*reading csv im OCaml*)

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

let lines = (Csv.lines csvs) - 1 

let input = matrix_string_to_float (Csv.to_array (Csv.sub 1 2 600 11 csvs))
  
let out = matrix_string_to_float (Csv.to_array (Csv.sub 1 13 600 3 csvs))

let test_examples = matrix_string_to_float (Csv.to_array (Csv.sub 600 2 (lines - 600-2) 11 csvs))

let test_out = matrix_string_to_float (Csv.to_array (Csv.sub 600 13 (lines - 600-2) 3 csvs))

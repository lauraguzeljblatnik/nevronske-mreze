(* NEURAL NETWORK*)

#use "csv_ocaml.ml";;

(*aktivacijska funkcija za posamezen perceptron - sigmoida*)
let sigmoid perceptron =
	1. /. (1. +. exp(-. perceptron))
		

(*odvod sigmoide za posamezen izhodni perceptron*)
let d_sigmoid output =
	(output)*.( 1. -. output)  

(*aktivacijska funkcija za sloj, funct je funkcija aktivacije*)
let activation_layer funct layer =
	Array.map funct layer
	
(*funkcija kombinacije za sloj in pripadajoče uteži*)
let combination_f layer weights = 
	let w1 = Array.length weights.(0) and
		w2 = Array.length weights and
		l = Array.length layer in
	if  w1 <> l then failwith "incompatible matrices!"
	else(
		let comb = Array.make w2 0. in
		for i = 0 to w2-1 do
			for j = 0 to w1-1 do
				comb.(i) <- comb.(i) +. weights.(i).(j) *. layer.(j)
			done;
		done;
	comb
	)
	

(*ustvari float matriko z naključnimi utežmi velikosti nxm,
 elementi so manjši od bound, spodnja meja je 0.,
 m je št nevronov v prejšnjem sloju, n je št nevronov v tem sloju*)	
let rand_weights n m bound =
	Array.init n
        (fun _ -> Array.init m (fun _ -> Random.float bound))

(*funkcija, ki vrne največji element v arrayu*)		
let max_el arr = Array.fold_left max arr.(0) arr
	
(*funkcija, ki ustvari seznam matrik uteži iz danega vektorja, ki opisuje topologijo nevronske mreže*)
let create_weights_matrix network_topology bound =
	let n = (Array.length network_topology) - 1 in
	let weights = Array.make n [||] in
	for i = 0 to n-1 do
		weights.(i) <- rand_weights network_topology.(i+1) network_topology.(i) bound
	done;
	weights
	
	
(*funkcija, ki vzame sznam, ki opisuje topologijo mreže in iz tega ustvari matriko, ki predstavlja nevrone v mreži.
Ima velikost mxn, kjer je m št slojev in n max št nevronov v sloju*)
let initialize_network network_topology =	
	let layers_n = Array.length network_topology and
		max_layer = max_el network_topology in
	let	network = Array.make_matrix layers_n max_layer 0. in 
	network
	

	
(*izračuna napako med dobljenim in željenim rezultatom, vrne vektor razlik za izhodni sloj!*)	
(*d je to kar hočeš, y to kar mreža dobi*)
let delta_output d y =
	let d0 = Array.length d and
	y0 = Array.length y in
	if y0 <> d0 then failwith "wrong dimensions!"
	else (
		let e = Array.make d0 0. in 
		for i = 0 to d0-1 do
			if d.(i) = y.(i) then failwith "hit the right value"
			else if y.(i) = 0. then failwith "output is zero"
			else if y.(i) = 1. then failwith "output is one"
			else e.(i) <- -.(d.(i) -. y.(i))*. (d_sigmoid y.(i))
		done;
	e
	)


		
(*w so uteži enega sloja naprej(že popravljene!), h_m so izhodi v tem sloju, d_out je delta naslednjega sloja*)
(*TO PREVERI ČE JE RES OK*)
let delta_hidden w h_m d_out = 
	let a = Array.length h_m and
		d_0 = Array.length d_out in 
	let e = Array.make a 0. in
	for i = 0 to a-1 do
		let sum = ref 0. in
		for j = 0 to d_0 -1 do
			sum := !sum +. w.(j).(i) *. d_out.(j)
		done;
		e.(i) <- (d_sigmoid h_m.(i)) *. !sum
	done;
	e

(*w so uteži, ki jih popravljaš, rate je stopnja učenja, delta je error - pomožne funkcije,
input so nevroni pred utežjo, eno stopnjo nazaj *)
let update_weights w rate delta input =
	let d_0 = Array.length delta and
		in_0 = Array.length input in
	for i = 0 to d_0 -1 do
		for j = 0 to in_0 -1 do
			w.(i).(j) <- w.(i).(j) -. rate *. delta.(i) *. input.(j)
		done;
	done;
	(* let n = w |> Array.iter (Array.iter print_float) in *)
	w



(*funkcija, ki sprejme en učni primer in ustrezno popravi uteži*)
(*zdi se mi da dela, kako to preverit je vprašanje*)
let learning_example x d network weights rate = 
	let n = Array.length network in
		network.(0) <- x;
	(* let () = network |> Array.iter (Array.iter print_float) in *)
   	for i = 0 to n-2 do
		network.(i+1) <- activation_layer sigmoid( combination_f network.(i) weights.(i))
	done; 
	(* let () = network |> Array.iter (Array.iter print_float) in *)
	(* let () = fun( print_newline) in  *)
	let delta = ref (delta_output d network.(n-1)) in
	for i = 0 to n-2 do
		(* let () = print_newline () in *)
		(* let () = print_endline "weights: " in *)
		(* weights |> Array.iter (Array.iter (Array.iter print_float)); *)
		(* let () = print_newline () in *)
		(* let () = print_endline "delta: " in *)
		(* !delta |> (Array.iter print_float); *)
		weights.(n-2-i) <- update_weights weights.(n-2-i) rate !delta network.(n-2-i);
		delta := (delta_hidden weights.(n-2-i) network.(n-2-i) !delta); 
	done; 
	(* let () = weights |> Array.iter (Array.iter (Array.iter print_float)) in *)
	weights
	
	
(*
funkciji podamo:
 - file s podatki examples 
 - seznam s št. nevronov po slojih n_neurons
 - stopnjo učenja rate
 - funkcija ustvari matrike 
 *)	
 
 
 
 let train_network input_array output_array network_topology rate bound = 
	let network = initialize_network network_topology in 
	let weights = create_weights_matrix network_topology bound in
	let n = Array.length input_array in 
	for i = 0 to n-1 do
		let pom = ref (learning_example  input_array.(i) output_array.(i) network weights rate) in
		for i = 0 to (Array.length weights)-1 do
			weights.(i) <- !pom.(i);
		done;
		(* weights <- pom; *)
	done;
	weights 
	
let train_with_input_weights input_array output_array network_topology rate weights =
	let network = initialize_network network_topology in 
	let n = Array.length input_array in 
	for i = 0 to n-1 do
		let pom = ref (learning_example  input_array.(i) output_array.(i) network weights rate) in
		for i = 0 to (Array.length weights)-1 do
			weights.(i) <- !pom.(i);
		done;
	done;
	weights 



(* daš notri uteži in topology in input in vrne napoved	 *)
let predict input network_topology weights =
	let network = initialize_network network_topology in
	let n = Array.length network in
		network.(0) <- input;
   	for i = 0 to n-2 do
		network.(i+1) <- activation_layer sigmoid ( combination_f network.(i) weights.(i))
		(* a moraštu čez vržt activation ali ne*)
	done;
	(* let n1 = network |> Array.iter (Array.iter print_float) in *)
	network.(n-1)
	







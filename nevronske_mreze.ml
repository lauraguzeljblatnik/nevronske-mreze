(* NEURAL NETWORK*)


(*aktivacijska funkcija za posamezen nevron - sigmoida*)
let sigmoid neuron =
	1. /. (1. +. exp(-. neuron))
		

(*odvod sigmoide za posamezen izhodni nevron, sprejme output - to kar mreža dobi
odvod sigmoide je f'(x) = f(x)*(1-f(x) *)
let d_sigmoid output =
	(output)*.( 1. -. output)  

(*aktivacijska funkcija za sloj, funct je funkcija aktivacije, ki jo želimo uporabiti *)
let activation_layer funct layer =
	Array.map funct layer
	
(*funkcija kombinacije za sloj in pripadajoče uteži, zmnoži vektor - sloj nevronov z matriko uteži,
vrne vektor*)
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
 elementi so manjši od bound - float, spodnja meja je 0.,
 m je št nevronov v prejšnjem sloju, n je št nevronov v tem sloju, oba int
 vrne matriko mxn*)	
let rand_weights n m bound =
	Array.init n
        (fun _ -> Array.init m (fun _ -> Random.float bound))

(*funkcija, ki vrne največji element v tabeli*)		
let max_el arr = Array.fold_left max arr.(0) arr
	
(*funkcija, ki ustvari tabelo matrik naključnih uteži s pravimi velikostmi, 
sprejme vektor, 
ki opisuje topologijo nevronske mreže - int array, in zgornjo mejo teh uteži - float*)
let create_weights_matrix network_topology bound =
	let n = (Array.length network_topology) - 1 in
	let weights = Array.make n [||] in
	for i = 0 to n-1 do
		weights.(i) <- rand_weights network_topology.(i+1) network_topology.(i) bound
	done;
	weights
	
	
(*funkcija, sprejme int array, ki opisuje topologijo mreže in iz tega ustvari float matriko, ki predstavlja nevrone v mreži.
Ima velikost mxn, kjer je m št slojev in n max št nevronov v sloju,
na začetku so vse vrednosti 0.*)
let initialize_network network_topology =	
	let layers_n = Array.length network_topology and
		max_layer = max_el network_topology in
	let	network = Array.make_matrix layers_n max_layer 0. in 
	network


(*delta je odvod dE/dA, glej zapiske!*)
	
(*izračuna napako za izhodni sloj, vrne vektor*)	
(*d je željen output, y to kar mreža dobi, act_der je odvod funkcije aktivacije*)
let delta_output act_der d y =
	let d0 = Array.length d and
	y0 = Array.length y in
	if y0 <> d0 then failwith "wrong dimensions!"
	else (
		let e = Array.make d0 0. in 
		for i = 0 to d0-1 do
			(* if d.(i) = y.(i) then failwith "hit the right value" *)
			(* else if y.(i) = 0. then failwith "output is zero" *)
			(* else if y.(i) = 1. then failwith "output is one" *)
			(* else  *)
			e.(i) <- -.(d.(i) -. y.(i))*. (act_der y.(i))
		done;
	e
	)


		
(*izračuna napako za skrite sloje,
w so uteži enega sloja naprej(že popravljene!), 
h_m so izhodi v tem sloju, d_out je delta naslednjega sloja,
vrne vektor*)
let delta_hidden w h_m d_out act_der = 
	let a = Array.length h_m and
		d_0 = Array.length d_out in 
	let e = Array.make a 0. in
	for i = 0 to a-1 do
		let sum = ref 0. in
		for j = 0 to d_0 -1 do
			sum := !sum +. w.(j).(i) *. d_out.(j)
		done;
		e.(i) <- (act_der h_m.(i)) *. !sum;
	done;
	e

(*w so uteži, ki jih popravljaš, rate je stopnja učenja, delta je error - pomožne funkcije,
input je vektor nevronov en sloj pred utežjo (eno stopnjo nazaj),
vrne popravljene uteži *)
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



(*funkcija, ki sprejme en učni primer in ustrezno popravi uteži
input: x - vhodni vektor
	d - željen izhodni vektor
	network - matrika nevronov v mreži, velikosti (št. slojev) x (max nevronov v sloju), vsi el. so 0.
	weights - tabela matrik uteži v mreži
	rate - stopnja učenja
	act_fun - funkcija aktivacije
	act_der - odvod funkcije aktivacije
output: popravljene uteži*)
let learning_example x d network weights rate act_fun act_der = 
	let n = Array.length network in
		network.(0) <- x;
	(* let () = network |> Array.iter (Array.iter print_float) in *)
	(*forward propragation*)
   	for i = 0 to n-2 do
		network.(i+1) <- activation_layer act_fun( combination_f network.(i) weights.(i))
	done; 
	(* let () = network |> Array.iter (Array.iter print_float) in *)
	(* let () = fun( print_newline) in  *)
	let delta = ref (delta_output act_der d network.(n-1)) in
	for i = 0 to n-2 do
		(* let () = print_newline () in *)
		(* let () = print_endline "weights: " in *)
		(* weights |> Array.iter (Array.iter (Array.iter print_float)); *)
		(* let () = print_newline () in *)
		(* let () = print_endline "delta: " in *)
		(* !delta |> (Array.iter print_float); *)
		weights.(n-2-i) <- update_weights weights.(n-2-i) rate !delta network.(n-2-i);
		delta := (delta_hidden weights.(n-2-i) network.(n-2-i) !delta act_der); 
	done; 
	(* let () = weights |> Array.iter (Array.iter (Array.iter print_float)) in *)
	weights
	
	
(* funkcija, ki nauči mrežo (uteži) pravilnega delovanja
vhod:
 - input_array: tabela vhodnih vektorjev
 - output_array: tabela pripadajočih željenih izhodnih vektorjev
 - network_topology: tabela s št. nevronov po slojih 
 - rate: stopnja učenja 
 - bound: zgornja meja, ko inicializiramo uteži
 - act_fun: funkcija aktivacije
 - act_der: odvod funkcije aktivacije
izhod:
- naučene uteži
 *)	
 let train_network input_array output_array network_topology rate bound act_fun act_der= 
	let network = initialize_network network_topology in 
	let weights = create_weights_matrix network_topology bound in
	let n = Array.length input_array in 
	for i = 0 to n-1 do
		let pom = ref (learning_example  input_array.(i) output_array.(i) network weights rate act_fun act_der) in
		for i = 0 to (Array.length weights)-1 do
			(* let () = print_newline () in *)
			(* weights.(i) |> Array.iter (Array.iter print_float); *)
			weights.(i) <- !pom.(i);
			(* let () = print_newline () in *)
			(* weights.(i) |> Array.iter (Array.iter print_float); *)
		done;
				(* let () = print_newline () in *)
				let () = print_newline () in
		weights |> Array.iter (Array.iter (Array.iter print_float));
		(* let () = print_newline () in *)
		(* weights <- pom; *)
	done;
	weights 
	
(*TESTNO, sprejme tabelo z matrikami uteži, ne ustvari sama*)
let train_with_input_weights input_array output_array network_topology rate weights act_fun act_der =
	let network = initialize_network network_topology in 
	let n = Array.length input_array in 
	for i = 0 to n-1 do
		let pom = ref (learning_example  input_array.(i) output_array.(i) network weights rate act_fun act_der) in
		for i = 0 to (Array.length weights)-1 do
			weights.(i) <- !pom.(i);
		done;
		(* let () = weights |> Array.iter (Array.iter print_float)  *)
	done;
	weights 



(* daš notri naučene uteži in topology in input in vrne napoved	 *)
(*NE NAPOVE PRAV, kje je napaka, popravi*)
let predict input network_topology weights act_fun =
	let network = initialize_network network_topology in
	let n = Array.length network in
		network.(0) <- input;
   	for i = 0 to n-2 do
		network.(i+1) <- activation_layer act_fun ( combination_f network.(i) weights.(i))
		(* a moraštu čez vržt activation ali ne*)
	done;
	(* let n1 = network |> Array.iter (Array.iter print_float) in *)
	network.(n-1)
	







(* NEURAL NETWORK*)

(*najprej definirajmo nekaj funkcij*)

(*aktivacijska funkcija za posamezen perceptron*)
let sigmoid perceptron =
	1. /. (1. +. exp(-. perceptron))

(*odvod sigmoide*)
let d_sigmoid output =
	(output)*.( 1. -. output)  

(*aktivacijska funkcija za sloj*)
let activation_layer layer =
	Array.map sigmoid layer

(*ustvari float matriko z naključnimi utežmi velikosti nxm,
 elementi so manjši od bound,
 m je št nevronov v prejšnjem sloju, n je št nevronov v tem sloju*)	
let rand_weights n m bound =
	Array.init n
        (fun _ -> Array.init m (fun _ -> Random.float bound))

(*funkcija, ki vrne največji element v arrayu*)		
let max_el arr = Array.fold_left max arr.(0) arr
	
(*!!!!!!!!!!TODO - zgleda ok - preveri če dela prav - brt, men se zdi ok*)
(*funkcija, ki ustvari seznam matrik uteži iz danega vektorja, ki opisuje topologijo nevronske mreže
aka število nevronov po slojih, bound je zgornja meja za uteži - spodnja meja je 0.*)
let create_weights_matrix network_topology bound =
	let n = (Array.length network_topology) - 1 in
	let weights = Array.make n [||] in
	for i = 0 to n-1 do
		weights.(i) <- rand_weights network_topology.(i+1) network_topology.(i) bound
	done;
	weights
	
(*funkcija, ki vzame array topologija_mreze in iz tega ustvari matriko, ki predstavlja nevrone v mreži.
Ima velikost mxn, kjer je m št slojev in n max št nevronov v sloju*)
let initialize_network network_topology =	
	let layers_n = Array.length network_topology and
		max_layer = max_el network_topology in
	let	network = Array.make_matrix layers_n max_layer 0. in 
	network
	
(*funkcija kombinacije za sloj*)
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

let output_matrix network weights = ()


(*izračuna napako med dobljenim in želenim rezultatom, vrne vektor razlik*)	
(*d je to kar hočeš, y to kar mreža dobi*)
let delta_output d y =
	let d0 = Array.length d and
	y0 = Array.length y in
	if y0 <> d0 then failwith "wrong dimensions!"
	else (
		let e = Array.make d0 0. in 
		for i = 0 to d0-1 do
			e.(i) <- -.(d.(i) -. y.(i))*. d_sigmoid y.(i)
		done;
	e
	)

		
(*w so uteži enega sloja naprej(že popravljene!), h_m so izhodi v tem sloju, d_out je delta naslednjega sloja*)
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

	
let update_weights w rate delta output =
	let d_0 = Array.length delta and
		out_0 = Array.length output in
	for i = 0 to d_0 -1 do
		for j = 0 to out_0 -1 do
			w.(i).(j) <- w.(i).(j) -. rate *. delta.(i) *. output.(j)
		done;
	done;
	w

(*funkcija, ki sprejme en učni primer in ustrezno popravi uteži*)
(*????? a res rabiš podat x, če je x že v inicialize network *)	
(*zdi se mi da dela, kako to preverit je vprašanje*)
let learning_example x d network weights rate = 
	let n = Array.length network in
		network.(0) <- x;
   	for i = 0 to n-2 do
		network.(i+1) <- activation_layer( combination_f network.(i) weights.(i))
	done; 
	let delta = ref (delta_output d network.(n-1)) in
	for i = 0 to n-2 do
		weights.(n-2-i) <- update_weights weights.(n-2-i) rate !delta network.(n-2-i);
		delta := (delta_hidden weights.(n-2-i) network.(n-2-i) network.(n-1-i)); (* TODO *) 
	done; 
	weights
	
	(*zaenkrat aktivira nevrone, to do je backprop oz popravi uteži*)
	
	
(*
funkciji podamo:
 - file s podatki examples 
 - seznam s št. nevronov po slojih n_neurons
 - stopnjo učenja rate
 - funkcija ustvari matrike 
 *)	
let train_network examples network_topology rate bound = 
	let n = number_of_examples in (*stevilo ucnih primerov, dobis is csv*)
	let input_n = len_onput_vector in
	let network = initialize_network network_topology in 
	let weights = create_weights_matrix network_topology bound in
	for i = 0 to n-1 do
		weights <- learning_example example_input.(i) example_outpit(i) network weights rate
	done;
	weights

(*daš notri uteži in topology in input in vrne napoved*)	
let predict input network weights =
	let n = Array.length network in
		network.(0) <- x;
   	for i = 0 to n-2 do
		network.(i+1) <- activation_layer( combination_f network.(i) weights.(i))
	done;
	netwotk.(n-1)
	




	
(*TEST*)

(*vhodni podatki*)
let x = [| 1. ; 1.; 1.; 1.|]
(*learning rate*)
let rate = 0.5
(*želen izhod*)
let d =[|0.191; 0.25|]
(*št nevronov v prvem hidden sloju*)
let layer1 = 3
(*število nevronov po slojih lahko podamo kot array*)
let network_topology = [|4; 3; 2|]
let net_top = [|1;2;3|]
(*funkcija, ki iz dane topologije ustvari matriko matrik uteži*)


let w1 = rand_weights layer1 (Array.length x) 5.
let w2 = rand_weights (Array.length d) layer1 5.
let l = combination_f x w1
let l1 = activation_layer l
let l2 = combination_f l1 w2
let y = activation_layer l2  
let e_o = delta_output d y
let w2_new = update_weights w2 rate e_o l1
let e_hidden = delta_hidden w2_new l1 e_o
let w1_new = update_weights w1 rate e_hidden x
(*let max_nevron = max_el network_topology*)
let neurons = initialize_network network_topology
let weights = create_weights_matrix network_topology 5.

let test = learning_example x d neurons weights rate 






(* IMPLEMENTACIJA NEVRONSKE MREŽE *)

(*pomožna funkcija: 
	aktivacijska funkcija za posamezen nevron - sigmoida *)
let sigmoid neuron =
	1. /. (1. +. exp(-. neuron))
		
(*pomožna funkcija:
	odvod sigmoide za posamezen izhodni nevron, sprejme output - 
	to kar mreža izračuna. Odvod sigmoide je f'(x) = f(x)*(1-f(x)),
	funkcijo x -> x(1-x) uporabimo na vrednostih f(x) *)
let d_sigmoid output =
	(output)*.( 1. -. output)  

(*pomožna funkcija:
	aktivacijska funkcija za sloj(vektor), funct je funkcija 
	aktivacije, ki jo želimo na uporabiti *)
let activation_layer funct layer =
	Array.map funct layer
	
(*pomožna funkcija:
	funkcija kombinacije za sloj in pripadajoče uteži, 
	zmnoži vektor (sloj nevronov) z matriko uteži,
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
				comb.(i) <- 
				comb.(i) +. weights.(i).(j) *. layer.(j)
			done;
		done;
	comb
	)

(*pomožna funkcija, ki ustvari float matriko z naključnimi 
	utežmi velikosti nxm,
vhod:
- n: št. nevronov v tem sloju
- m: št. nevronov v prejšnjem sloju
- bound: vrednosti v matriki so med bound in (-bound)
izhod: 
- matrika uteži velikosti mxn *)	
let rand_weights n m bound =
	Array.init n
	(fun _ 
		-> Array.init m (fun _ 
			-> (Random.float 2.*.bound)-.bound))

(*pomožna funkcija:
	vrne največji element v tabeli*)		
let max_el arr = Array.fold_left max arr.(0) arr
	
(*pomožna funkcija, ki ustvari tabelo matrik naključnih 
uteži s pravimi velikostmi
vhod:
- network_topology: vektor, ki opisuje topologijo 
	nevronske mreže - int array,
- bound: zgornja meja uteži - float
izhod: tabela matrik naključnih uteži prave velikosti*)
let create_weights_matrix network_topology bound =
	let n = (Array.length network_topology) - 1 in
	let weights = Array.make n [||] in
	for i = 0 to n-1 do
		weights.(i) <- 
			rand_weights 
				network_topology.(i+1) network_topology.(i) bound
	done;
	weights
	
(*funkcija, ki ustvari float matriko, ki predstavlja nevrone v mreži
vhod:
- network_topology: tabela, ki opisuje topologijo mreže
izhod: 
- float matrika velikosti mxn, kjer je m št. slojev in n max 
št. nevronov v sloju, na začetku so vse vrednosti v matriki 0.*)
let initialize_network network_topology =	
	let layers_n = Array.length network_topology and
		max_layer = max_el network_topology in
	let	network = Array.make_matrix layers_n max_layer 0. in 
	network

(*delta = dE/dA*)
	
(*funkcija, ki izračuna vektor delta za izhodni sloj
vhod:
- d: tabela z želenimi izhodi
- y: tabela z izhodnimi vrednostmi mreže 
- act_der: odvod funkcije aktivacije
izhod: 
- tabela vrednosti delta *)
let delta_output act_der d y =
	let d0 = Array.length d and
	y0 = Array.length y in
	if y0 <> d0 then failwith "wrong dimensions!"
	else (
		let e = Array.make d0 0. in 
		for i = 0 to d0-1 do
			e.(i) <- -.(d.(i) -. y.(i))*. (act_der y.(i))
		done;
	e
	)
		
(*funkcija, ki izračuna vektor delta za skrite sloje
vhod:
- w: matrika uteži en sloj naprej (že popravljene uteži!)
- h_m: tabela izhodov v sloju, kjer računamo delto
- d_out: tabela delta v naslednjem sloju
izhod: 
- tabela vrednosti delta *)
let delta_hidden w h_m d_out act_der = 
	let a = Array.length h_m and
		d_0 = Array.length d_out in 
	let e = Array.make a 0. in
	for i = 0 to a-1 do
		let sum = ref 0. in
		for j = 0 to d_0 - 1 do
			sum := !sum +. w.(j).(i) *. d_out.(j)
		done;
		e.(i) <- (act_der h_m.(i)) *. !sum;
	done;
	e

(*funkcija, ki ustrezno popravi vrednosti v matriki uteži
vhod:
- w: matrika uteži, ki jo spreminjamo
- rate: stopnja učenja
- delta: ustrezna tabela napake dE/dA
- input: tabela vhodnih podatkov/vrednosti 
izhod:
- ustrezno popravljene uteži *)
let update_weights w rate delta input =
	let d_0 = Array.length delta and
		in_0 = Array.length input in
	for i = 0 to d_0 -1 do
		for j = 0 to in_0 -1 do	
			w.(i).(j) <- 
				w.(i).(j) -. rate *. delta.(i) *. input.(j)
		done;
	done;
	w

(*funkcija, ki vrne povprečje danih vhodnih podatkov
vhod: matrika s podatki
izhod: vektor s povprečji po stolpcih 
*)	
let mean data =
	let n = Array.length data in
	let n_a = Array.length data.(0) in
	let m = Array.make n_a 0. in
	for i = 0 to n-1 do
		for j = 0 to n_a - 1 do
			m.(j) <- m.(j) +. data.(i).(j)
		done;
	done;
	for j = 0 to n_a - 1 do
		m.(j) <- m.(j) /. (float_of_int n)
	done;
	m
	
(*funkcija, ki vrne standardni odklon danih vhodnih podatkov
vhod: 
- data: matrika s podatki 
- mean: povprečje podatkov
izhod: vektor standardnih odklonov po stolpcih 
*)	
let std data mean = 
	let n = Array.length data in
	let n_a = Array.length data.(0) in
	let s = Array.make n_a 0. in
	for i = 0 to n-1 do
		for j = 0 to n_a - 1 do
			s.(j) <- s.(j) +. (data.(i).(j) -. mean.(j))**2.;
		done;
	done;
	for j = 0 to n_a - 1 do
		s.(j) <- sqrt(s.(j) /. (float_of_int n))
	done;
	s
	
	
(*funkcija, ki normalizira podatke z z-score normalizacijo
vhod:
- a: tabela z vektorji podatki
- mean: vektor povprečja podatkov
- std_dev: vektor standardnega odklona podatkov
izhod: normalizirana tabela vektorjev podatkov
*)
let norm_z_score a mean std_dev =
	let n = Array.length a in
	let arr = Array.copy a in
	for i = 0 to n-1 do
		arr.(i) <- (arr.(i) -. mean.(i)) /. std_dev.(i)
	done;
	arr
	
(*funkcija, ki denormalizira podatke z z-score normalizacijo
vhod: 
- a: tabela z vektorji podatkov
- mean: vektor povprečja podatkov
- std_dev: vektor standardnega odklona podatkov
izhod: denormalizirana tabela vektorjev podatkov
*)
let denorm_z_score a mean std_dev =
	let n = Array.length a in
	let arr = Array.copy a in
	for i = 0 to n-1 do
		arr.(i) <- (arr.(i) *. std_dev.(i)) +. mean.(i)
	done;
	arr

(*funkcija, ki sprejme en učni primer in ustrezno popravi uteži
vhod: 
- x: vhodni vektor
- d: želen izhodni vektor
- network: matrika nevronov v mreži,
	velikosti (št. slojev) x (max nevronov v sloju), 
	vsi el. so 0.
- weights: tabela matrik uteži v mreži
- rate: stopnja učenja
- act_fun: funkcija aktivacije
- act_der: odvod funkcije aktivacije
izhod: popravljene uteži*)
let learning_example x d network weights rate act_fun act_der = 
	let n = Array.length network in
		network.(0) <- x;
	(*forward propragation*)
   	for i = 0 to n-2 do
		network.(i+1) <- activation_layer act_fun( 
			combination_f network.(i) weights.(i)
			);
	done; 
	let delta = ref(delta_output act_der d network.(n-1)) in
	for i = 0 to n-2 do
		weights.(n-2-i) <- update_weights 
			weights.(n-2-i) rate !delta network.(n-2-i)
			;
		delta := (delta_hidden 
			weights.(n-2-i) network.(n-2-i) !delta act_der
		); 
	done; 
	weights
		
	
(* funkcija, ki nauči mrežo (uteži) pravilnega delovanja
vhod:
 - input_array: tabela vhodnih vektorjev
 - output_array: tabela pripadajočih želenih izhodnih vektorjev
 - network_topology: tabela s št. nevronov po slojih 
 - rate: stopnja učenja 
 - bound: zgornja meja, ko inicializiramo uteži
 - act_fun: funkcija aktivacije
 - act_der: odvod funkcije aktivacije
izhod:
- naučene uteži
 *)	
 let train_network input_array output_array 
		network_topology rate bound act_fun act_der = 
	let network = initialize_network network_topology in 
	let weights = create_weights_matrix network_topology bound in
	let mean_in = mean input_array in
	let std_dev_in = std input_array mean_in in	
	let mean_out = mean output_array in
	let std_dev_out = std output_array mean_out in
	let n = Array.length input_array in 
	for i = 0 to n-1 do
		let norm_input_array = norm_z_score 
			input_array.(i) mean_in std_dev_in in
		let norm_output_array = norm_z_score 
			output_array.(i) mean_out std_dev_out in
		let pom = ref (learning_example  
			norm_input_array norm_output_array 
			network weights rate act_fun act_der) in
		for i = 0 to (Array.length weights)-1 do
			weights.(i) <- !pom.(i);
		done;
	done;
	weights 
	
(*funkcija, ki nauči mrežo (uteži) pravilnega delovanja
na podanih začetnih utežeh
vhod:
 - input_array: tabela vhodnih vektorjev
 - output_array: tabela pripadajočih želenih izhodnih vektorjev
 - network_topology: tabela s št. nevronov po slojih 
 - rate: stopnja učenja 
 - w: vhodne inicializirane uteži
 - act_fun: funkcija aktivacije
 - act_der: odvod funkcije aktivacije
izhod:
- naučene uteži
 *)	
let train_with_input_weights input_array output_array 
	network_topology rate w act_fun act_der =
	let network = initialize_network network_topology in
	let mean_in = mean input_array in
	let std_dev_in = std input_array mean_in in	
	let mean_out = mean output_array in
	let std_dev_out = std output_array mean_out in
	let n = Array.length input_array in 
	for i = 0 to n-1 do
		let norm_input_array = norm_z_score 
			input_array.(i) mean_in std_dev_in in
		let norm_output_array = norm_z_score 
			output_array.(i) mean_out std_dev_out in
		let pom = ref (learning_example  
			norm_input_array norm_output_array network w 
			rate act_fun act_der) in
		for i = 0 to (Array.length w)-1 do
			w.(i) <- !pom.(i);
		done;
	done;
	w 
 
(*funkcija, ki napove vrednost pri testnem primeru
vhod:
 - input: vhodna tabela
 - network: matrika, ki predstavlja nevrone v mreži
 - weights: tabela matrik uteži
 - act_fun: funkcija aktivacije
izhod:
- napoved
 *)	
let predict input network weights act_fun =
	let n = Array.length network in
		network.(0) <- input;
   	for i = 0 to n-2 do
		network.(i+1) <- activation_layer act_fun ( 
		combination_f network.(i) weights.(i)
		)
	done;
	network.(n-1)
	
(*funkcija, ki napove vrednost pri testnem primeru
vhod:
- test_input: tabela vhodnih vektorjev
- test_output: tabela izhodnih vektorjev
- weights: tabela matrik uteži
- network_topology: tabela, ki opisuje topologijo mreže
- act_fun: funkcija aktivacije
- mean_in: tabela povprečnih vrednosti vhodnih podatkov
- std_in: tabela standardnih odklonov vhodnih podatkov
- mean_out: tabela povprečnih vrednosti izhodnih podatkov
- std_out: tabela standardnih odklonov izhodnih podatkov
izhod:
- povprečna napaka med dobljeno in želeno vrednostjo
 *)		
let evaluate test_input test_output weights network_topology 
	act_fun mean_in std_in mean_out std_out =	
	let network = initialize_network network_topology in
	let n = Array.length test_input in
	let o1 = Array.length test_output.(0) in
	let error = Array.make_matrix n o1 0. in
	for i=0 to n-1 do
		let prediction = predict (
			norm_z_score test_input.(i) mean_in std_in)
			network weights act_fun  in
		let norm_pred = denorm_z_score 
			prediction mean_out std_out  in 
		for j=0 to o1-1 do		
			let e_j = abs_float (
				norm_pred.(j) -. test_output.(i).(j)) in 
			error.(i).(j) <- e_j;
		done;
	done;
	let e_m = mean error in
	e_m
	
	
(*funkcija, ki n-krat požene evaluate na n 
različnih mrežah, n-krat nauči in pogleda napako
vhod:
- train_input: tabela vhodnih učnih vektorjev
- train_output: tabela vhodnih testnih vektorjev
- test_input: tabela izhodnih učnih vektorjev
- test_output: tabela izhodnih testnih vektorjev
- network_topology: tabela, ki opisuje topologijo mreže
- act_fun: funkcija aktivacije
- act_der: odvod funkcije aktivacije
- rate: stopnja učenja
- n: število ponovitev
izhod:
- povprečna napaka med dobljeno in želeno vrednostjo
 *)	
let analysis_error train_input train_output test_input 
	test_output network_topology act_fun act_der rate n =
	let o1 = Array.length test_output.(0) in
	let e = Array.make_matrix n o1 0. in
	let mean_in = mean train_input in
	let mean_out = mean train_output in
	let std_in = std train_input mean_in in
	let std_out = std train_output mean_out in
	for i=0 to (n-1) do
		let trained_w = train_network train_input train_output 
			network_topology rate 1.0 act_fun act_der in
		let error = evaluate test_input test_output trained_w 
			network_topology act_fun 
			mean_in std_in mean_out std_out in
		e.(i) <- error;
	done;
	let e_m = mean e in
	e_m
	
(*funkcija, ki n-krat požene evaluate na n 
različnih mrežah, mreže ne nauči, le izračuna napako
vhod:
- train_input: tabela vhodnih učnih vektorjev
- train_output: tabela vhodnih testnih vektorjev
- test_input: tabela izhodnih učnih vektorjev
- test_output: tabela izhodnih testnih vektorjev
- network_topology: tabela, ki opisuje topologijo mreže
- act_fun: funkcija aktivacije
- act_der: odvod funkcije aktivacije
- rate: stopnja učenja
- n: število ponovitev
izhod:
- povprečna napaka med dobljeno in želeno vrednostjo 
pri nenaučenih utežeh
 *)		
let analysis_unlearned train_input train_output test_input 
	test_output network_topology act_fun act_der rate n =
	let o1 = Array.length test_output.(0) in
	let e = Array.make_matrix n o1 0. in
	let mean_in = mean train_input in
	let mean_out = mean train_output in
	let std_in = std train_input mean_in in
	let std_out = std train_output mean_out in
	for i=0 to (n-1) do
		let w = create_weights_matrix network_topology 1.0 in
		let error = evaluate test_input test_output w 
			network_topology act_fun 
			mean_in std_in mean_out std_out in
		e.(i) <- error;
	done;
	let e_m = mean e in
	e_m

(*funkcija, ki prešteje, kolikorat je bilo 
učenje z naučeno mrežo boljše (napaka je bila manjša), 
kot učenje z nenaučeno mrežo
 vhod:
- train_input: tabela vhodnih učnih vektorjev
- train_output: tabela vhodnih testnih vektorjev
- test_input: tabela izhodnih učnih vektorjev
- test_output: tabela izhodnih testnih vektorjev
- network_topology: tabela, ki opisuje topologijo mreže
- act_fun: funkcija aktivacije
- act_der: odvod funkcije aktivacije
- rate: stopnja učenja
- n: število ponovitev
izhod:
- tabela s številom uspešnih učenj po izhodnih
	parametrih
 *)	
let learned_vs_unlearned train_input train_output test_input 
	test_output network_topology act_fun act_der rate n =
	let o1 = Array.length test_output.(0) in
	let count = Array.make o1 0 in
	let mean_in = mean train_input in
	let mean_out = mean train_output in
	let std_in = std train_input mean_in in
	let std_out = std train_output mean_out in
	for i=0 to (n-1) do
		let w = create_weights_matrix network_topology 1.0 in
		let error_unlearned = evaluate test_input test_output
			w network_topology act_fun 
			mean_in std_in mean_out std_out in
		let trained_w = train_with_input_weights 
			train_input train_output network_topology 
			rate w act_fun act_der in
		let error_learned = evaluate test_input test_output 
			trained_w network_topology act_fun 
			mean_in std_in mean_out std_out in
		for j=0 to (o1-1) do
			if (error_unlearned.(j) > error_learned.(j)) 
			then count.(j) <- count.(j) + 1
			else count.(j) <- count.(j) - 1
		done;
	done;
	count
	
	
(*funkcija, ki za različne stopnje učenja izračuna 
povprečno absolutno napako pri fiksni topologiji
vhod:
- r_vect: tabela vrednosti stopenj učenja
- train_input: tabela vhodnih učnih vektorjev
- train_output: tabela vhodnih testnih vektorjev
- test_input: tabela izhodnih učnih vektorjev
- test_output: tabela izhodnih testnih vektorjev
- network_topology: tabela, ki opisuje topologijo mreže
- act_fun: funkcija aktivacije
- act_der: odvod funkcije aktivacije
izhod:
- povprečna absolutna napaka za vsako stopnjo
	učenja iz r_vect
 *)	
let rate_analysis r_vect train_input train_output test_input 
	test_output network_topology act_fun act_der =
	let o1 = Array.length test_output.(0) in 
	let mean_in = mean train_input in
	let mean_out = mean train_output in
	let std_in = std train_input mean_in in
	let std_out = std train_output mean_out in
	let n = Array.length r_vect in
	let r = Array.make_matrix n o1 0. in
	for r_i=0 to n-1 do
		let error = analysis_error train_input train_output 
			test_input test_output network_topology 
			act_fun act_der r_vect.(r_i) 50 in
		r.(r_i) <- error;
	done;
	r
	
(*funkcija, ki za različna števila nevronov v skritem 
sloju izračuna povprečno absolutno napako 
pri fiksni stopnji učenja
vhod:
- min_hidden: najmanjše št. skritih nevronov
- n: število za kolikor želimo povečati min_hidden
- train_input: tabela vhodnih učnih vektorjev
- train_output: tabela vhodnih testnih vektorjev
- test_input: tabela izhodnih učnih vektorjev
- test_output: tabela izhodnih testnih vektorjev
- act_fun: funkcija aktivacije
- act_der: odvod funkcije aktivacije
- rate: stopnja učenja
izhod:
- povprečna absolutna napaka za vsako topologijo
	z od min_hidden do n nevroni v skritem sloju
 *)		
let topology_analysis min_hidden n train_input train_output 
	test_input test_output act_fun act_der rate = 
	let o1 = Array.length test_output.(0) in 
	let i1 = Array.length test_input.(0) in 
	let mean_in = mean train_input in
	let mean_out = mean train_output in
	let std_in = std train_input mean_in in
	let std_out = std train_output mean_out in
	let t = Array.make_matrix n o1 0. in
	for t_i=0 to n-1 do
		let error = analysis_error train_input train_output 
			test_input test_output [|i1; min_hidden + t_i;o1|]
			act_fun act_der rate 50 in
		t.(t_i) <- error;
	done;
	t
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

		
(*w_out so uteži enega sloja naprej(že popravljene!), a_m so izhodi v tem sloju, d_out je delta naslednjega sloja*)
let delta_hidden w_out a_m d_out = 
	let a = Array.length a_m in
	let d_0 = Array.length d_out in 
	let e = Array.make a 0. in
	for i = 0 to a -1 do
		let sum = ref 0. in
		for j = 0 to d_0 -1 do
			sum := !sum +. w_out.(j).(i) *. d_out.(j)
		done;
		e.(i) <- (d_sigmoid a_m.(i)) *. !sum
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
	
(*TEST*)

(*vhodni podatki*)
let x = [| 1. ; 1.; 1.; 1.|]
(*learning rate*)
let rate = 0.5
(*želen izhod*)
let d =[|0.191; 0.25|]
(*št nevronov v prvem hidden sloju*)
let layer1 = 3


let w1 = rand_weights layer1 (Array.length x) 5.
let w2 = rand_weights (Array.length d) layer1 5.

let l = combination_f x w1
let l1 = activation_layer l1
let l2 = combination_f l1 w2
let y = activation_layer l2  
let e_o = delta_output d y
let w2_new = update_weights w2 rate e_o l1
let e_hidden = delta_hidden w2_new l1 e_o
let w1_new = update_weights w1 rate e_hidden x


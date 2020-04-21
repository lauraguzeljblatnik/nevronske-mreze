#use "nevronske_mreze.ml";;

#use "csv_ocaml.ml";;




(* let lay = test_examples.(3);;

let out_lay = test_out.(3);;	 *)

(* let top1 = [|3;10;2;12;3|];; *)


let w = [|[|1.; 1.;1.;1.;1.;1.;1.;1.;1.;1.;1.;|];
[|0.; 1.;0.;0.;0.;0.;0.;0.;0.;0.;0.;|];
[|1.; 1.;1.;1.;1.;1.;1.;1.;1.;1.;1.;|];
[|1.; 1.;1.;1.;1.;1.;1.;1.;1.;1.;1.;|];
[|1.; 1.;1.;1.;1.;1.;1.;1.;1.;1.;1.;|]|];;



let top1 = [|1;3;1|];;


(* let lerarn_example_test = learning_example [|10.|] [|0.5|] [|[|1.|];[|0.;0.|];[|0.|]|] [|[|[|1.|];[|1.|]|]; [|[|1.;1.|]|]|] 1. sigmoid d_sigmoid *)

(* let predict_test = predict [|11.|] [|1;2;1|] lerarn_example_test sigmoid *)

let wei =   [|[|[|0.76594695592428386|]; [|-0.0287914411528156|];
      [|-0.12863740799826928|]; [|0.649951111889334|];
      [|-0.37240440413099885|]; [|1.9318063496133289|];
      [|0.66085046029785544|]; [|1.9867981411519844|];
      [|-0.10306741622833798|]; [|1.7239153258873936|]|];
    [|[|-1.339577207198831; -0.95478141399598282; -0.87221932009666459;
        -1.2374381676241106; -0.822782238706887; -1.6181914776335191;
        -1.3169744816251547; -1.6853794283881924; -1.0685219023423334;
        -1.5259079504420299|]|]|] ;;
		
let trained_weights1 = train_with_input_weights input out [|1;10;1|] 1. wei sigmoid d_sigmoid		
	
let trained_weights = train_network input out top1 1. 1.0 sigmoid d_sigmoid

let prediction = predict [|1.|] top1 trained_weights sigmoid

let prediction1 = predict [|1.|] [|1;10;1|] trained_weights1 sigmoid

let it_should_be = test_examples.(100)

let error = prediction.(0) -. it_should_be.(0)
let error1 = prediction1.(0) -. it_should_be.(0) 

(* (*TEST*)

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

(* let w1 = [|update_weights w 2. [|1.; 1.; 1.; 1.;1.|] [|1.; 1.; 1.; 8.; 1.; 1.; 1.; 1.; 1.; 1.; 1.|]|] *)

let d_h_test = delta_hidden [|[|0.96001880057; 0.960018800875|]|] [|0.999954; 0.999954|] [|0.0399830142681|]



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

let test = learning_example x d neurons weights rate  *)


(* let delta_out = delta_output d_sigmoid [|1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 10.; 1.|] *)
    (* [|0.; 2.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 9.; 0.|] *)
	
(* let d_h_test = delta_hidden [|[|0.43; 0.87|];[|0.45; 0.96|]|] [|0.9; 0.8|] [|0.3;0.4|] d_sigmoid *)
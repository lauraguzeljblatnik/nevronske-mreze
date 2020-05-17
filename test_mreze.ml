#use "nevronske_mreze.ml";;
#use "csv_ocaml.ml";;
	
let topology = [|11;3;5;3|]	

let network = initialize_network topology 


let w1 = create_weights_matrix topology 1.

let w = create_weights_matrix topology 1.

		
let mean_i = mean input	
let std_i = std input mean_i	

let mean_o = mean out	
let std_o = std out mean_o
	
let trained_weights = train_network input out topology 0.001 1.0 sigmoid d_sigmoid

let trained_w_input = train_with_input_weights input out topology 0.1 w1 sigmoid d_sigmoid

let norm_test = norm_z_score test_examples.(10) mean_i std_i

(* let prediction_unlearned1 = predict norm_test network w1 sigmoid *)
(* let denorm_pred_unlearned1 = denorm_z_score prediction_unlearned1 mean_o std_o *)

let prediction_learned_input = predict norm_test network trained_w_input sigmoid
let denorm_pred_learned_input = denorm_z_score prediction_learned_input mean_o std_o

let prediction_learned = predict norm_test network trained_weights sigmoid
let denorm_pred_learned = denorm_z_score prediction_learned mean_o std_o

let prediction_unlearned = predict norm_test network w sigmoid
let denorm_pred_unlearned = denorm_z_score prediction_unlearned mean_o std_o

(* let prediction1 = predict test_examples.(0) network top1 trained_weights sigmoid *)

let it_should_be = test_out.(10)

(* let error0 = denorm_pred.(0) -. it_should_be.(0) *)
(* let error1 = denorm_pred.(1) -. it_should_be.(1) *)
(* let error2 = denorm_pred.(2) -. it_should_be.(2) *)

(* let error1 = prediction1.(0) -. it_should_be.(0)  *)

(*REAL TEST*)

let error_mean_trained = evaluate test_examples test_out trained_weights topology sigmoid mean_i std_i mean_o std_o

let error_mean_untrained = evaluate test_examples test_out w topology sigmoid mean_i std_i mean_o std_o

let error_mean_trained_input = evaluate test_examples test_out trained_w_input topology sigmoid mean_i std_i mean_o std_o

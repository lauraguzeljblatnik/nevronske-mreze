#use "nevronske_mreze.ml";;
#use "csv_ocaml.ml";;
	
let topology = [|11; 5;2;3|]	

let w1 = create_weights_matrix topology 1.

let w = create_weights_matrix topology 1.

		
let mean_i = mean input	
let std_i = std input mean_i	

let mean_o = mean out	
let std_o = std out mean_o
	
let trained_weights = train_network input out topology 0.1 1.0 sigmoid d_sigmoid

let trained_w_input = train_with_input_weights input out topology 0.1 w1 sigmoid d_sigmoid

let norm_test = norm_z_score test_examples.(10) mean_i std_i

let prediction_unlearned1 = predict norm_test topology w1 sigmoid
let denorm_pred_unlearned1 = denorm_z_score prediction_unlearned1 mean_o std_o

let prediction_learned_input = predict norm_test topology trained_w_input sigmoid
let denorm_pred_learned_input = denorm_z_score prediction_learned_input mean_o std_o

let prediction_learned = predict norm_test topology trained_weights sigmoid
let denorm_pred_learned = denorm_z_score prediction_learned mean_o std_o

let prediction_unlearned = predict norm_test topology w sigmoid
let denorm_pred_unlearned = denorm_z_score prediction_unlearned mean_o std_o

(* let prediction1 = predict test_examples.(0) topology top1 trained_weights sigmoid *)

let it_should_be = test_out.(10)

(* let error0 = denorm_pred.(0) -. it_should_be.(0) *)
(* let error1 = denorm_pred.(1) -. it_should_be.(1) *)
(* let error2 = denorm_pred.(2) -. it_should_be.(2) *)

(* let error1 = prediction1.(0) -. it_should_be.(0)  *)


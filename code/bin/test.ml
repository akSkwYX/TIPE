(* let token_list_list = *)
(*   Lexing.sentence_to_token_list_list *)
(*     (In_channel.input_all *)
(*        (In_channel.open_text "dictionarys/ESCHYLE_SEPTCONTRETHEBES.txt")) *)
(* let token_list = List.flatten token_list_list *)
(* let line_list = List.map Token.token_to_line token_list *)
(* let () = Dictionary.update_dictionary line_list *)

(* let token_list_list = List.map Syntax_tree.get_token_list parsing_result *)
(* let line_list = List.fold_left ( *)
(*                   fun acc token_list -> *)
(*                     acc @ (List.map Token.token_to_line) token_list *)
(*                 ) [] token_list_list *)
(* let () = Dictionary.update_dictionary line_list *)

let parsing_result = Grammar.parse_sentence_from_string "la petit chat roug bois du lait"
let _ = Syntax_tree.print_syntax_tree_list parsing_result

let parsing_result = (Grammar.parse (Array.of_list (Token.sentence_to_token_list_list "le chat joue")))

let _ = List.iter (Syntax_tree.print_syntax_tree) parsing_result

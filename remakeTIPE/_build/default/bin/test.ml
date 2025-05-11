let parsing_result = Grammar.parse_sentence "la pitit chatte joue"
let _ = Syntax_tree.print_syntax_tree_list parsing_result

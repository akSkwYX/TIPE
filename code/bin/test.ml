let parsing_result = Grammar.parse_sentence_from_string "le petut chat rouges boits du lait"
let _ = Syntax_tree.print_syntax_tree_list parsing_result

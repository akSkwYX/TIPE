let size = Trie.size (Dictionary.dictionary)
let height = Trie.height (Dictionary.dictionary)
let () = Printf.printf "Size : %d\nHeight : %d" size height

(*let parsing_result = Grammar.parse_sentence_from_string "le petit chat rouge mange une souris grise"*)
(*let _ = Syntax_tree.print_syntax_tree_list parsing_result*)

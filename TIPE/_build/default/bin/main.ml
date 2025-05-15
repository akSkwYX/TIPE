let () = print_endline "Input sentence :"
let s = Utility.string_of_option @@ In_channel.input_line In_channel.stdin
let res = Grammar.parse_sentence_from_string s
let _ = Syntax_tree.print_syntax_tree_list res

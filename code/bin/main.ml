let () = print_endline "Input sentence :"
let s = Utility.string_of_option @@ In_channel.input_line In_channel.stdin
let res = Grammar.parse_sentence_from_string s
let sorted_res = List.sort (fun x y -> 
                              Corrections.distanceLevenshtein (Syntax_tree.st_get_word x) s -
                              Corrections.distanceLevenshtein (Syntax_tree.st_get_word y) s)
                            res
let _ = Syntax_tree.print_syntax_tree_list sorted_res
let distance_min = Corrections.distanceLevenshtein (Syntax_tree.st_get_word (List.hd sorted_res)) s
let min_res = List.take_while (fun x -> distance_min = Corrections.distanceLevenshtein (Syntax_tree.st_get_word x) s) sorted_res
let () = print_endline "Result :"
let () = List.iter (fun x -> print_endline (Syntax_tree.st_get_word x)) min_res

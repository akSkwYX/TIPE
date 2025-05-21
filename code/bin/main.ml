(*le jeune grçon rames*)

let () = print_endline "Input sentence :"
let s = Utility.string_of_option @@ In_channel.input_line In_channel.stdin
let res = Grammar.parse_sentence_from_string s
let sorted_res_lev = List.sort (fun x y -> 
                              Corrections.distanceLevenshtein (Syntax_tree.st_get_word x) s -
                              Corrections.distanceLevenshtein (Syntax_tree.st_get_word y) s)
                            res
let _ = Syntax_tree.print_syntax_tree_list sorted_res_lev
let distance_min = Corrections.distanceLevenshtein (Syntax_tree.st_get_word (List.hd sorted_res_lev)) s
let min_res_lev = List.take_while (fun x -> distance_min = Corrections.distanceLevenshtein (Syntax_tree.st_get_word x) s) sorted_res_lev
let () = print_newline ()
let () = print_endline "Result with levenshtein distance : "
let () = print_int distance_min
let () = print_newline ()
let () = List.iter (fun x -> print_endline (Syntax_tree.st_get_word x)) min_res_lev

let sort_fun x y =
  let token_list_x = Syntax_tree.get_token_list x in
  let token_list_y = Syntax_tree.get_token_list y in
  let c_x = List.fold_left (fun acc x -> acc + (Tags.get_frequency @@ Token.get_tags x)) 0 token_list_x in
  let c_y = List.fold_left (fun acc y -> acc + (Tags.get_frequency @@ Token.get_tags y)) 0 token_list_y in
  c_y - c_x
let sorted_res_freq = List.sort sort_fun res
let freq_min = List.fold_left (fun acc x -> acc + (Tags.get_frequency @@ Token.get_tags x))
                               0 (Syntax_tree.get_token_list (List.hd sorted_res_freq))
let min_res_freq = List.take_while (
                     fun x ->
                       freq_min = List.fold_left (fun acc y -> acc + (Tags.get_frequency (Token.get_tags y))) 0 (Syntax_tree.get_token_list x)
                   ) sorted_res_freq
let () = print_newline ()
let () = print_endline "Result with frequency : "
let () = print_int freq_min
let () = print_newline ()
let () = List.iter (fun x -> print_endline (Syntax_tree.st_get_word x)) min_res_freq

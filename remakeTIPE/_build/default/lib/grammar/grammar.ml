open Syntax_tree
open Token

(**
  Chore of the parsing algorithm.
  Iterate over the list of possible solution which are being parsed.
  For each of them, iterate over the list of token (possible meanings).
  For each token, apply the match function to get the result of parsing the token 
  with the current syntax tree.
*)
let rec iterate_parse token_list_array match_fun precedent_result new_result = 
  match precedent_result with
  | [] -> new_result
  | (syntax_tree_list, index) :: tl ->
    let token_list = token_list_array.(index) in
    let rec iterate_token_list token_list result_solution =
      match token_list with
      | [] -> result_solution
      | token :: tl ->
        let further_parse_result = match_fun token syntax_tree_list index in
        iterate_token_list tl (further_parse_result @ result_solution)
    in
    let result_parsing = iterate_token_list token_list [] in
    match result_parsing with
    | [] -> iterate_parse token_list_array match_fun tl new_result
    | _ -> iterate_parse token_list_array match_fun tl (result_parsing @ new_result)

(**
  Parse the sentence and return a list of possible syntax tree.

  Each function return a list of possible syntax tree associated with the index of
  the next token to parse.
*)
let rec parse token_list_array =
  let solution_list = parse_verbal_group token_list_array [([], 0)] in
  List.map (
    fun (syntax_tree_list, index) ->
      match syntax_tree_list with
      | [] -> Empty
      | [Node (token, _)] -> Node (SENTENCE ((get_word token), []), syntax_tree_list)
      | _ -> Error "Unmerged syntax tree"
  ) solution_list

and parse_verbal_group token_list_array result =
  let result_parsing_subject = parse_subject token_list_array result in
  let result_parsing_verb = parse_verb token_list_array result_parsing_subject in
  List.map (
    fun (syntax_tree_list, index) ->
      match syntax_tree_list with
      | [subject; verb] -> (Corrections.correct_verbal_group subject verb, index)
      | _ -> ([Error "Verbal group parsing get other than two syntax tree"], index)
  ) result_parsing_verb

and parse_subject token_list_array result =
  let match_fun token syntax_tree_list index =
    match token with
    | NOUN _ | DETERMINER _ | ADJECTIVE _ ->
      List.map (
        fun (syntax_tree_list, index) ->
          match syntax_tree_list with
          | [x] -> ([Node (SUBJECT (st_get_word x, st_get_tags x), [x])], index)
          | _ -> ([Error "Subject parsing get other than one syntax tree"], index)
      ) (parse_nominal_group token_list_array result)
    | PERSONAL_PRONOUN_SUBJECT _ ->
      List.map (
        fun (syntax_tree_list, index) ->
          match syntax_tree_list with
          | [x] -> ([Node (SUBJECT (st_get_word x, st_get_tags x), [x])], index)
          | _ -> ([Error "Subject parsing get other than one syntax tree"], index)
      ) (parse_personal_pronoun_subject token_list_array result)
    | _ -> []
  in
  iterate_parse token_list_array match_fun result [] 

and parse_verb token_list_array result =
  let match_fun token syntax_tree_list index =
    match token with
    | VERB _ ->
      [(syntax_tree_list @ [Node (token, [Leaf token])] , index + 1)]
    | _ -> []
  in
  iterate_parse token_list_array match_fun result []

and parse_personal_pronoun_subject token_list_array result =
  let match_fun token syntax_tree_list index =
    match token with
    | PERSONAL_PRONOUN_SUBJECT _ ->
      [(syntax_tree_list @ [Node (token, [Leaf token])] , index + 1)]
    | _ -> []
  in
  iterate_parse token_list_array match_fun result []

and parse_nominal_group token_list_array result =
  let result_parsing_determiner = parse_determiner token_list_array result in
  let result_parsing_adjective = parse_adjectives token_list_array result_parsing_determiner in
  let result_parsing_noun = parse_noun token_list_array result_parsing_adjective in
  let result_parsing_adjective = parse_adjectives token_list_array result_parsing_noun in
  List.map (
    fun (syntax_tree_list, index) ->
      match syntax_tree_list with
      | [determiner; adjective1; noun; adjective2] -> 
        (Corrections.correct_nominal_group determiner adjective1 noun adjective2, index)
      | _ -> ([Error "Verbal group parsing get other than 4 syntax tree"], index)
  ) result_parsing_adjective

and parse_determiner token_list_array result =
  let match_fun token syntax_tree_list index =
    match token with
    | DETERMINER _ ->
      [(syntax_tree_list @ [Node (token, [Leaf token])] , index + 1)]
    | _ -> []
  in
  iterate_parse token_list_array match_fun result []

and parse_adjective token_list_array result =
  let match_fun token syntax_tree_list index =
    match token with
    | ADJECTIVE _ ->
      [(syntax_tree_list @ [Node (token, [Leaf token])] , index + 1)]
    | _ -> [(syntax_tree_list @ [Empty], index)]
  in
  iterate_parse token_list_array match_fun result []

(**
  Parse the adjectives of the nominal group.

  Recursively try to parse adjectives until no more adjective can be parsed for 
  each of the possible syntax tree.

  The fold_left is used to iterate over the list of possible syntax tree and
  separate the items which are complete (no longer adjective to parse) and the
  items which are uncomplete (still adjective to parse).
*)
and parse_adjectives token_list_array result =
  let result_parsing_adjective = parse_adjective token_list_array result in
  let complete_adjective_parsing, uncomplete_adjective_parsing =
  List.fold_left (
    fun (complete_adjective_parsing, uncomplete_adjective_parsing) (syntax_tree_list, index) ->
      let rec aux2 syntax_tree_list res =
        match syntax_tree_list with
        | Node (ADJECTIVE (word, tags), [children]) as tree1 :: [Node (ADJECTIVE (word2, tags2), [children2]) as tree2] ->
          let syntax_tree_result = Corrections.correct_adjectives tree1 tree2 in
          (complete_adjective_parsing,
           uncomplete_adjective_parsing @ [res @ syntax_tree_result, index])
        | Node (ADJECTIVE (word, tags), children) :: [Empty] ->
          (complete_adjective_parsing @ [res @ [Node (ADJECTIVE (word, tags), children @ [Empty])] , index],
           uncomplete_adjective_parsing)
        | [Empty] ->
          (complete_adjective_parsing @ [res @ [Node (ADJECTIVE ("", []), [Empty])], index], uncomplete_adjective_parsing)
        | h :: t -> aux2 t (res @ [h])
        | [] -> (complete_adjective_parsing, uncomplete_adjective_parsing)
      in
      aux2 syntax_tree_list []
  ) ([], []) result_parsing_adjective
  in
  match uncomplete_adjective_parsing with
  | [] -> complete_adjective_parsing
  | _ -> complete_adjective_parsing @ parse_adjectives token_list_array uncomplete_adjective_parsing

and parse_noun token_list_array result =
  let match_fun token syntax_tree_list index =
    match token with
    | NOUN _ ->
      [(syntax_tree_list @ [Node (token, [Leaf token])] , index + 1)]
    | _ -> []
  in
  iterate_parse token_list_array match_fun result []

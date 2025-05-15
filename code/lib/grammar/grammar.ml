open Syntax_tree
open Token

let is_equal_result (t_l1, i1) (t_l2, i2) =
  if i1 = i2 then
    List.for_all2 Syntax_tree.is_equal t_l1 t_l2
  else
    false

(**
  @param token_list_array  @type 'a list array
  the list of token to parse (i.e. the sentence)
  @param match_fun @type 'a -> 'b -> int -> 'c list
  the function to apply to each token
  @param precedent_result @type ('a list * int) list
  the list of possible solution which are being parsed
  @param new_result @type ('a list * int) list
  the result of the parsing algorithm applied to the current token (* quite unclear, need to rephrase *)

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
    begin
    let error = Syntax_tree.contains_error syntax_tree_list in
    if error <> [] then iterate_parse token_list_array match_fun tl ((error, index) :: new_result) else
    if index >= Array.length token_list_array then
      iterate_parse token_list_array match_fun tl ((syntax_tree_list @ [Empty], index) :: new_result) else
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
    | _ -> let next_new_result =
             List.fold_left (
               fun acc r ->
                 if List.exists (is_equal_result r) acc then
                   acc
                 else
                   r :: acc
             ) new_result result_parsing
           in
           iterate_parse token_list_array match_fun tl next_new_result
    end

(**
  Parse the sentence and return a list of possible syntax tree.

  Each function return a list of possible syntax tree associated with the index of
  the next token to parse.
*)
let rec parse token_list_array =
  let solution_list = parse_sentence token_list_array [([], 0)] in
  let sentence_length = Array.length token_list_array in
  List.filter_map (
    fun (syntax_tree_list, index) ->
      match syntax_tree_list with
      | [Node (token, _) as tree] -> if index >= sentence_length then
                                       Some tree
                                     else
                                       None
      | _ -> None
  ) solution_list

and parse_sentence token_list_array result =
  let result_parsing_subject = parse_subject token_list_array result in
  let result_parsing_verbal_group = parse_verbal_group token_list_array result_parsing_subject in
  List.fold_left ( 
    fun acc (syntax_tree_list, index) ->
      match syntax_tree_list with
      | [subject; verbal_group] -> 
        List.map (fun x -> ([x], index)) (Corrections.correct_sentence subject verbal_group) @ acc
      | _ -> ([Error "Sentence parsing get other than two syntax tree"], index) :: acc
  ) [] result_parsing_verbal_group

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


and parse_verbal_group token_list_array result =
  let result_parsing_pronoun = parse_pronoun token_list_array result in
  let result_parsing_verb = parse_verb token_list_array result_parsing_pronoun in
  let result_parsing_complement = parse_complement token_list_array result_parsing_verb in
  List.fold_left (
    fun acc (syntax_tree_list, index) ->
      match syntax_tree_list with
      | [subject; pronoun; verb; complement] ->
        List.map (fun x -> ([subject; x], index)) (Corrections.correct_verbal_group pronoun verb complement) @ acc
      | _ -> ([Error "Verbal group parsing get other than four syntax tree"], index) :: acc
  ) [] result_parsing_complement

and parse_pronoun token_list_array result =
  let match_fun token syntax_tree_list index =
    match token with
    | PRE_VERB_PRONOUN _ ->
      [(syntax_tree_list @ [Node (token, [Leaf token])] , index + 1)]
    | _ -> [syntax_tree_list @ [Empty], index]
  in
  iterate_parse token_list_array match_fun result []

and parse_verb token_list_array result =
  let match_fun token syntax_tree_list index =
    match token with
    | VERB (word, tags) ->
        if Checkings.is_auxiliary token then
          let next_token_list = token_list_array.(index + 1) in
          (syntax_tree_list @ [Node (token, [Leaf token])] , index + 1) ::
          List.fold_left (
            fun acc possibility_token ->
              match possibility_token with
              | VERB (main_verb, main_tags) ->
                let res = Corrections.correct_auxiliary_verb (Node (token, [Leaf token]))
                                                             (Node (possibility_token, [Leaf possibility_token]))
                in
                (syntax_tree_list @ res, index + 2) :: acc
              | _ -> acc
          ) [] next_token_list
        else
          [(syntax_tree_list @ [Node (token, [Leaf token])] , index + 1)]
    | _ -> []
  in
  iterate_parse token_list_array match_fun result []

and parse_complement token_list_array result =
  let match_fun token syntax_tree_list index =
    match token with
    | PREPOSITION (w, t) ->
      (*TODO : CC + COS*)
      begin
      if Checkings.is_preposition_coi token then
        List.fold_left (
          fun acc (syntax_tree_list, index) ->
            match syntax_tree_list with
            | [subject; pronoun; verb; preposition; coi] ->
              List.map (fun x -> ([subject; pronoun; verb] @ [x], index))
                       (Corrections.correct_coi preposition coi)
              @ acc
            | _ -> failwith "grammar.ml/parse_complement : coi parsing result in more than one syntax tree"
          ) [] (parse_coi token_list_array (parse_preposition token_list_array result))
      else
        failwith "grammar.ml/parse_complement : not implemented yet"
      end
    | _ ->
      List.fold_left (
        fun acc (syntax_tree_list, index) ->
          match syntax_tree_list with
          | [subject; pronoun; verb; cod] -> List.map (fun stl -> ([subject; pronoun; verb] @ [ stl ], index)) 
                                                      (Corrections.correct_cod cod)
                                             @ acc
          | _ -> failwith "grammar.ml/parse_complement : cod parsing result in more than one syntax tree"
      ) [] (parse_cod token_list_array result)
  in
  iterate_parse token_list_array match_fun result []

and parse_preposition token_list_array result =
  let match_fun token syntax_tree_list index =
    match token with
    | PREPOSITION (w, t) ->
      [(syntax_tree_list @ [Node (token, [Leaf token])] , index + 1)]
    | _ -> [([Error "Trying to parse a preposition but got something else"], index)]
  in
  iterate_parse token_list_array match_fun result []

and parse_cod token_list_array result =
  let match_fun token syntax_tree_list index =
    match token with
    | DETERMINER _ | NOUN _ | ADJECTIVE _ ->
      let gn = parse_nominal_group token_list_array [([], index)] in
      List.map (fun (gn, index_gn) -> 
        match gn with
        | [Node (NOMINAL_GROUP (word, tags) as token, children)] ->
          (syntax_tree_list @ [Node (COD (word, []), gn)] , index_gn)
        | _ -> failwith "grammar.ml/parse_cod : receive more than one syntax tree from parse_nominal_group"
      ) gn
    | VERB (word, tags)
    | PROPER_NOUN (word, tags) ->
      [(syntax_tree_list @ [Node ((COD (word, [])), [Node (token, [Leaf token])])] , index + 1)]
    | _ -> []
  in
  iterate_parse token_list_array match_fun result []

and parse_coi token_list_array result =
  let match_fun token syntax_tree_list index =
    List.map (fun (gn, index_gn) ->
      (syntax_tree_list @ gn, index_gn)
    ) ( parse_nominal_group token_list_array [([], index)] )
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
  List.fold_left (
    fun acc (syntax_tree_list, index) -> 
    match syntax_tree_list with
      | [determiner; adjective1; noun; adjective2] -> 
        List.map (fun x -> ([x], index)) (Corrections.correct_nominal_group determiner adjective1 noun adjective2) @ acc
      | _ -> ([Error "Nominal group parsing get other than 4 syntax tree"], index) :: acc
  ) [] result_parsing_adjective

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
        | Node (ADJECTIVE (word, tags), children) as tree1 :: [Node (ADJECTIVE (word2, tags2), children2) as tree2] ->
          let syntax_tree_result = Corrections.correct_adjectives tree1 tree2 in
          (complete_adjective_parsing,
           uncomplete_adjective_parsing @ [res @ syntax_tree_result, index])
        | Node (ADJECTIVE (word, tags), children) as tree :: [Empty] ->
          (complete_adjective_parsing @ [res @ [Node (ADJECTIVE (word, tags), [tree ;Empty])] , index],
           uncomplete_adjective_parsing)
        | Node (ADJECTIVE (word, tags), [children]) as tree :: [] ->
          (complete_adjective_parsing,
           uncomplete_adjective_parsing @ [res @ [tree], index])
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


let parse_sentence_from_string (s:string) =
  parse (Array.of_list (Lexing.sentence_to_token_list_list s))

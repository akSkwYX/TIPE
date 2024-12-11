open Syntax_tree

let rad_dict = Dictionnary.rad_dictionnary

let is_verb word_informations =
  match word_informations with
  | rad_word :: "V" :: _ -> true
  | _ -> false

let seek_word word_token =
  match word_token with
  | Token.Token (_, (_, (rad_word::_))) 
    -> 
    begin
      let (is_word, potential_correction) = Trie.trie_search rad_dict rad_word in
      let rec get_correction_token_list potential_correction res =
        match potential_correction with
        | [] -> res
        | (word :: "V" :: complementary_informations) :: t -> get_correction_token_list t ((Token.get_token_from_informations word (rad_word :: "V" :: complementary_informations)) :: res)
        | h::t -> get_correction_token_list t res
      in
      get_correction_token_list potential_correction []
    end
  | _ -> failwith "seek_word : invalid token"

let correct_verb subject_token verb_token =
  let potential_correction_token_list = seek_word verb_token in
  let rec aux l res =
    match l with
    | [] -> res
    | h :: t -> let (is_correct, informations) = Checkings.check_subject_verb subject_token h in
                if is_correct then aux t ((h, informations) :: res)
                else aux t res
  in
  aux potential_correction_token_list []

let construct_verb_tree verb_token =
  match verb_token with
		| Token.Token (Word_classe.Verbe, (verb, informations)) -> Node (Word_classe.Verbe, informations, [Leaf verb])
    | _ -> failwith "construct_verb_tree : invalid token"

let correct_verbal_group subject_tree subject_token verb_tree verb_token :(syntax_tree * Token.token) list =
  let (is_correct, informations, error) = Checkings.check_verbal_group subject_token verb_token in
  if is_correct then
    let token = Token.Token ( Word_classe.GV, ((Token.get_word subject_token) ^ " " ^ (Token.get_word verb_token), informations) ) in
    [(Node (Word_classe.GV, informations, [subject_tree; verb_tree]), token)]
  else
    begin
      print_string "Error on verbal group :: Wrong conjugation of : "; print_string (Token.get_word verb_token); print_newline ();
      print_string "Trying to fix..."; print_newline ();
      let correct_token_information_list = correct_verb subject_token verb_token in
      print_string "Suggested corrections : "; print_newline ();
      List.iter (fun (token, informations) -> Printf.printf "- %s -" (Token.get_word token)) correct_token_information_list; print_newline ();
      List.map (fun (token, informations) -> 
                (Node (Word_classe.GV,
                      informations,
                      [subject_tree; (construct_verb_tree token)]),
                      (Token.Token ( Word_classe.GV, ((Token.get_word subject_token) ^ " " ^ (Token.get_word token), informations) )))
               )
               correct_token_information_list
    end
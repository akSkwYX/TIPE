open Syntax_tree

let rad_dict = Dictionnary.rad_dictionnary

let seek_word word =
  let (is_word, potential_correction) = Trie.trie_search rad_dict word in
  Token.get_token_from_informations word potential_correction

let correct_verb subject_token verb_token =
  let verb = Token.get_word verb_token in
  let potential_correction_token_list = seek_word verb in
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
      let correct_token_information_list = correct_verb subject_token verb_token in
      List.iter (fun (token, informations) -> Printf.printf "Correction : %s\n" (Token.get_word token)) correct_token_information_list;
      List.map (fun (token, informations) -> (Node (Word_classe.GV, informations, [subject_tree; (construct_verb_tree verb_token)]), token)) correct_token_information_list
    end
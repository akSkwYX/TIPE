open Syntax_tree

let rad_dict = Dictionnary.rad_dictionnary

(* Distance between words : Distance of Levenshtein *)

let distanceLevenshtein word_1 word_2 =

  let len_1 = String.length word_1 in
  let len_2 = String.length word_2 in
  
  let matrix_distance = Array.make_matrix 2 (len_2 + 1) 0 in
  matrix_distance.(1).(0) <- 1;
  for j = 1 to len_2 do
    matrix_distance.(0).(j) <- j
  done;
  
  for i = 1 to len_1 do
    for j = 1 to len_2 do
      let cost = if word_1.[i - 1] = word_2.[j - 1] then 0 else 1 in
      matrix_distance.(1).(j) <- min (min (matrix_distance.(0).(j) + 1) (matrix_distance.(1).(j - 1) + 1)) (matrix_distance.(0).(j - 1) + cost)
    done;
    for j = 0 to len_2 do
      matrix_distance.(0).(j) <- matrix_distance.(1).(j)
    done;
    matrix_distance.(1).(0) <- i+1
  done;

  matrix_distance.(1).(len_2)

let min_distance word l =
  let rec aux l min_word min_result min_distance =
    match l with
    | [] -> min_result
    | (Token.Token (_, (word_2, _)),_) as result :: t -> let d = distanceLevenshtein word word_2 in
                                            if d < min_distance then aux t word_2 result d
                                            else aux t min_word min_result min_distance 
    | _ -> failwith "min_distance : invalid token"
  in
  match l with
  | [] -> failwith "min_distance : empty list"
  | (token, informations) :: t -> aux t (Token.get_word token) (token, informations) (distanceLevenshtein word (Token.get_word token))

(*Misspelled correction *)

let get_correction_possibility_for_word word = ()
  (* TODO *)

(* Verb correction *)

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
  min_distance (Token.get_word verb_token) (aux potential_correction_token_list [])

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
      let (corrected_token, corrected_informations) = correct_verb subject_token verb_token in
      print_string "Suggested corrections : "; print_string (Token.get_word corrected_token); print_newline ();
      (* List.map (fun (token, informations) -> 
                (Node (Word_classe.GV,
                      informations,
                      [subject_tree; (construct_verb_tree token)]),
                      (Token.Token ( Word_classe.GV, ((Token.get_word subject_token) ^ " " ^ (Token.get_word token), informations) )))
               )
               correct_token_information_list
      *)
      [(Node (Word_classe.GV, informations, [subject_tree; (construct_verb_tree corrected_token)]), (Token.Token ( Word_classe.GV, ((Token.get_word subject_token) ^ " " ^ (Token.get_word corrected_token), informations ))) )]
      end

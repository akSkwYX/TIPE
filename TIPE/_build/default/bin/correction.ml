open Syntax_tree

let rad_dict = Dictionnary.rad_dictionnary

(* Distance between words : Distance of Levenshtein *)

let distance word_1 word_2 =

  let word_list_1 = Utility.char_list_of_string word_1 in
  let word_list_2 = Utility.char_list_of_string word_2 in
  let length_1 = String.length word_1 in
  let length_2 = String.length word_2 in
  let min_length = min length_1 length_2 in
  let max_length = max length_1 length_2 in
  let matrix_distance = Array.make_matrix 2 (min_length+1) 0 in
  matrix_distance.(0) <- Array.mapi (fun i x -> i) matrix_distance.(0);
  matrix_distance.(1).(0) <- 1;
  
  for _ = 0 to max_length-1 do
    for j = 1 to min_length do
      match word_list_1, word_list_2 with
      | h1::t1, h2::t2 when h1=h2 -> matrix_distance.(1).(j) <- matrix_distance.(0).(j-1)
      | h1::t1, h2::t2 -> matrix_distance.(1).(j) <- min (min (matrix_distance.(0).(j) + 1) (matrix_distance.(1).(j-1) + 1)) (matrix_distance.(0).(j-1)+1)
      | _ -> failwith "distance : word_1 longer than word_2"
    done;
    Utility.print_int_matrix matrix_distance; print_newline ();
    matrix_distance.(0) <- matrix_distance.(1);
  done;
  matrix_distance.(1).(min_length)
  

(* Misspelled correction *)

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

let () = print_int (distance "dors" "dort"); print_newline ()
(* let () = print_int (distance "chien" "chat"); print_newline ()
let () = print_int (distance "couille" "couille"); print_newline () *)
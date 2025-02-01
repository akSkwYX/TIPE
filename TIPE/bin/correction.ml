open Syntax_tree

let rad_dict = Dictionnary.rad_dictionnary

(* Distance between words : Distance of Levenshtein *)

(** 
  Calculates the Levenshtein distance between two words.

  The Levenshtein distance is a measure of the difference between two sequences. 
  It is the minimum number of single-character edits (insertions, deletions, or substitutions) 
  required to change one word into the other.

  @param word_1 The first word.
  @param word_2 The second word.
  @return The Levenshtein distance between the two words.
*)
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

(** 
  [min_distance word l] returns the element from the list [l] that has the minimum Levenshtein distance to the given [word].

  @param word The reference word to compare against.
  @param l A list of tuples where each tuple contains a token and its associated information.
  @return The tuple from the list [l] that has the smallest Levenshtein distance to [word].
  @raise Failure if the list [l] is empty or contains an invalid token.

  The function uses a helper function [aux] to recursively traverse the list and keep track of the current minimum distance and the corresponding result.
*)
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

(* Global functions *)

(** 
  [seek_word word_token] takes a [word_token] and attempts to find potential corrections for it.
  
  @param word_token The token representing the word to be corrected.
  
  @return A list of tokens representing potential corrections for the given word token.
  
  @raise Failure if the [word_token] does not match the expected pattern or if an invalid token is encountered during processing.
  
  The function works as follows:
  - It matches the [word_token] to extract the word class and the root word.
  - It converts the word class to a short string representation.
  - It searches for potential corrections in a trie structure using the root word.
  - It filters the potential corrections to find those that match the word class.
  - It constructs a list of correction tokens from the filtered potential corrections.
*)
let seek_word word_token =
  match word_token with
  | Token.Token (wc, (_, (rad_word::_) ))
    ->
    begin
      let seek_word_class = Word_classe.word_class_to_short_string wc in
      let (is_word, potential_correction) = Trie.trie_search rad_dict rad_word in
      let rec get_correction_token_list potential_correction res =
        match potential_correction with
        | [] -> res
        | (frequency, word :: word_class :: complementary_informations) :: t when word_class = seek_word_class -> get_correction_token_list t ((Token.get_token_from_informations word (rad_word :: seek_word_class :: complementary_informations)) :: res)
        | ( (frequency, word :: word_class :: complementary_informations) as h) ::t -> get_correction_token_list t res
        | _ -> failwith "seek_word : invalid token"
      in
      get_correction_token_list potential_correction []
    end
  | _ -> failwith "seek_word : invalid token"

(* Verb correction *)

let is_verb word_informations =
  match word_informations with
  | rad_word :: "V" :: _ -> true
  | _ -> false

(** [correct_verb subject_token verb_token] takes a subject token and a verb token,
  and returns a tuple containing the verb token with the minimum distance to the 
  original verb token and a list of checked verb tokens with their corresponding 
  information.

  @param subject_token The token representing the subject.
  @param verb_token The token representing the verb to be corrected.
  @return A tuple where the first element is the verb token with the minimum 
      distance to the original verb token, and the second element is a list 
      of tuples containing the checked verb tokens and their corresponding 
      information.
*)
let correct_verb subject_token verb_token =
  let potential_correction_token_list = seek_word verb_token in
  let rec aux l res =
    match l with
    | [] -> res
    | h :: t -> let (is_correct, informations) = Checkings.check_subject_verb subject_token h in
                if is_correct then aux t ((h, informations) :: res)
                else aux t res
  in
  let checked_list = aux potential_correction_token_list [] in
  (min_distance (Token.get_word verb_token) checked_list, checked_list)

let construct_verb_tree verb_token =
  match verb_token with
		| Token.Token (Word_classe.Verbe, (verb, informations)) -> Node (Word_classe.Verbe, informations, [Leaf verb])
    | _ -> failwith "construct_verb_tree : invalid token"

(** [correct_verbal_group subject_tree subject_token verb_tree verb_token] 
  checks and corrects the verbal group formed by the given subject and verb tokens.

  @param subject_tree The syntax tree of the subject.
  @param subject_token The token representing the subject.
  @param verb_tree The syntax tree of the verb.
  @param verb_token The token representing the verb.
  @return A list containing a tuple with the corrected syntax tree and the corresponding token.

  The function performs the following steps:
  1. Checks if the verbal group is correct using [Checkings.check_verbal_group].
  2. If the verbal group is correct, it constructs a new token and returns the syntax tree and token.
  3. If the verbal group is incorrect, it prints an error message and attempts to correct the verb.
  4. Prints the suggested correction and other possible corrections.
  5. Constructs and returns the corrected syntax tree and token.
*)
let correct_verbal_group subject_tree subject_token verb_tree verb_token :(syntax_tree * Token.token) list =
  let (is_correct, informations, error) = Checkings.check_verbal_group subject_token verb_token in
  if is_correct then
    match verb_tree with
    | Empty -> 
      begin
        match subject_tree with
        | Node (Word_classe.Sujet, informations_subject, [Node (Word_classe.GN, informations, [ Node (Word_classe.Determinant, _, _) as det_tree ; Node (Word_classe.Nom, _, _) as noun_tree])])
          -> let token = Token.Token ( Word_classe.GN, (Token.get_word subject_token, informations)) in
              [(Node (Word_classe.GN, informations, [det_tree; noun_tree]), token)]
        | Node (Word_classe.Sujet, informations_subject, [Node (Word_classe.GN, informations, [ Node (Word_classe.Determinant, _, _) as det_tree; Node (Word_classe.Adjectif, _, _) as adj_tree; Node (Word_classe.Nom, _, _) as noun_tree])])
        | Node (Word_classe.Sujet, informations_subject, [Node (Word_classe.GN, informations, [ Node (Word_classe.Determinant, _, _) as det_tree; Node (Word_classe.MultipleAdj, _, _) as adj_tree; Node (Word_classe.Nom, _, _) as noun_tree])])
        -> let token = Token.Token ( Word_classe.GN, (Token.get_word subject_token, informations)) in
              [(Node (Word_classe.GN, informations, [det_tree; adj_tree; noun_tree]), token)]
        | Node (Word_classe.Sujet, informations_subject, [Node (Word_classe.GN, informations, [ Node (Word_classe.Determinant, _, _) as det_tree; Node (Word_classe.Adjectif, _, _) as noun_tree; Node (Word_classe.Adjectif, _, _) as adj_tree_2])])
        | Node (Word_classe.Sujet, informations_subject, [Node (Word_classe.GN, informations, [ Node (Word_classe.Determinant, _, _) as det_tree; Node (Word_classe.MultipleAdj, _, _) as noun_tree; Node (Word_classe.Adjectif, _, _) as adj_tree_2])])
        -> let token = Token.Token ( Word_classe.GN, (Token.get_word subject_token, informations)) in
              [(Node (Word_classe.GN, informations, [det_tree; noun_tree; adj_tree_2]), token)]
        | Node (Word_classe.Sujet, informations_subject, [Node (Word_classe.GN, informations, [ Node (Word_classe.Determinant, _, _) as det_tree; Node (Word_classe.Adjectif, _, _) as adj_tree; Node (Word_classe.Nom, _, _) as noun_tree; Node (Word_classe.Adjectif, _, _) as adj_tree_2])])
        | Node (Word_classe.Sujet, informations_subject, [Node (Word_classe.GN, informations, [ Node (Word_classe.Determinant, _, _) as det_tree; Node (Word_classe.MultipleAdj, _, _) as adj_tree; Node (Word_classe.Nom, _, _) as noun_tree; Node (Word_classe.Adjectif, _, _) as adj_tree_2])])
        | Node (Word_classe.Sujet, informations_subject, [Node (Word_classe.GN, informations, [ Node (Word_classe.Determinant, _, _) as det_tree; Node (Word_classe.Adjectif, _, _) as adj_tree; Node (Word_classe.Nom, _, _) as noun_tree; Node (Word_classe.MultipleAdj, _, _) as adj_tree_2])])
        | Node (Word_classe.Sujet, informations_subject, [Node (Word_classe.GN, informations, [ Node (Word_classe.Determinant, _, _) as det_tree; Node (Word_classe.MultipleAdj, _, _) as adj_tree; Node (Word_classe.Nom, _, _) as noun_tree; Node (Word_classe.MultipleAdj, _, _) as adj_tree_2])])
          -> let token = Token.Token ( Word_classe.GN, (Token.get_word subject_token, informations)) in
              [(Node (Word_classe.GN, informations, [det_tree; adj_tree; noun_tree; adj_tree_2]), token)]
        | _ -> failwith "correction.ml - correct_verbal_group : invalid subject_tree"
      end
    | _ ->
      let token = Token.Token ( Word_classe.GV, ((Token.get_word subject_token) ^ " " ^ (Token.get_word verb_token), []) ) in
      [(Node (Word_classe.GV, [], [subject_tree; verb_tree]), token)]
  else
    begin
      print_newline ();
      print_string "Error on verbal group ::"; print_newline ();
      print_string "Subject is : "; Token.print_token subject_token; print_newline ();
      print_string "Verb is : "; Token.print_token verb_token; print_newline (); 
      print_string "Wrong conjugation of "; print_string (Token.get_word verb_token); print_newline ();
      print_string "Trying to fix..."; print_newline ();
      let ((corrected_token, corrected_informations), other_possibility) = correct_verb subject_token verb_token in
      print_string "Suggested correction : "; print_string (Token.get_word corrected_token); print_newline ();
      print_string "Other possibilities : "; List.iter (fun (t,_) -> print_string (Token.get_word t); print_string ", ") other_possibility; print_newline ();
      [(Node (Word_classe.GV, informations, [subject_tree; (construct_verb_tree corrected_token)]), (Token.Token ( Word_classe.GV, ((Token.get_word subject_token) ^ " " ^ (Token.get_word corrected_token), informations ))) )]
      end


(* nominal group correction *)


(* determiner & noun *)
(* We based the correction on the determiner if the noun is variable and if not we change the determiner *)

let is_determiner word_informations =
  match word_informations with
  | _ :: "D" :: _ -> true
  | _ -> false

let correct_determiner determiner_token noun_token =
  let potential_correction_token_list = seek_word determiner_token in
  Token.print_token_list potential_correction_token_list;
  let rec aux l res =
    match l with
    | [] -> res
    | h :: t -> let (is_correct, informations) = Checkings.check_det_noun h noun_token in
                if is_correct then aux t ((h, informations) :: res)
                else aux t res
  in
  let checked_list = aux potential_correction_token_list [] in
  match checked_list with
  | [] -> ((determiner_token, []), [])
  | _ -> (min_distance (Token.get_word determiner_token) checked_list, checked_list)

let construct_determiner_tree determiner_token =
  match determiner_token with
    | Token.Token (Word_classe.Determinant, (determiner, informations)) -> Node (Word_classe.Determinant, informations, [Leaf determiner])
    | _ -> failwith "Correction.ml - construct_determiner_tree : invalid token"

let correct_determiner_noun det_tree det_token noun_tree noun_token =
  let (is_correct, informations) = Checkings.check_det_noun det_token noun_token in
  if is_correct then
    (det_tree, det_token)
  else
    begin
      print_string "Error between determiner and noun :: Wrong agreement between "; print_string ((Token.get_word det_token) ^ " and " ^ (Token.get_word noun_token)); print_newline ();
      print_string "Trying to fix..."; print_newline ();
      let ((corrected_token, corrected_informations), other_possibility) = correct_determiner det_token noun_token in
      print_string "Suggested correction : "; print_string (Token.get_word corrected_token); print_newline ();
      print_string "Other possibilities : "; List.iter (fun (t,_) -> print_string (Token.get_word t); print_string ", ") other_possibility; print_newline (); print_newline ();
      (construct_determiner_tree corrected_token, corrected_token)
      end

(* noun & adjective *)

let is_adjective word_informations =
  match word_informations with
  | _ :: "A" :: _ -> true
  | _ -> false

let correct_adjective noun_token adjective_token =
  let potential_correction_token_list = seek_word adjective_token in
  let rec aux l res =
    match l with
    | [] -> res
    | h :: t -> let (is_correct, informations) = Checkings.check_noun_adjective noun_token h in
                if is_correct then aux t ((h, informations) :: res)
                else aux t res
  in
  let checked_list = aux potential_correction_token_list [] in
  (min_distance (Token.get_word adjective_token) checked_list, checked_list)

let construct_adjective_tree adjective_token =
  match adjective_token with
    | Token.Token (Word_classe.Adjectif, (adjective, informations)) -> Node (Word_classe.Adjectif, informations, [Leaf adjective])
    | _ -> failwith "Correction.ml - construct_adjective_tree : invalid token"

let construct_multiple_adjective_tree adjective_tree_list adjective_token_list =
  let gender_number_comparison adj_informations_1 adj_informations_2 =
		let formated_adj_informations adj_informations =
			match adj_informations with
			| [rad; g; n] -> [g; n]
			| [g; n] -> [g; n]
			| _ -> failwith ("formated_adj_informations : adj_informations doesn't match the expected format")
		in
		match (formated_adj_informations adj_informations_1), (formated_adj_informations adj_informations_2) with
		| x, y when x = y -> x
		| ["e"; "i"], [g; n] | [g; n], ["e"; "i"] -> [g; n]
		| ["e"; "s"], [g; "s"] | [g; "s"], ["e"; "s"] -> [g; "s"]
		| ["e"; "p"], [g; "p"] | [g; "p"], ["e"; "p"] -> [g; "p"]
		| ["m"; "i"], ["m"; n] | ["m"; n], ["m"; "i"] -> ["m"; n]
		| ["f"; "i"], ["f"; n] | ["f"; n], ["f"; "i"] -> ["f"; n]
		| _ -> failwith "information_comparison : adj_informations_1 and adj_informations_2 doesn't match"
  in
  match adjective_token_list with
  | [] -> Empty
  | [adjective_token] -> construct_adjective_tree adjective_token
  | _ -> begin
          let adj_informations_list = List.map Token.get_information adjective_token_list in
          let adj_list = List.map Token.get_word adjective_token_list in
          Node (Word_classe.MultipleAdj, (List.fold_left gender_number_comparison ["e"; "i"] adj_informations_list), adjective_tree_list)
         end

let correct_adj_noun adj_tree adj_token noun_tree noun_token =
  let (is_correct, informations) = Checkings.check_noun_adjective noun_token adj_token in
  if is_correct then
    (adj_tree, adj_token)
  else
    begin
      print_string "Error between adjective and noun :: Wrong agreement between "; print_string ((Token.get_word adj_token) ^ " and " ^ (Token.get_word noun_token)); print_newline ();
      print_string "Trying to fix..."; print_newline ();
      let ((corrected_token, corrected_informations), other_possibility) = correct_adjective noun_token adj_token in
      print_string "Suggested correction : "; print_string (Token.get_word corrected_token); print_newline ();
      print_string "Other possibilities : "; List.iter (fun (t,_) -> print_string (Token.get_word t); print_string ", ") other_possibility; print_newline (); print_newline ();
      (construct_adjective_tree corrected_token, corrected_token)
      end

let correct_multiple_adj_noun mult_adj_tree mult_adj_token noun_tree noun_token =

  let get_token_from_tree tree =
    match tree with
    | Node (word_classe, information, [Leaf word]) -> Token.Token (word_classe, (word, information))
    | _ -> failwith "Correction :: get_token_from_tree : not a Node with a single Leaf as children"
  in

  let adj_tree_list = get_children mult_adj_tree in
  List.iter print_syntax_tree adj_tree_list;
  let adj_token_list = List.map get_token_from_tree adj_tree_list in
  let corrected_list = List.map2 (fun adj_tree adj_token -> correct_adj_noun adj_tree adj_token noun_tree noun_token) adj_tree_list adj_token_list in
  let corrected_tree_list = List.map fst corrected_list in
  let corrected_token_list = List.map snd corrected_list in
  (construct_multiple_adjective_tree corrected_tree_list corrected_token_list, mult_adj_token)

let correct_nominal_group det_tree det_token adj_tree adj_token noun_tree noun_token adj_tree_2 adj_token_2 =
  let (is_correct, informations, error) = Checkings.check_nominal_group det_token adj_token noun_token adj_token_2 in
  if is_correct then
    match adj_tree, adj_tree_2 with
    | Empty, Empty ->
      begin
        let token = Token.Token ( Word_classe.GN, ((Token.get_word det_token) ^ " " ^ (Token.get_word noun_token), informations) ) in
        [(Node (Word_classe.GN, informations, [det_tree; noun_tree]), token)]
      end
    | Empty, _ ->
      begin
        let token = Token.Token ( Word_classe.GN, ((Token.get_word det_token) ^ " " ^ (Token.get_word noun_token) ^ " " ^ (Token.get_word adj_token_2), informations) ) in
        [(Node (Word_classe.GN, informations, [det_tree; noun_tree; adj_tree_2]), token)]
      end
    | _, Empty ->
      begin
        let token = Token.Token ( Word_classe.GN, ((Token.get_word det_token) ^ " " ^ (Token.get_word adj_token) ^ " " ^ (Token.get_word noun_token), informations) ) in
        [(Node (Word_classe.GN, informations, [det_tree; adj_tree; noun_tree]), token)]
      end
    | _ ->
      begin
        let token = Token.Token ( Word_classe.GN, ((Token.get_word det_token) ^ " " ^ (Token.get_word adj_token) ^ " " ^ (Token.get_word noun_token) ^ " " ^ (Token.get_word adj_token_2), informations) ) in
        [(Node (Word_classe.GN, informations, [det_tree; adj_tree; noun_tree; adj_tree_2]), token)]
      end
  else
    begin
      print_string "Error on nominal group"; print_newline ();
      let (corrected_det_tree, corrected_det_token) = correct_determiner_noun det_tree det_token noun_tree noun_token in
      let (corrected_adj_tree, corrected_adj_token) = 
        match adj_tree with
        | Empty -> (Empty, Token.Token (Word_classe.Adjectif, ("", [])))
        | Node (Word_classe.MultipleAdj, _, _) -> correct_multiple_adj_noun adj_tree adj_token noun_tree noun_token
        | Node (Word_classe.Adjectif, _, _) -> correct_adj_noun adj_tree adj_token noun_tree noun_token
        | _ -> failwith "Correction : correct_nominal_group : invalid adj_tree"
      in
      let (corrected_adj_tree_2, corrected_adj_token_2) = 
        match adj_tree_2 with
        | Empty -> (Empty, Token.Token (Word_classe.Adjectif, ("", [])))
        | Node (Word_classe.MultipleAdj, _, _) -> correct_multiple_adj_noun adj_tree_2 adj_token_2 noun_tree noun_token
        | Node (Word_classe.Adjectif, _, _) -> correct_adj_noun adj_tree_2 adj_token_2 noun_tree noun_token
        | _ -> failwith "Correction : correct_nominal_group : invalid adj_tree_2"
      in
      [(Node (Word_classe.GN, List.tl (Token.get_information noun_token), [corrected_det_tree; corrected_adj_tree; noun_tree; corrected_adj_tree_2]), (Token.Token ( Word_classe.GN, ((Token.get_word corrected_det_token) ^ " " ^ (Token.get_word corrected_adj_token) ^ " " ^ (Token.get_word noun_token) ^ " " ^ (Token.get_word corrected_adj_token_2), List.tl (Token.get_information noun_token) ) )))]
      end

type token = 
	| Token of ( Word_classe.word_classe * (string * string list) )
	| Unknown of string


(**
  [string_of_token t] returns a string representation of the token [t].

  @param t The token to be converted to a string.
  @return The string representation of the token.
*)
let string_of_token t =
  match t with
  | Token (Word_classe.Determinant, (s, l)) ->  "D : " ^ s ^ " | " ^ (Utility.string_of_string_list l)
  | Token (Word_classe.Nom, (s, l)) ->  "N : " ^ s ^ " | " ^ (Utility.string_of_string_list l)
  | Token (Word_classe.Adjectif, (s, l)) ->  "A : " ^ s ^ " | " ^ (Utility.string_of_string_list l)
  | Token (Word_classe.S, (s, l)) ->  "S : " ^ s ^ " | " ^ (Utility.string_of_string_list l)
  | Token (Word_classe.GV, (s, l)) ->  "GV : " ^ s ^ " | " ^ (Utility.string_of_string_list l)
  | Token (Word_classe.GN, (s, l)) ->  "GN : " ^ s ^ " | " ^ (Utility.string_of_string_list l)
  | Token (Word_classe.MultipleAdj, (s, l)) ->  "MA : " ^ s ^ " | " ^ (Utility.string_of_string_list l)
  | Token (Word_classe.Pronom_sujet, (s, l)) ->  "PS : " ^ s ^ " | " ^ (Utility.string_of_string_list l)
  | Token (Word_classe.Sujet, (s, l)) ->  "SU : " ^ s ^ " | " ^ (Utility.string_of_string_list l)
  | Token (Word_classe.Verbe, (s, l)) ->  "V : " ^ s ^ " | " ^ (Utility.string_of_string_list l)
  | Token (Word_classe.Unknown, (s, l)) ->  failwith "string_of_token : Unknown token declared as Token"
  | Unknown w ->  "Unknown " ^ w

(**
  [print_token t] prints the string representation of the token [t].

  @param t The token to be printed.
  @return Unit
*)
let print_token token =
  print_string (string_of_token token)


(**
  [print_token_list l] prints the string representation of the list of tokens [l].

  @param l The list of tokens to be printed.
  @return Unit
*)
let print_token_list =
  List.iter (fun x -> print_token x; print_string " | ")


(**
  [print_token_list_list l] prints the string representation of the list of list of tokens [l].

  @param l The list of list of tokens to be printed.
  @return Unit
*)
let print_token_list_list =
  List.iter (fun x -> print_token_list x; print_newline ())


(**
  [get_word token] returns the word of the token [token].

  @param token The token from which to extract the word.
  @return The word of the token.
*)
let get_word token =
  match token with
  | Token (Word_classe.Determinant, (s, l)) -> s
  | Token (Word_classe.Nom, (s, l)) -> s
  | Token (Word_classe.Adjectif, (s, l)) -> s
  | Token (Word_classe.Sujet, (s, l)) -> s
  | Token (Word_classe.Verbe, (s, l)) -> s
  | Token (Word_classe.Pronom_sujet, (s, l)) -> s
  | Token (Word_classe.S, (s, l)) -> s
  | Token (Word_classe.GV, (s, l)) -> s
  | Token (Word_classe.GN, (s, l)) -> s
  | Token (Word_classe.MultipleAdj, (s, l)) -> s
  | Token (Word_classe.Unknown, (s, l)) -> failwith "get_word : Unknown token declared as Token"
  | Unknown w -> w


(**
  [get_information token] returns the information of the token [token].

  @param token The token from which to extract the information.
  @return The information of the token.
*)
let get_information token =
  match token with
  | Token (Word_classe.Determinant, (s, l)) -> l
  | Token (Word_classe.Nom, (s, l)) -> l
  | Token (Word_classe.Adjectif, (s, l)) -> l
  | Token (Word_classe.Sujet, (s, l)) -> l
  | Token (Word_classe.Verbe, (s, l)) -> l
  | Token (Word_classe.Pronom_sujet, (s, l)) -> l
  | Token (Word_classe.GN, (s, l)) -> l
  | Token (Word_classe.MultipleAdj, (s, l)) -> l
  | Token (Word_classe.S, (s, l)) -> l
  | Token (Word_classe.GV, (s, l)) -> l
  | Token (Word_classe.Unknown, (s, l)) -> failwith "get_information : Unknown token declared as Token"
  | Unknown _ -> failwith "get_information : tring to get a information from a Unknown"

  let distinct token_list =
    List.fold_left (fun acc x -> if List.mem x acc then acc else x :: acc) [] token_list

(** [get_gender token] returns the gender associated with the given [token].
  * The function matches the [token] against different word classes and extracts
  * the gender information from the corresponding list.
  *
  * @param token The token from which to extract the gender.
  * @return The gender of the token.
  * @raise Failure if the token does not belong to a word class that has a gender.
  *
  * The function handles the following word classes:
  * - [Adjectif]: Extracts the second element from the list.
  * - [Nom]: Extracts the second element from the list.
  * - [Determinant]: Extracts the second element from the list.
  * - [Pronom_sujet]: Extracts the third element from the list.
*)
let get_gender token =
  match token with
  | Token (Word_classe.Adjectif, (_, l)) -> List.nth l 1 (* Wanting catching error but no success trying later *)
  | Token (Word_classe.Nom, (_, l)) -> List.nth l 1
  | Token (Word_classe.Determinant, (_, l)) -> List.nth l 1
  | Token (Word_classe.Pronom_sujet, (_, l)) -> List.nth l 2
  | _ -> failwith "get_gender : not something which have a gender"


(** [get_number token] extracts the number from a given token.
  *
  * @param token The token from which to extract the number. The token is expected to be of type [Token] with specific word classes.
  * @return The number associated with the token.
  * @raise Failure if the token does not have a number or if the index is out of bounds.
  *
  * The function matches the token with different word classes and extracts the number from the list associated with each word class.
  * - For [Adjectif], [Nom], and [Determinant], it extracts the third element (index 2) from the list.
  * - For [Pronom_sujet], it extracts the fourth element (index 3) from the list.
  * - If the token does not match any of the specified word classes, it raises a failure with the message "get_number : not something which have a number".
  *)
let get_number token =
  match token with
  | Token (Word_classe.Adjectif, (_, l)) -> List.nth l 2 (* Wanting catching error but no success trying later *)
  | Token (Word_classe.Nom, (_, l)) -> List.nth l 2
  | Token (Word_classe.Determinant, (_, l)) -> List.nth l 2
  | Token (Word_classe.Pronom_sujet, (_, l)) -> List.nth l 3
  | _ -> failwith "get_number : not something which have a number"


(** [get_word_classe token] returns the word class of the given [token].
  The function matches the [token] against various patterns and returns
  the corresponding word class from the [Word_classe] module.

  @param token The token to be analyzed.
  @return The word class of the token.
  @raise Failure if the token is of type [Word_classe.Unknown].

  The possible word classes are:
  - [Word_classe.Determinant]
  - [Word_classe.Nom]
  - [Word_classe.Adjectif]
  - [Word_classe.Sujet]
  - [Word_classe.Verbe]
  - [Word_classe.Pronom_sujet]
  - [Word_classe.S]
  - [Word_classe.GV]
  - [Word_classe.GN]
  - [Word_classe.MultipleAdj]
  - [Word_classe.Unknown] (raises an exception)
  - [Unknown] (returns [Unknown])
*)
let get_word_classe token =
  match token with
  | Token (Word_classe.Determinant, (s, l)) -> Word_classe.Determinant
  | Token (Word_classe.Nom, (s, l)) -> Word_classe.Nom
  | Token (Word_classe.Adjectif, (s, l)) -> Word_classe.Adjectif
  | Token (Word_classe.Sujet, (s, l)) -> Word_classe.Sujet
  | Token (Word_classe.Verbe, (s, l)) -> Word_classe.Verbe
  | Token (Word_classe.Pronom_sujet, (s, l)) -> Word_classe.Pronom_sujet
  | Token (Word_classe.S, (s, l)) -> Word_classe.S
  | Token (Word_classe.GV, (s, l)) -> Word_classe.GV
  | Token (Word_classe.GN, (s, l)) -> Word_classe.GN
  | Token (Word_classe.MultipleAdj, (s, l)) -> Word_classe.MultipleAdj
  | Token (Word_classe.Unknown, (s, l)) -> failwith "get_word_classe : Unknown token declared as Token"
  | Unknown _ -> Unknown

(** [get_verb_person token] extracts the person information from a verb token.
  @param token The token to extract the person information from.
  @return A list of strings representing the person information.
  @raise Failure if the token does not match the expected verb format or if the verb information is not in the correct format.
  The function expects the verb information to be a list of strings with specific elements in a specific order.
  If the verb information ends with "Y", "P", or "Q", an empty list is returned.
  Otherwise, the last element of the verb information list is split by commas and returned as a list of strings.

  Y : infinitif
  P : participe passé
  Q : participe présent

  Sp : subjonctif présent
  Sq : subjonctif imparfait
  Ip : présent simple
  Iq : imparfait
  Is : passé simple
  If : futur simple
  e : impératif
  K : conditionnel
*)
let get_verb_person ( token:token ) :string list =
  match token with
  | Token (Word_classe.Verbe, (verb, verb_informations)) ->
    begin
      match verb_informations with
      | rad_verb :: intransitif :: transitif_direct :: transitif_indirect :: pronominal :: impersonnel :: auxiliaire_etre :: auxiliaire_avoir :: "Y" :: []
      | rad_verb :: intransitif :: transitif_direct :: transitif_indirect :: pronominal :: impersonnel :: auxiliaire_etre :: auxiliaire_avoir :: "P" :: []
        -> []
      | rad_verb :: intransitif :: transitif_direct :: transitif_indirect :: pronominal :: impersonnel :: auxiliaire_etre :: auxiliaire_avoir :: "Q" :: auxiliaire :: gender :: number :: []
        -> []
      | rad_verb :: intransitif :: transitif_direct :: transitif_indirect :: pronominal :: impersonnel :: auxiliaire_etre :: auxiliaire_avoir :: "e" :: person :: []
        -> []
      | rad_verb :: intransitif :: transitif_direct :: transitif_indirect :: pronominal :: impersonnel :: auxiliaire_etre :: auxiliaire_avoir :: "Q" :: auxiliaire :: gender :: number :: person :: []
        -> (String.split_on_char ',' person)
      | rad_verb :: intransitif :: transitif_direct :: transitif_indirect :: pronominal :: impersonnel :: auxiliaire_etre :: auxiliaire_avoir :: temps :: person :: []
        -> (String.split_on_char ',' person)
      | l -> (Utility.print_string_list verb_informations; print_newline (); failwith "token.get_verb_person (1) : Doesn't receive a correct Verb")
    end
  | _ -> failwith "get_verb_person (2) : Doesn't receive a correct Verb"

(** 
  Converts a sentence (string) into a list of words.

  @param s The input sentence as a string.
  @return A list of words extracted from the input sentence.

  The function handles spaces and apostrophes as word delimiters. It uses a recursive helper function `aux` to process the characters of the input string. The helper function `find_word` is used to extract individual words from the character list.

  The function `Utility.list_without_x_last_char` is used to remove processed characters from the list, and `Utility.string_of_char_list` is used to convert a list of characters back into a string.

  Example:
  {[
    let words = sentence_to_list "Hello, world! It's a beautiful day."
    (* words = ["Hello,"; "world!"; "It's"; "a"; "beautiful"; "day."] *)
  ]}
*)
let sentence_to_list s =
	let rec aux s_l l =
		match s_l with
		| [] -> l
		| ' ' :: t -> aux t l
		(* Just handeling ' case which is quite annoying more details on other case*)
		| '\'' :: t ->
			let rec find_word l w length_w =
				match l with
				| [] -> w, length_w
				| ' ' :: t | '\'' :: t -> w, length_w
				| c :: t -> find_word t (c::w) (length_w + 1)
			in
			let w, length_w = find_word (List.tl s_l) [] 0
			in
			aux (Utility.list_without_x_last_char (length_w+1) s_l) ((Utility.string_of_char_list (w @ ['\'']))::l)
		(* Finding a new word ! *)
		| c :: t ->
			(* Getting the whole word *)
			let rec find_word l w length_w =
				match l with
				| [] -> w, length_w
				| ' ' :: t | '\'' :: t -> w, length_w
				| c :: t -> find_word t (c::w) (length_w + 1)
			in
			let w, length_w = find_word s_l [] 0
			in
			(* Reccursive call with adding the whole word to list of word and jumping in the string from a step of word found length *)
			aux (Utility.list_without_x_last_char length_w s_l) ((Utility.string_of_char_list w)::l)
	in
	aux (List.rev (Utility.char_list_of_string s) ) []


let get_token_from_informations word informations =
  match informations with
  | (rad_det::"D"::complementary_information) -> Token ( Word_classe.Determinant, (word, rad_det::complementary_information) )
  | (rad_noun::"N"::complementary_information) -> Token ( Word_classe.Nom, (word, rad_noun::complementary_information) )
  | (rad_adj::"A"::complementary_information) -> Token ( Word_classe.Adjectif, (word, rad_adj::complementary_information) )
  | (rad_verb::"V"::complementary_information) -> Token ( Word_classe.Verbe, (word, rad_verb::complementary_information) )
  | (rad_subj_pronoun::"Os"::complementary_information) -> Token ( Word_classe.Pronom_sujet, (word, rad_subj_pronoun::complementary_information) )
  | _ -> failwith "get_token_from_informations : wrong information"

let get_token_from_informations_list word informations =
  let rec match_information i possibility word =
    match i with
    | [] -> possibility
    | (rad_det::"D"::complementary_information) :: tl -> match_information tl (Token ( Word_classe.Determinant, (word, rad_det::complementary_information) ) :: possibility) word
    | (rad_noun::"N"::complementary_information) :: tl -> match_information tl (Token ( Word_classe.Nom, (word, rad_noun::complementary_information) ) :: possibility) word
    | (rad_adj::"A"::complementary_information) :: tl -> match_information tl (Token ( Word_classe.Adjectif, (word, rad_adj::complementary_information) ) :: possibility) word
    | (rad_verb::"V"::complementary_information) :: tl -> match_information tl (Token ( Word_classe.Verbe, (word, rad_verb::complementary_information) ) :: possibility) word
    | (rad_subj_pronoun::"Os"::complementary_information) :: tl -> match_information tl (Token ( Word_classe.Pronom_sujet, (word, rad_subj_pronoun::complementary_information) ) :: possibility) word
    | _ when possibility = [] -> [Unknown "Unknown"]
    | _ -> failwith "get_token_from_informaations_list :: match_information : wrong information"
  in
  match_information informations [] word

(*Misspelled correction *)

let azerty_keyboard = [|
  (0, 0); (2, 4); (2, 2); (1, 2); (0, 2); (1, 3); (1, 4); (1, 5); (0, 7); (1, 6); (1, 7); (1, 8); (1, 9); (2, 5);
  (0, 8); (0, 9); (1, 0); (0, 3); (1, 1); (0, 4); (0, 6); (2, 3); (2, 0); (2, 1); (0, 5); (0, 1)
|]

let qwerty_keyboard = [|
  (1, 0); (2, 4); (2, 2); (1, 2); (0, 2); (1, 3); (1, 4); (1, 5); (0, 7); (1, 6); (1, 7); (1, 8); (2, 6); (2, 5);
  (0, 8); (0, 9); (0, 0); (0, 3); (1, 1); (0, 4); (0, 6); (2, 3); (0, 1); (2, 1); (0, 5); (2, 0)
|]

let distance_between_key keyboard key_1 key_2 =
  max (abs (fst keyboard.(int_of_char key_1 - 97) - fst keyboard.(int_of_char key_2 - 97))) (abs (snd keyboard.(int_of_char key_1 - 97) - snd keyboard.(int_of_char key_2 - 97)))

let alphabet = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z']

let split l =
  let rec aux l l1 l2 =
    match l with
    | [] -> (l1, l2)
    | h :: t -> aux t (h :: l2) l1
  in
  aux l [] []

let merge l1 l2 =
  let rec aux l1 l2 res =
    match l1, l2 with
    | [], l2 -> res @ l2
    | l1, [] -> res @ l1
    | ((_, distance_1, _) as h1) :: t1, ((_, distance_2, _) as h2) :: t2 when distance_1 < distance_2 -> aux l1 t2 (h2 :: res)
    | h1 :: t1, h2 :: t2 -> aux t1 l2 (h1 :: res)
  in
  aux l1 l2 []

let rec merge_filter_sort l =
  match l with
  | [] -> []
  | [(word, distance, [])] -> let (success, informations) = Trie.trie_search Dictionnary.dictionnary word in
           if success then [(word, distance, get_token_from_informations_list word informations)] else []
  | [x] -> [x]
  | _ -> let (l1, l2) = split l in
         merge (merge_filter_sort l1) (merge_filter_sort l2)

let get_correction_possibility_for_word word =
  let insertion string i = List.map (fun x -> let (fst_part, snd_part) = Utility.split_string string (i+1) in (fst_part ^ (Char.escaped x) ^ snd_part, 2, [])) alphabet in
  let substitution string i = List.map (fun x -> (Utility.replace_char_in_string string i x, distance_between_key azerty_keyboard x (string.[i]), [])) alphabet in
  let deletion string i = (Utility.string_delete_i_char string i, 2, []) in
  let rec aux string i res =
    if i = -1 then 
      res
    else 
      let possible_change = (insertion string i) @ (substitution string i) @ [deletion string i] in
      aux string (i - 1) (possible_change @ res)
  in
  match merge_filter_sort ( (insertion word (-1)) @ aux word (String.length word - 1) [] ) with
  | [] -> []
  | [(word, distance, token_list)] -> token_list
  | (word_1, distance_1, token_list_1) :: (word_2, distance_2, token_list_2) :: [] -> token_list_1 @ token_list_2
  | (word_1, distance_1, token_list_1) :: (word_2, distance_2, token_list_2) :: (word_3, distance_3, token_list_3) :: t -> token_list_1 @ token_list_2 @ token_list_3

(** 
  [sentence_to_token_list s] converts a sentence [s] into a list of token lists.
  
  @param s The input sentence as a string.
  @return A list of token lists representing the sentence.
  
  The function works as follows:
  - It defines a recursive helper function [match_information] that matches the information of each word in the sentence to its corresponding token.
  - It defines another helper function [get_correction_possibility_for_word] that generates correction possibilities for a given word by replacing each character with every letter from 'a' to 'z' and checking if the modified word exists in the dictionary.
  - It defines a recursive function [aux] that processes each word in the sentence, checks if it exists in the dictionary, and either adds its token information to the list or attempts to find correction possibilities if the word is unknown.
  - The function [sentence_to_list] is used to split the input sentence into a list of words.
  - The final result is a reversed list of token lists representing the sentence.
*)
let sentence_to_token_list (s:string) :token list list =
	let rec aux (list_word:string list) (list_token:token list list) =
		match list_word with
		| [] -> list_token
		| word :: t ->
			(* Searching in dictionnary if word exists and its information if it does *)
			let (is_word, information) = Trie.trie_search Dictionnary.dictionnary word in
			if is_word then
				(* Return all possibility for a word *)
        aux t ((get_token_from_informations_list word information) :: list_token)
			else
        begin
          print_newline ();
          print_string ("Not a correct sentence : Unknown word " ^ word ^ "\nTrying to find correction\n");
          let correction_possibilitys = get_correction_possibility_for_word word in
          print_string ("Possible correction found : "); List.iter (fun x -> print_string ((get_word x) ^ " - ")) correction_possibilitys; print_newline (); print_newline ();
          aux t (correction_possibilitys :: list_token)
        end
	in
	List.rev (aux (sentence_to_list s) [])


let has_one_or_zero_noun l =
  let rec aux l has_seen_noun =
    match l with
    | [] -> true
    | Token (Word_classe.Nom, _) :: t when has_seen_noun -> false
    | Token (Word_classe.Nom, _) :: t -> aux t true
    | _ :: t -> aux t has_seen_noun
  in
  aux l false

(** 
  [all_possibility l] generates all possible combinations of tokens from a list of lists of tokens.
  
  @param l A list of lists of tokens, where each inner list represents possible tokens for a word.
  @return A list of lists of tokens, where each inner list represents a possible combination of tokens.
  
  The function works in two main steps:
  1. [first_floor_course]: Iterates over the outer list of token lists.
  2. [second_floor_course]: Iterates over each inner list of tokens and combines them with the accumulated results.
  
  The final result is a list of all possible combinations of tokens, with each combination being a list of tokens.
*)
let all_possibility (l:token list list) :token list list =
	(* Firstly itterating over the token list of list of possibility for words *)
	let rec first_floor_course (l:token list list) (acc: token list list) :token list list =
		match l with
		| [] -> acc
		| [h] :: t -> first_floor_course t (List.map (fun x -> h :: x) acc)
		| h :: t ->
			(* Secondly itterating over the possibilitys of words *)
			let rec second_floor_course (l:token list) :token list list =
				match l with
				| [] -> []
				(* For each possibility, adding this possibility to all sentence possible already found *)
				| hd :: tl -> List.map (fun x -> hd :: x) acc @ second_floor_course tl
			in
			first_floor_course t (second_floor_course h)
	in
  List.filter has_one_or_zero_noun ( List.map (List.rev) (first_floor_course l [[]]) )
(* ------------------------------------------------------------------ *)
(*     Hearth of project : Detecting errors and making correction     *)
(* ------------------------------------------------------------------ *)

(* TODO :: Implementing correction of agreement inside nominal group and between verb and nominal group *)

type error =
	| Empty_sentence
	| Unknown_word of string (* Word unknown *)
	| Missing of (Word_classe.word_classe * string) (* Word_class missing * word got *)
	| Accord of (Word_classe.word_classe * string) * (Word_classe.word_classe * string)
	| Conjuguaison of (Word_classe.word_classe * string * (string list) ) * (string * string list) (* (Word classe of subject * subject * informations of subject) * (Verb * informations of verb) *)
	| No

let string_of_error e =
	match e with
	| Empty_sentence -> "Empty sentence"
	| Unknown_word s -> "Unknown word : " ^ s
	| Missing (wc, w) -> "Missing " ^ (Word_classe.word_classe_to_string wc) ^ " got : " ^ w
	| Accord ((wc1, w1), (wc2, w2)) -> "Accord between " ^ (Word_classe.word_classe_to_string wc1) ^ " and " ^ (Word_classe.word_classe_to_string wc2) ^ " : " ^ w1 ^ " and " ^ w2
	| Conjuguaison ((w1, str, info), (verb, info_verb)) ->
    "Conjugaison of " ^ verb ^ " with " ^ Word_classe.word_classe_to_string w1 ^ " : " ^ str ^ 
    "\nVerb person is : " ^ Utility.string_of_string_list (Token.get_verb_person (Token.Token (Word_classe.Verbe, (verb, info_verb))) ) ^
    "\nPerson should match the gender and number of the subject : " ^ Utility.string_of_string_list info
	| No -> "No"

type syntax_tree =
	| Node of Word_classe.word_classe * string list * syntax_tree list
	| Leaf of string
	| Error of error
	| Empty


(** 
  Recursively generates LaTeX code for a syntax tree and writes it to a file.

  @param file The output file where the LaTeX code will be written.
  @param tree The syntax tree to be converted into LaTeX code.

  The function handles different types of nodes in the syntax tree:
  - [Empty]: Does nothing.
  - [Leaf s]: Writes a LaTeX node with the string [s].
  - [Error e]: Writes a LaTeX node with the error string [e].
  - [Node (wc, [], l)]: Writes a LaTeX node with the word class [wc] and recursively processes the list of children [l].
  - [Node (wc, i, l)]: Writes a LaTeX node with the word class [wc] and additional information [i], then recursively processes the list of children [l].
*)
let rec syntax_tree_in_tex_aux file tree =
	match tree with
	| Empty -> ()
	| Leaf s -> Printf.fprintf file "child {node {%s}}" s
	| Error e -> Printf.fprintf file "child {node {%s}}" (string_of_error e)
	| Node (wc, [], l) ->
		Printf.fprintf file "child { node {%s} " (Word_classe.word_classe_to_string wc);
		List.iter (fun x -> syntax_tree_in_tex_aux file x) l;
		Printf.fprintf file "}"
	| Node (wc, i, l) ->
		Printf.fprintf file "child { node {%s \\\\ %s} " (Word_classe.word_classe_to_string wc) (Utility.string_of_string_list i);
		List.iter (fun x -> syntax_tree_in_tex_aux file x) l;
		Printf.fprintf file "}"


(** [syntax_tree_in_tex tree file] generates a LaTeX representation of the given syntax tree [tree]
  and writes it to the specified output [file]. The function uses the TikZ package to create a 
  graphical representation of the syntax tree, with nodes and edges formatted for better readability.

  @param tree The syntax tree to be converted into LaTeX format.
  @param file The output channel where the LaTeX code will be written.
*)
let syntax_tree_in_tex tree file =
	Printf.fprintf file "\\begin{center}\n\\begin{tikzpicture}\n\\node{Sentence}[sibling distance = 4cm, level distance = 3cm, align=center]\n";
	syntax_tree_in_tex_aux file tree;
	Printf.fprintf file ";\n\\end{tikzpicture}\n\\end{center}\n"


(** [syntax_tree_list_in_tex tree_list] generates a LaTeX representation of a list of syntax trees.
  It changes the current working directory to "results", appends the LaTeX representation of each
  syntax tree in [tree_list] to the file "result.tex", and then changes the working directory back.

  @param tree_list The list of syntax trees to be converted to LaTeX and appended to "result.tex".
*)
let syntax_tree_list_in_tex tree_list =
	Sys.chdir "results";
  let file = open_out_gen [Open_append] 0 "result.tex" in
	List.iter (fun x -> syntax_tree_in_tex x file) tree_list;
  close_out file;
	Sys.chdir ".."


(** 
  Checks if the given tree is a success tree.

  @param tree The tree to check.
  @return [true] if the tree is a success tree, [false] otherwise.
*)
let is_a_success_tree tree =
	match tree with
	| Empty | Error _ -> false
	| _ -> true


(** [get_results tree_list] filters the given [tree_list] to only include 
  successful syntax trees and prints the results in TeX format. 
  If no successful trees are found, it prints "Not a correct sentence". 
  Otherwise, it prints "Correct sentence".

  @param tree_list The list of syntax trees to be processed.
*)
let get_results tree_list =
	let filtered_list = List.filter is_a_success_tree tree_list in
	match filtered_list with
	| [] -> syntax_tree_list_in_tex tree_list; print_string "Not a correct sentence\n"
	| h :: t -> syntax_tree_list_in_tex filtered_list; print_string "Correct sentence\n"
	


(* Represente the sentence which will be itterated over with indice being the index of where is the correction actually *)
type item =
	{ 
	t_a : Token.token array;
	mutable indice : int;
	length : int;
	}

(** 
  Checks the gender compatibility between two given genders.

  @param g1 The first gender to check.
  @param g2 The second gender to check.
  @return A tuple where the first element is a boolean indicating if the genders are compatible,
          and the second element is the resulting gender if they are compatible, or an empty string if not.
          
  The function considers the following rules for compatibility:
  - If both genders are the same, they are compatible and the resulting gender is the same as the input genders.
  - If one gender is "m" (male) and the other is "e" (either), they are compatible and the resulting gender is "m".
  - If one gender is "f" (female) and the other is "e" (either), they are compatible and the resulting gender is "f".
  - Any other combination is considered incompatible.
*)
let check_gender g1 g2 =
	match g1, g2 with
	| x, y when x = y -> true, x
	| "m", "e" | "e", "m" -> true, "m"
	| "f", "e" | "e", "f" -> true, "f"
	| _ -> false, ""

(** 
  Checks the relationship between two numbers represented as strings.

  @param n1 The first number as a string.
  @param n2 The second number as a string.
  @return A tuple where the first element is a boolean indicating if the numbers have a valid relationship,
          and the second element is the resulting string based on the relationship.
          
  The function returns:
  - (true, x) if both numbers are equal.
  - (true, "s") if one number is "s" and the other is "i".
  - (true, "p") if one number is "p" and the other is "i".
  - (false, "") if none of the above conditions are met.
*)
let check_number n1 n2 =
	match n1, n2 with
	| x, y when x = y -> true, x
	| "s", "i" | "i", "s" -> true, "s"
	| "p", "i" | "i", "p" -> true, "p"
	| _ -> false, ""


(** 
  [check_gender_number informations_1 informations_2] checks if the gender and number information 
  in [informations_1] and [informations_2] match. 

  @param informations_1 A list of strings representing gender and number information.
  @param informations_2 A list of strings representing gender and number information.
  @return A tuple where the first element is a boolean indicating if both gender and number match, 
          and the second element is a list containing the resulting gender and number if they match, 
          or an empty list if they do not match.
*)
let check_gender_number (informations_1:string list) (informations_2:string list) :(bool * string list) =
	match informations_1, informations_2 with
	| [], [] -> true, []
	| gender_1 :: number_1 :: [], gender_2 :: number_2 :: [] ->
		let (same_gender, result_gender) = check_gender gender_1 gender_2 in
		let (same_number, result_number) = check_number number_1 number_2 in
		if same_gender && same_number then
			(true, result_gender :: [result_number])
		else
			(false, [])
	| _ -> (false, [])


(** 
  [check_det_noun det_token noun_token] checks the agreement between a determinant and a noun.
  
  @param det_token The token representing the determinant.
  @param noun_token The token representing the noun.
  
  @return A tuple where the first element is a boolean indicating whether the determinant and noun agree in gender and number, 
          and the second element is a list of resulting information if they agree, or an empty list if they do not.
  
  @raise Failure if the provided tokens are not a correct Determinant and a Nom.
*)
let check_det_noun det_token noun_token =
	match det_token, noun_token with
	| Token.Token (Word_classe.Determinant, (det, rad_det::det_informations)), Token.Token (Word_classe.Nom, (noun, rad_noun::noun_informations)) 
		->
			begin
				let (success, result_informations) = check_gender_number det_informations noun_informations in
				if success then
					(true, result_informations)
				else
					(false, [])
			end
	| _, _ -> failwith "check_det_noun : Doesn't receive a correct Determinant and a Nom"

(** 
  [check_adjectives adj_token_1 adj_token_2] checks if two adjective tokens are compatible in terms of gender and number.
  
  @param adj_token_1 The first adjective token to be checked.
  @param adj_token_2 The second adjective token to be checked.
  
  @return A tuple where the first element is a boolean indicating whether the adjectives are compatible, 
          and the second element is a list of adjective information if they are compatible, or an empty list if they are not.
  
  @raise Failure if either of the tokens is not an adjective.
*)
let check_adjectives adj_token_1 adj_token_2 =
	match adj_token_1, adj_token_2 with
	| Token.Token (Word_classe.Adjectif, (adj1, rad_adj_1::adj_informations_1)), Token.Token (Word_classe.Adjectif, (adj2, rad_adj_2::adj_informations_2))
		->
			begin
				match adj_informations_1, adj_informations_2 with
				| _, [] -> true, adj_informations_1
				| [], _ -> true, adj_informations_2
				| _ ->
					let (success, result_informations) = check_gender_number adj_informations_1 adj_informations_2 in
					if success then
						(true, result_informations)
					else
						(false, [])
			end
	| _, _ -> failwith "check_adjectives : Doesn't receive a correct Adjectif"

(** 
  [check_noun_adjective noun_token adj_token] checks if the given noun and adjective tokens agree in gender and number.
  
  @param noun_token The token representing the noun, expected to be of type [Token.Token (Word_classe.Nom, ...)].
  @param adj_token The token representing the adjective, expected to be of type [Token.Token (Word_classe.Adjectif, ...)] or [Token.Token (Word_classe.MultipleAdj, ...)].
  
  @return A tuple [(bool, informations)] where:
    - [bool] indicates whether the noun and adjective agree in gender and number.
    - [informations] is a list of resulting information if the agreement check is successful, otherwise an empty list.
  
  @raise Failure if the provided tokens are not of the correct types (i.e., not a noun and an adjective).
*)
let check_noun_adjective noun_token adj_token =
	match noun_token, adj_token with
	| Token.Token (Word_classe.Nom, (noun, rad_noun::noun_informations)), Token.Token (Word_classe.Adjectif, (adj, rad_adj::adj_informations))
	| Token.Token (Word_classe.Nom, (noun, rad_noun::noun_informations)), Token.Token (Word_classe.MultipleAdj, (adj, ((rad_adj::_) as adj_informations) ))
		->
			begin
				match noun_informations, adj_informations with
				| _, [] -> (true, noun_informations)
				| _ ->
					let (success, result_informations) = check_gender_number noun_informations adj_informations in
					if success then
						(true, result_informations)
					else
						(false, [])
			end
	| _, _ -> failwith "check_noun_adjective : Doesn't receive a correct Nom and a Adjectif"

(** 
  [check_subject_verb subject_token verb_token] checks if the subject and verb tokens agree in person and number.
  
  @param subject_token The token representing the subject, expected to be of type [Token.Token (Word_classe.Sujet, (subject, subject_informations))].
  @param verb_token The token representing the verb, expected to be of type [Token.Token (Word_classe.Verbe, (verb, rad_verb::verb_informations))].
  
  @return A tuple [(bool, subject_informations)] where the boolean indicates if the subject and verb agree, and [subject_informations] contains details about the subject.
  
  @raise Failure if the tokens do not match the expected patterns for a subject and a verb, or if the verb does not have the correct person and number information.
  
  The function works by extracting the person and number information from the subject and verb tokens, and then recursively checking if they match.
  
  Example:
  - If the subject is first person singular ("O1", "s") and the verb is also first person singular ("1s"), the function returns (true, subject_informations).
  - If the subject is third person plural ("O3", "p") and the verb is third person plural ("3p"), the function returns (true, subject_informations).
  - If there is no match, the function returns (false, []).
*)
let check_subject_verb subject_token verb_token =
	match subject_token, verb_token with
	| Token.Token (Word_classe.Sujet, (subject, subject_informations)), Token.Token (Word_classe.Verbe, (verb, rad_verb::verb_informations))
		->
			begin
				let list_verb_person = verb_token |> Token.get_verb_person in
				match subject_informations, list_verb_person with
				| rad_subject :: person :: _ :: number :: [], h :: t ->
					begin
					let rec check_all_person_verb list_verb_person =
						match person, number, list_verb_person with
						| "O1", "s", ("1s"::t) -> (true, subject_informations)
						| "O1", "p", ("1p"::t) -> (true, subject_informations)
						| "O2", "s", ("2s"::t) -> (true, subject_informations)
						| "O2", "p", ("2p"::t) -> (true, subject_informations)
						| "O3", "s", ("3s"::t) -> (true, subject_informations)
						| "O3", "p", ("3p"::t) -> (true, subject_informations)
						| _, _, (h::t) -> check_all_person_verb t
						| _ -> (false, [])
					in
					check_all_person_verb list_verb_person
					end
				| gender :: number :: [], h :: t ->
					begin
						let rec check_all_person_verb list_verb_person =
							match number, list_verb_person with
							| "s", ("3s"::t) -> (true, subject_informations)
							| "p", ("3p"::t) -> (true, subject_informations)
							| _, (h::t) -> check_all_person_verb t
							| _ -> (false, [])
						in
						check_all_person_verb list_verb_person
					end
				| rad_subject :: person :: _ :: number :: [], _ -> failwith "check_subject_verb : Doesn't receive a correct Verb"
				| person :: number :: [], _ -> failwith "check_subject_verb : Doesn't receive a correct Verb"
				| _ -> failwith "check_subject_verb : Doesn't receive a correct Subject"
			end
	| _, _ -> failwith "check_subject_verb : Doesn't receive a correct Sujet and a Verbe"

(** 
  Checks if a verb is conjugated based on its information.

  @param verb_informations A list of strings representing the verb information. 
  The list should contain the following elements in order:
  - rad_verb: The root of the verb.
  - intransitif: Indicates if the verb is intransitive.
  - transitif_direct: Indicates if the verb is directly transitive.
  - transitif_indirect: Indicates if the verb is indirectly transitive.
  - pronominal: Indicates if the verb is pronominal.
  - impersonnel: Indicates if the verb is impersonal.
  - auxiliaire_etre: Indicates if the verb uses "être" as an auxiliary.
  - auxiliaire_avoir: Indicates if the verb uses "avoir" as an auxiliary.
  - A string indicating the conjugation status or tense/person information.

  @return true if the verb is conjugated, false otherwise.

  @raise Failure if the input list does not match the expected format.
*)
let is_conjugue verb_informations =
  match verb_informations with
		| rad_verb :: intransitif :: transitif_direct :: transitif_indirect :: pronominal :: impersonnel :: auxiliaire_etre :: auxiliaire_avoir :: "Y" :: []
		| rad_verb :: intransitif :: transitif_direct :: transitif_indirect :: pronominal :: impersonnel :: auxiliaire_etre :: auxiliaire_avoir :: "P" :: []
		| rad_verb :: intransitif :: transitif_direct :: transitif_indirect :: pronominal :: impersonnel :: auxiliaire_etre :: auxiliaire_avoir :: "Q" :: _
			-> false
		| rad_verb :: intransitif :: transitif_direct :: transitif_indirect :: pronominal :: impersonnel :: auxiliaire_etre :: auxiliaire_avoir :: temps :: person :: []
			-> true
		| _ -> failwith "is_conjugue : Doesn't receive a correct Verb"

(** 
  [check_verbal_group subject_token verb_token] checks the agreement between a subject and a verb.
  
  @param subject_token The token representing the subject, expected to be of type [Token.Token (Word_classe.Sujet, (subject, informations_subject))].
  @param verb_token The token representing the verb, expected to be of type [Token.Token (Word_classe.Verbe, (verb, informations_verb))].
  
  @return A tuple containing:
    - A boolean indicating whether the subject and verb agree.
    - A list of result informations if the agreement check is successful.
    - An error type [No] if the agreement check is successful, or [Conjuguaison] with relevant details if it fails.
  
  @raise Failure if the input tokens do not match the expected subject and verb types, or if the subject informations do not match the expected patterns in [get_person].
*)
let check_verbal_group subject_token verb_token =
	let get_person informations =
		match informations with
		| g :: n :: [] -> ["O3"; g; n]
    | rad_subject :: "O1" :: g :: n :: [] -> ["O1"; g; n]
		| _ -> failwith "get_person : informations doesn't match a gn or a pronuoun subjet"
	in
	match subject_token, verb_token with
	| Token.Token (Word_classe.Sujet, (subject, informations_subject)), Token.Token (Word_classe.Verbe, (verb, informations_verb))
		->
			begin
				if is_conjugue informations_verb then
					let (success, result_informations) = check_subject_verb subject_token verb_token in
					if success then
						(true, result_informations, No)
					else
						(false, [], Conjuguaison ((Word_classe.Sujet, subject, get_person informations_subject), (verb, informations_verb)))
				else
					(false, [], Conjuguaison ((Word_classe.Sujet, subject, get_person informations_subject), (verb, informations_verb)))
			end
	| _, _ -> failwith "check_verbal_group : Doesn't receive a correct Sujet and a Verbe"
	
(** 
  [check_nominal_group det_token adj_token noun_token adj_token_2] checks the agreement between a determinant, 
  one or two adjectives, and a noun. The function takes four tokens as input: a determinant token, an adjective token, 
  a noun token, and a second adjective token. It returns a tuple containing a boolean indicating success, 
  a list of result informations, and an optional error type.

  The function matches the input tokens against several patterns to ensure they are of the correct word classes 
  (Determinant, Adjectif, Nom, MultipleAdj). It then performs agreement checks between the determinant and the noun, 
  the first adjective and the noun, and the second adjective and the noun, if applicable.

  @param det_token The determinant token to be checked.
  @param adj_token The first adjective token to be checked.
  @param noun_token The noun token to be checked.
  @param adj_token_2 The second adjective token to be checked.

  @return A tuple containing:
    - A boolean indicating whether the agreement checks were successful.
    - A list of result informations from the agreement checks.
    - An optional error type indicating the type of agreement error, if any.

  @raise Failure if the input tokens do not match the expected patterns.
*)
let check_nominal_group det_token adj_token noun_token adj_token_2 =
	match det_token, adj_token, noun_token, adj_token_2 with
	| Token.Token (Word_classe.Determinant, (det, rad_det::det_informations)),
	 	Token.Token (Word_classe.Adjectif, (adj, rad_adj::adj_informations)),
		Token.Token (Word_classe.Nom, (noun, rad_noun::noun_informations)),
		Token.Token (Word_classe.Adjectif, (adj_2, rad_adj2::adj_informations_2))

	| Token.Token (Word_classe.Determinant, (det, rad_det::det_informations)),
		Token.Token (Word_classe.MultipleAdj, (adj, ((rad_adj::_) as adj_informations))),
		Token.Token (Word_classe.Nom, (noun, rad_noun::noun_informations)),
		Token.Token (Word_classe.Adjectif, (adj_2, rad_adj2::adj_informations_2))

	| Token.Token (Word_classe.Determinant, (det, rad_det::det_informations)),
		Token.Token (Word_classe.Adjectif, (adj, rad_adj::adj_informations)),
		Token.Token (Word_classe.Nom, (noun, rad_noun::noun_informations)),
		Token.Token (Word_classe.MultipleAdj, (adj_2, ((rad_adj2::_) as adj_informations_2)))

	| Token.Token (Word_classe.Determinant, (det, rad_det::det_informations)),
		Token.Token (Word_classe.MultipleAdj, (adj, ((rad_adj::_) as adj_informations))),
		Token.Token (Word_classe.Nom, (noun, rad_noun::noun_informations)),
		Token.Token (Word_classe.MultipleAdj, (adj_2, ((rad_adj2::_) as adj_informations_2)))
		->
			begin
				let (success_det_noun, result_informations_det_noun) = check_det_noun det_token noun_token in
				if success_det_noun then
					let (success_adj1_noun, result_informations_adj1_noun) = check_noun_adjective noun_token adj_token in
					if success_adj1_noun then
						let (success_adj2_noun, result_informations_adj2_noun) = check_noun_adjective noun_token adj_token_2 in
						if success_adj2_noun then
							(true, result_informations_det_noun, No)
						else
							(false, [], Accord ((Word_classe.Adjectif, adj_2), (Word_classe.Nom, noun)))
					else
						(false, [], Accord ((Word_classe.Adjectif, adj), (Word_classe.Nom, noun)))
				else
					(false, [], Accord ((Word_classe.Determinant, det), (Word_classe.Nom, noun)))
			end
	| Token.Token (Word_classe.Determinant, (det, rad_det::det_informations)),
		Token.Token (Word_classe.Adjectif, ("", [])),
		Token.Token (Word_classe.Nom, (noun, rad_noun::noun_informations)),
		Token.Token (Word_classe.Adjectif, ("", []))
		->
			begin
				let (success_det_noun, result_informations_det_noun) = check_det_noun det_token noun_token in
				if success_det_noun then
					(true, result_informations_det_noun, No)
				else
					(false, [], Accord ((Word_classe.Determinant, det), (Word_classe.Nom, noun)))
			end
	| Token.Token (Word_classe.Determinant, (det, rad_det::det_informations)),
		Token.Token (Word_classe.Adjectif, (adj, rad_adj::adj_informations)),
		Token.Token (Word_classe.Nom, (noun, rad_noun::noun_informations)),
		Token.Token (Word_classe.Adjectif, ("", []))

	| Token.Token (Word_classe.Determinant, (det, rad_det::det_informations)),
		Token.Token (Word_classe.MultipleAdj, (adj, ((rad_adj::_) as adj_informations))),
		Token.Token (Word_classe.Nom, (noun, rad_noun::noun_informations)),
		Token.Token (Word_classe.Adjectif, ("", []))
		->
			begin
				let (success_det_noun, result_informations_det_noun) = check_det_noun det_token noun_token in
				if success_det_noun then
					let (success_adj_noun, result_informations_adj_noun) = check_noun_adjective noun_token adj_token in
					if success_adj_noun then
						(true, result_informations_det_noun, No)
					else
						(false, [], Accord ((Word_classe.Adjectif, adj), (Word_classe.Nom, noun)))
				else
					(false, [], Accord ((Word_classe.Determinant, det), (Word_classe.Nom, noun)))
			end
	| Token.Token (Word_classe.Determinant, (det, rad_det::det_informations)),
		Token.Token (Word_classe.Adjectif, ("", [])),
		Token.Token (Word_classe.Nom, (noun, rad_noun::noun_informations)),
		Token.Token (Word_classe.Adjectif, (adj_2, rad_adj2::adj_informations_2))
		
	| Token.Token (Word_classe.Determinant, (det, rad_det::det_informations)),
		Token.Token (Word_classe.Adjectif, ("", [])),
		Token.Token (Word_classe.Nom, (noun, rad_noun::noun_informations)),
		Token.Token (Word_classe.MultipleAdj, (adj_2, ((rad_adj2::_) as adj_informations_2)))
		->
			begin
				let (success_det_noun, result_informations_det_noun) = check_det_noun det_token noun_token in
				if success_det_noun then
					let (success_adj_noun, result_informations_adj_noun) = check_noun_adjective noun_token adj_token_2 in
					if success_adj_noun then
						(true, result_informations_det_noun, No)
					else
						(false, [], Accord ((Word_classe.Adjectif, adj_2), (Word_classe.Nom, noun)))
				else
					(false, [], Accord ((Word_classe.Determinant, det), (Word_classe.Nom, noun)))
			end
	| _, _, _, _ -> failwith "check_nominal_group : Doesn't receive a correct Determinant, Adjectif, Nom and Adjectif"

(** [pass sentence] increments the [indice] field of the given [sentence] by 1.

  @param sentence The record whose [indice] field will be incremented.
*)
let pass sentence =
	sentence.indice <- sentence.indice + 1


(** [get_syntax_tree sentence] returns the syntax tree of the given [sentence].
  If the sentence index is greater than its length, it returns an [Error Empty_sentence].
  Otherwise, it extracts the verbal group tree and token from the sentence and returns the verbal group tree.

  @param sentence The sentence to analyze.
  @return A syntax tree list of possible correction of the sentence or the syntax tree of the sentence if it's correct
*)
let rec get_syntax_tree sentence :syntax_tree list =
	if sentence.indice > sentence.length then
		[Error Empty_sentence]
	else
		let (verbal_group_tree, verbal_group_token) = get_verbal_group sentence in
		verbal_group_tree

(** [get_verbal_group sentence] parses a sentence to extract the verbal group.
  *
  * This function first attempts to extract the subject from the sentence using [get_subject].
  * If the subject extraction fails, it returns an error along with the subject token.
  * If the subject extraction is successful, it proceeds to extract the verb using [get_verb].
  * If the verb extraction fails, it returns an error along with the verb token.
  * If both the subject and verb extractions are successful, it checks the validity of the verbal group
  * using [check_verbal_group].
  * 
  * If the verbal group is valid, it returns a tuple containing:
  * - A node representing the verbal group with the subject and verb trees as children.
  * - A token representing the verbal group.
  * 
  * If the verbal group is invalid, it returns an error along with the token.
  *
  * @param sentence The sentence to parse.
  * @return A tuple containing either:
  * - A node representing the verbal group and a token if successful.
  * - An error and a token if unsuccessful.
  *)
and get_verbal_group sentence =
  let construct_verbal_group subject_list verb_list =
    let combination = Utility.join_2_lists subject_list verb_list in
    let rec aux l result =
      match l with
      | ((Error e, token), _)::t | (_, (Error e, token))::t -> aux t (([Error e], token)::result)
      | ((subject_tree, subject_token), (verb_tree, verb_token))::t 
        ->  let (success, informations, error) = check_verbal_group subject_token verb_token in 
            let token = Token.Token ( Word_classe.GV, ((Token.get_word subject_token) ^ " " ^ (Token.get_word verb_token), informations) ) in
            if success then
              aux t (([Node (Word_classe.GV, informations, [subject_tree; verb_tree])], token)::result)
            else
              aux t (([Error error], token)::result)
      | _ -> failwith "get_verbal_group : wrong list"
    in
    aux combination []
  in
	let subject_list = get_subject sentence in
	let verb_list = get_verb sentence in
  construct_verbal_group subject_list verb_list

(** [get_subject s] extracts the subject from the sentence represented by [s].
  It returns a tuple containing a parse tree and a token.

  @param s The sentence structure containing the tokens and other information.
  @return A tuple where the first element is either:
      - [Node (Word_classe.Sujet, informations, children)] if a subject is successfully parsed.
      - [Error e] if an error occurs during parsing.
      The second element of the tuple is a token representing the subject.

  The function handles different cases based on the word class of the current token:
  - If the token is a determinant, noun, or adjective, it attempts to parse a nominal group.
  - If the token is a subject pronoun, it attempts to parse a subject pronoun.
  - If the token does not match any expected word class, it returns an error indicating a missing subject.

  @raise Failure if the parse tree does not match the expected structure.
*)
and get_subject s =
	if s.indice >= s.length then
		[(Error Empty_sentence, Token.Token (Word_classe.Sujet, ("", [])))]
	else
		begin
		let wc_of_token = Token.get_word_classe s.t_a.(s.indice) in
    match wc_of_token with
    | Word_classe.Determinant | Word_classe.Nom | Word_classe.Adjectif
      ->
      begin
        let nominal_group_list = get_nominal_group s in
        List.map (fun (nominal_group_tree, nominal_group_token) -> 
          match nominal_group_tree with
          | Error e -> (Error e, nominal_group_token)
          | Node (Word_classe.GN, informations, _) -> (Node (Word_classe.Sujet, informations, [nominal_group_tree]), Token.Token (Word_classe.Sujet, (Token.get_word nominal_group_token, Token.get_information nominal_group_token)))
          | _ -> failwith "get_subject : wrong tree, waiting for a GN"
        ) nominal_group_list
      end
		| Word_classe.Pronom_sujet
      ->
      begin
        let pronoun_subject_list = get_pronom_sujet s in
        List.map (fun (pronoun_subject_tree, pronoun_subject_token) ->
          match pronoun_subject_tree with
          | Error e -> (Error e, pronoun_subject_token)
          | Node (Word_classe.Sujet, informations, _) -> (Node (Word_classe.Sujet, informations, [pronoun_subject_tree]), Token.Token (Word_classe.Sujet, (Token.get_word pronoun_subject_token, Token.get_information pronoun_subject_token)))
          | _ -> failwith "get_subject : wrong tree, waiting for a Pronom_sujet"
        ) pronoun_subject_list
      end
		| _ -> [Error (Missing (Word_classe.Sujet, Token.get_word s.t_a.(s.indice))), (Token.Token (Word_classe.Sujet, (Token.get_word s.t_a.(s.indice), []))) ]
		end

(** 
  Parses a nominal group from the input string `s`.

  This function attempts to parse a nominal group by sequentially parsing its components:
  determinant, adjectives, and noun. It then checks the validity of the parsed nominal group.

  @param s The input string to parse.
  @return A tuple containing:
    - A parse tree representing the nominal group or an error.
    - A token representing the nominal group or an error token.

  The function follows these steps:
  1. Parse the determinant using `get_determinant`.
  2. Parse the adjectives using `get_adjectifs`.
  3. Parse the noun using `get_nom`.
  4. Parse additional adjectives using `get_adjectifs`.
  5. Check the validity of the nominal group using `check_nominal_group`.
  6. Construct the final parse tree and token based on the results.

  If any parsing step fails, the function returns an error.
*)
and get_nominal_group s =
  let combination = Utility.combine_4_lists (get_determinant s) (get_adjectifs s) (get_nom s) (get_adjectifs s) in
    

	let det_list = get_determinant s in
	let adjective1_list = get_adjectifs s in
	let noun_list = get_nom s in
	let adjective2_list = get_adjectifs s in
  let (success, informations, error) = check_nominal_group det_token adj_token noun_token adj_token_2 in
  let token = Token.Token (Word_classe.GN, (Token.get_word det_token ^ " " ^ Token.get_word adj_token ^ " " ^ Token.get_word noun_token ^ " " ^ Token.get_word adj_token_2, informations)) in
  if success then
    match adj_tree, adj_tree_2 with
    | Node (Word_classe.Adjectif, [], [Leaf ""]), Node (Word_classe.Adjectif, [], [Leaf ""]) -> (Node (Word_classe.GN, informations, [det_tree; noun_tree]), token)
    | Node (Word_classe.Adjectif, [], [Leaf ""]), _ -> (Node (Word_classe.GN, informations, [det_tree; noun_tree; adj_tree_2]), token)
    | _, Node (Word_classe.Adjectif, [], [Leaf ""]) -> (Node (Word_classe.GN, informations, [det_tree; adj_tree; noun_tree]), token)
    | _ -> (Node (Word_classe.GN, informations, [det_tree; adj_tree; noun_tree; adj_tree_2]), token)
  else
    (Error error, token)

(** [get_determinant s] is a function that processes a sentence structure [s] to extract a determinant token.
  It returns a tuple containing an error or a node and the token itself.
  
  - If the sentence index [s.indice] is greater than or equal to the sentence length [s.length], 
    it returns an error [Empty_sentence] and a default determinant token.
  - If the current token is unknown, it returns an error [Unknown_word w] and the token.
  - If the current token is a determinant, it advances the sentence index and returns a node with the determinant information and the token.
  - If the current token is not a determinant, it returns an error [Missing (Word_classe.Determinant, word)] and the token.
  
  @param s The sentence structure to be processed.
  @return A tuple containing either an error or a node and the token.
*)
and get_determinant s =
	if s.indice >= s.length then
		(Error Empty_sentence, Token.Token (Word_classe.Determinant, ("", [])))
	else
		begin
		let token = s.t_a.(s.indice) in
		match token with
		| Unknown w -> (Error ( Unknown_word w ), token)
		| Token (Word_classe.Determinant, (det, informations)) -> pass s; (Node (Word_classe.Determinant, informations, [Leaf det]), token)
		| Token (_, (word, _)) -> (Error (Missing (Word_classe.Determinant, word)), token)
		end

(** [get_nom s] is a function that processes a sentence [s] to extract a noun (Nom).
  It returns a tuple containing a result and a token.
  
  - If the current index of the sentence [s] is greater than or equal to the length of the sentence,
    it returns an error indicating an empty sentence and a default token.
  - Otherwise, it retrieves the current token from the sentence.
  
  The function then matches the token:
  - If the token is [Unknown w], it returns an error indicating an unknown word [w] and the token.
  - If the token is a noun ([Token (Word_classe.Nom, (nom, informations))]), it advances the sentence index,
    and returns a node containing the noun information and the token.
  - If the token is of any other type, it returns an error indicating a missing noun and the token.
  
  @param s The sentence to process.
  @return A tuple containing a result and a token.
*)
and get_nom s =
	if s.indice >= s.length then
		(Error Empty_sentence, Token.Token (Word_classe.Nom, ("", [])))
	else
		begin
		let token = s.t_a.(s.indice) in
		match token with
		| Unknown w -> (Error ( Unknown_word w ), token)
		| Token (Word_classe.Nom, (nom, informations)) -> pass s; (Node (Word_classe.Nom, informations, [Leaf nom]), token)
		| Token (_, (word, _)) -> (Error (Missing (Word_classe.Nom, word)), token)
		end

(** 
  This module provides functions to process adjectives in a sentence.

  @param s The input sentence structure containing tokens and indices.

  @return A tuple containing a parse tree node and the current token.

  The main function is [get_adjectifs], which processes the input sentence to extract adjectives and their information.
  
  - [make_adj_tree_list adj_list adj_informations_list] creates a list of adjective parse tree nodes from the given lists of adjectives and their information.
  - [gender_number_comparison adj_informations_1 adj_informations_2] compares two lists of adjective information (gender and number) and returns the most appropriate one.
  - [aux' s current_adj_list current_adj_informations_list current_token] is a recursive helper function that processes the sentence to extract adjectives and build the parse tree.

  The function handles various cases:
  - If the end of the sentence is reached, it returns the appropriate parse tree node based on the collected adjectives.
  - If an unknown word is encountered, it returns an error.
  - If an adjective token is encountered, it updates the current lists and continues processing.
  - If a non-adjective token is encountered, it returns the appropriate parse tree node based on the collected adjectives.

  @raise Failure if the adjective lists and their information lists do not have the same length or if the adjective information comparison fails.
*)
and get_adjectifs s =
	let make_adj_tree_list adj_list adj_informations_list =
		let rec aux adj_list adj_informations_list result =
			match adj_list, adj_informations_list with
			| [], [] -> result
			| h :: t, h_info :: t_info -> aux t t_info (Node (Word_classe.Adjectif, h_info, [Leaf h]) :: result)
			| _ -> failwith "make_adj_tree_list : adj_list and adj_informations_list doesn't have the same length"
		in
		List.rev (aux adj_list adj_informations_list [])
	in
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
	let rec aux' s current_adj_list current_adj_informations_list current_token =
		if s.indice >= s.length then
			match current_adj_list with
			| [] -> (Error Empty_sentence, Token.Token (Word_classe.Adjectif, ("", [])))
			| _ -> (Node (Word_classe.MultipleAdj, (List.fold_left gender_number_comparison ["e"; "i"] current_adj_informations_list), make_adj_tree_list current_adj_list current_adj_informations_list), current_token)
		else
			begin
				let token = s.t_a.(s.indice) in
				match token with
				| Unknown w -> (Error ( Unknown_word w ), token)
				| Token (Word_classe.Adjectif, (adj, informations)) 
					-> 
						begin
							pass s;
							match current_adj_list with
							| [] -> aux' s [adj] [informations] token
							| _ -> 
								let (success, result_informations) = check_adjectives token current_token in
								if success then
									aux' s (adj :: current_adj_list) (informations :: current_adj_informations_list) (Token.Token (Word_classe.MultipleAdj, (Token.get_word current_token ^ " " ^ adj, result_informations)))
								else
									(Error ( Accord ((Word_classe.Adjectif, adj), (Word_classe.Adjectif, Token.get_word current_token)) ), token)
						end
				| Token _ ->
					match current_adj_list with
					| [] -> (Empty, Token.Token (Word_classe.Adjectif, ("", [])))
					| [adj] -> (Node (Word_classe.Adjectif, (List.fold_left gender_number_comparison ["e"; "i"] current_adj_informations_list), [Leaf adj]), current_token)
					| _ -> (Node (Word_classe.MultipleAdj, (List.fold_left gender_number_comparison ["e"; "i"] current_adj_informations_list), ( make_adj_tree_list current_adj_list current_adj_informations_list ) ), current_token)
			end
	in
	aux' s [] [] ( Token.Token (Word_classe.Adjectif, ("", [])) )


(** [get_verb s] is a function that processes the input sentence [s] to extract a verb token.
  It returns a tuple containing an error or a node and the token.
  
  - If the sentence index is greater than or equal to the sentence length, it returns an error indicating an empty sentence.
  - If the token at the current index is unknown, it returns an error indicating an unknown word.
  - If the token is a verb, it advances the sentence index and returns a node containing the verb information and the token.
  - If the token is not a verb, it returns an error indicating a missing verb and the token.
  
  @param s The input sentence to process.
  @return A tuple containing either an error or a node and the token.
*)
and get_verb s =
	if s.indice >= s.length then
		(Error Empty_sentence, Token.Token (Word_classe.Verbe, ("", [])))
	else
		begin
		let token = s.t_a.(s.indice) in
		match token with
		| Unknown w -> (Error ( Unknown_word w ), token)
		| Token (Word_classe.Verbe, (verb, informations)) -> pass s; (Node (Word_classe.Verbe, informations, [Leaf verb]), token)
		| Token (_, (word, _)) -> (Error (Missing (Word_classe.Verbe, word)), token)
		end

(** [get_pronom_sujet s] analyzes the token stream [s] to identify and process a subject pronoun.
  - If the current index in the stream [s] is beyond its length, it returns an [Error Empty_sentence] and a default token.
  - If the current token is an unknown word, it returns an [Error (Unknown_word w)] and the token.
  - If the current token is a subject pronoun, it advances the stream and returns a [Node] with the pronoun information and the token.
  - If the current token is not a subject pronoun, it returns an [Error (Missing (Word_classe.Pronom_sujet, word))] and the token.
  
  @param s The token stream to be analyzed.
  @return A tuple containing either an error or a node, and the current token.
*)
and get_pronom_sujet s =
	if s.indice >= s.length then
		(Error Empty_sentence, Token.Token (Word_classe.Pronom_sujet, ("", [])))
	else
		begin
		let token = s.t_a.(s.indice) in
		match token with
		| Unknown w -> (Error ( Unknown_word w ), token)
		| Token (Word_classe.Pronom_sujet, (pronom, informations)) -> pass s; (Node (Word_classe.Pronom_sujet, informations, [Leaf pronom]), token)
		| Token (_, (word, _)) -> (Error (Missing (Word_classe.Pronom_sujet, word)), token)
		end

(** [string_to_item s] converts a string [s] into a list of items.
  Each item is a record with the following fields:
  - [t_a]: an array of tokens derived from the string [s]
  - [indice]: an index initialized to 0
  - [length]: the length of the token array

  The function first converts the string [s] into a list of token lists
  using [Token.sentence_to_token_list] and [Token.all_possibility].
  Then, it recursively processes each token list, converting it into an
  array and creating a record for each array.

  @param s The input string to be converted.
  @return A list of items, where each item is a record containing the token array,
      an initial index, and the length of the token array.
*)
let string_to_item s =
	let token_list = Token.sentence_to_token_list s |> Token.all_possibility in
	Token.print_token_list_list token_list; print_newline ();
	let rec aux l =
		match l with
		| [] -> []
		| h :: t -> let token_array = h |> Utility.array_of_list in { t_a = token_array; indice = 0; length = Array.length token_array } :: aux t
	in
	aux token_list

(** [item_list_to_syntax_tree_list item] converts a list of items into a list of syntax trees.
  @param item A list of items to be converted.
  @return A list of syntax trees obtained by applying [get_syntax_tree] to each item in the input list.
*)
let item_list_to_syntax_tree_list item =
	List.map get_syntax_tree item

(** [string_to_syntax_tree_list s] converts a string [s] into a list of syntax trees.
  It first converts the string to an item and then transforms the item list into a list of syntax trees.

  @param s The input string to be converted.
  @return A list of syntax trees derived from the input string.
*)
let string_to_syntax_tree_list s =
	s |> string_to_item |> item_list_to_syntax_tree_list
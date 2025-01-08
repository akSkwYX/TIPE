open Syntax_tree

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
	| Error e -> Printf.fprintf file "child {node {%s}}" (Checkings.string_of_error e)
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
let is_a_success_result (tree, token) =
	match tree with
	| Empty | Error _ -> false
	| _ -> true


(** [get_results tree_list] filters the given [tree_list] to only include 
  successful syntax trees and prints the results in TeX format. 
  If no successful trees are found, it prints "Not a correct sentence". 
  Otherwise, it prints "Correct sentence".

  @param tree_list The list of syntax trees to be processed.
*)
let get_results result_list =
	let (tree_list, token_list) = Utility.disjoin_in_2_lists result_list in
	let filtered_list = List.filter is_a_success_result result_list in
	let (filtered_tree_list, filtered_token_list) = Utility.disjoin_in_2_lists filtered_list in
	match filtered_list with
	| [] -> syntax_tree_list_in_tex tree_list; print_string "Not a correct sentence\n"
	| _ -> syntax_tree_list_in_tex (Syntax_tree.distinct filtered_tree_list); print_string "Corrected sentence :\n"; List.iter (fun x -> Token.print_token x; print_newline ()) (Token.distinct filtered_token_list)

(* Represente the sentence which will be itterated over with indice being the index of where is the correction actually *)
type item =
	{ 
	t_a : Token.token array;
	mutable indice : int;
	length : int;
	}

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
let rec get_syntax_tree sentence =
	if sentence.indice > sentence.length then
		[(Error Empty_sentence, Token.Token (Word_classe.GV, ("", [])))]
	else
		let verbal_group_list = get_verbal_group sentence in
		verbal_group_list

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
			| [] -> result
      | ((Error e, token), _)::t | (_, (Error e, token))::t -> aux t ((Error e, token)::result)
      | ((subject_tree, subject_token), (verb_tree, verb_token))::t 
        ->	aux t ((Correction.correct_verbal_group subject_tree subject_token verb_tree verb_token) @ result)
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
          | Node (Word_classe.Pronom_sujet, informations, _) -> (Node (Word_classe.Sujet, informations, [pronoun_subject_tree]), Token.Token (Word_classe.Sujet, (Token.get_word pronoun_subject_token, Token.get_information pronoun_subject_token)))
          | _ -> failwith "get_subject : wrong tree, waiting for a Pronom_sujet"
        ) pronoun_subject_list
      end
		| Word_classe.Verbe
			-> [(Empty, Token.Token (Word_classe.Sujet, ("", [])))]
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
  let construct_nominal_group det_list adjective1_list noun_list adjective2_list =
		let combination = Utility.join_4_lists det_list adjective1_list noun_list adjective2_list in
		let rec aux l result =
			match l with
			| [] -> result
			| ((Error e, token), _, _, _)::t | (_, (Error e, token), _, _)::t | (_, _, (Error e, token), _)::t | (_, _, _, (Error e, token))::t
				-> aux t ((Error e, token)::result)
			| ((det_tree, det_token), (adj_tree, adj_token), (noun_tree, noun_token), (adj_tree_2, adj_token_2))::t
				->  aux t ((Correction.correct_nominal_group det_tree det_token adj_tree adj_token noun_tree noun_token adj_tree_2 adj_token_2) @ result)
		in
		aux combination []
	in
	let det_list = get_determinant s in
	let adjective1_list = get_adjectifs s in
	let noun_list = get_nom s in
	let adjective2_list = get_adjectifs s in
	construct_nominal_group det_list adjective1_list noun_list adjective2_list

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
		[(Error Empty_sentence, Token.Token (Word_classe.Determinant, ("", [])))]
	else
		begin
		let token = s.t_a.(s.indice) in
		match token with
		| Unknown w -> [(Error ( Unknown_word w ), token)]
		| Token (Word_classe.Determinant, (det, informations)) -> pass s; [(Node (Word_classe.Determinant, informations, [Leaf det]), token)]
		| Token (_, (word, _)) -> [(Error (Missing (Word_classe.Determinant, word)), token)]
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
		[(Error Empty_sentence, Token.Token (Word_classe.Nom, ("", [])))]
	else
		begin
		let token = s.t_a.(s.indice) in
		match token with
		| Unknown w -> [(Error ( Unknown_word w ), token)]
		| Token (Word_classe.Nom, (nom, informations)) -> pass s; [(Node (Word_classe.Nom, informations, [Leaf nom]), token)]
		| Token (_, (word, _)) -> [(Error (Missing (Word_classe.Nom, word)), token)]
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
			| [] -> (Empty, Token.Token (Word_classe.Adjectif, ("", [])))
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
								let (success, result_informations) = Checkings.check_adjectives token current_token in
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
	[aux' s [] [] ( Token.Token (Word_classe.Adjectif, ("", [])) )]


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
		[(Empty, Token.Token (Word_classe.Verbe, ("", [])))]
	else
		begin
		let token = s.t_a.(s.indice) in
		match token with
		| Unknown w -> [(Error ( Unknown_word w ), token)]
		| Token (Word_classe.Verbe, (verb, informations)) -> pass s; [(Node (Word_classe.Verbe, informations, [Leaf verb]), token)]
		| Token (_, (word, _)) -> [(Error (Missing (Word_classe.Verbe, word)), token)]
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
		[(Error Empty_sentence, Token.Token (Word_classe.Pronom_sujet, ("", [])))]
	else
		begin
		let token = s.t_a.(s.indice) in
		match token with
		| Unknown w -> [(Error ( Unknown_word w ), token)]
		| Token (Word_classe.Pronom_sujet, (pronom, informations)) -> pass s; [(Node (Word_classe.Pronom_sujet, informations, [Leaf pronom]), token)]
		| Token (_, (word, _)) -> [(Error (Missing (Word_classe.Pronom_sujet, word)), token)]
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
	(* Token.print_token_list_list token_list; print_newline (); *)
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
	List.concat_map get_syntax_tree item

(** [string_to_syntax_tree_list s] converts a string [s] into a list of syntax trees.
  It first converts the string to an item and then transforms the item list into a list of syntax trees.

  @param s The input string to be converted.
  @return A list of syntax trees derived from the input string.
*)
let string_to_syntax_tree_list s =
	s |> string_to_item |> item_list_to_syntax_tree_list
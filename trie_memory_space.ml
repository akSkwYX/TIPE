let ( @. ) = Fun.compose

let word_without_first_char word =
	String.sub word 1 (String.length word - 1)

let word_without_last_char word =
	String.sub word 0 (String.length word - 1)

let string_without_x_last_char string x =
	String.sub string 0 (String.length string - x)

let list_of_string string =
	string |> String.to_seq |> List.of_seq

let string_of_list list =
	list |> List.to_seq |> String.of_seq

let char_of_string string =
	string.[0]

let print_char_list = List.iter (fun x -> print_char x; print_string " | ")

let print_string_list = List.iter (fun x -> print_string x; print_string " | ")

let print_bool = print_endline @. string_of_bool

let rec list_without_x_last_char x list =
	match list with
	| [] -> []
	| h :: t when x = 0 -> list
	| h :: t -> list_without_x_last_char (x - 1) t

module Trie =
	struct

		type trie =
			| Node of (bool * (char list) list) * (char * trie) list
			| Leaf of bool * (char list) list

		let trie_create = Leaf (false, [])

		let rec trie_insert trie word information =
			(* print_string word; print_string " : "; print_char_list information; print_newline (); *)
			match trie with
			| Leaf (b,i) when word = "" -> Leaf (true, information::i)
			| Leaf (b,i) -> trie_insert (Node ((b,i), [(word.[0], Leaf (false, []))])) word information
			| Node ((b,i), list) when word = "" -> Node ((true, information::i), list)
			| Node ((b,i), list) ->
				let rec aux l head =
					match l with
					| [] -> Node ( (b,i), (word.[0], ( trie_insert trie_create (word_without_first_char word) information) )::list )
					| (c, t) :: tail when c = word.[0] -> Node ( (b,i), ( (c, trie_insert t (word_without_first_char word) information) :: head @ tail ) )
					| h :: tail -> aux tail (h :: head)
				in aux list []
				
		let rec trie_search trie word  =
			match trie with
			| Leaf (b,i) when word = "" -> (b,i)
			| Leaf (b,i) -> (false, [])
			| Node ((b,i), l) when word = "" -> (b,i)
			| Node ((b,i), l) -> 
				let rec aux l =
					match l with
					| [] -> (false, [])
					| (c, t) :: tl -> if c = word.[0] then trie_search t (word_without_first_char word) else aux tl
				in aux l

		let print_trie_node (b, l) =
			b |> string_of_bool |> print_string; print_string " ";
			let rec aux l =
				match l with
				| [] -> print_newline ()
				| h :: t -> print_char_list h; aux t
			in aux l

		(* let rec print_trie trie =
			match trie with
			| Leaf b -> b |> string_of_bool |> print_string; print_string " "; print_newline ()
			| Node (b, l) ->
				b |> string_of_bool |> print_string; print_string " ";
				let rec aux l =
					match l with
					| [] -> print_newline ()
					| (c, t) :: tl -> print_char c; print_string " "; print_trie t; aux tl
				in aux l*)
	end

(* ---------------------------------------------------------------------------------------------------------------------------------------------------------------- *)

let read_dictionnary file =
	let file = open_in file in
	let rec read_lines trie =
		let line = In_channel.input_line file |> Option.map (fun x -> String.split_on_char ',' x) in
		match line with
		| None -> trie
		| Some (word::information) -> (* print_string word; print_string " : "; print_char_list (List.map char_of_string information); print_newline (); *)
																	read_lines ( Trie.trie_insert trie word (List.map char_of_string information) )
		| Some [] -> failwith "read_dictionnary : empty line"
	in
	read_lines Trie.trie_create

let dictionnary = read_dictionnary "GroupeNominal.txt"
let determinants_dictionary = read_dictionnary "Liste_determinants.txt"
let nom_dictionary = read_dictionnary "Liste_nom_communs.txt"
let adjectif_dictionary = read_dictionnary "Liste_adjectifs.txt"

let sentence_to_list s =
	let rec aux s_l l =
		match s_l with
		| [] -> l
		| ' ' :: t -> aux t l
		| c :: t ->
			let rec find_word l w length_w =
				match l with
				| [] -> w, length_w
				| ' ' :: t -> w, length_w
				| c :: t -> find_word t (c::w) (length_w + 1)
			in
			let w, length_w = find_word s_l [] 0
			in
			aux (list_without_x_last_char length_w s_l) ((string_of_list w)::l)
	in
	aux (List.rev (list_of_string s) ) []

let rec orthographic_correction l =
	match l with
	| [] -> []
	| h :: t -> let is_word = Trie.trie_search dictionnary h in
						  is_word :: (orthographic_correction t)

let rec verify_sentence l =
	match l with
	| [] -> true
	| h :: t -> if h then verify_sentence t else false

let option_to_string o =
	match o with
	| None -> failwith "option_to_string : None"
	| Some x -> x

type token =
	| D of (string * char list)
	| N of (string * char list)
	| A of (string * char list)
	| F

let sentence_to_token_list s =
	let rec aux list_word list_token =
		match list_word with
		| [] -> list_token
		| word :: t -> let (is_word, information) = Trie.trie_search dictionnary word in
										if is_word then
											let rec match_information i possibility=
												match i with
												| [] -> possibility
												| ('D'::complementary_information) :: tl -> match_information tl (D (word, complementary_information) :: possibility)
												| ('N'::complementary_information) :: tl -> match_information tl (N (word, complementary_information) :: possibility)
												| ('A'::complementary_information) :: tl -> match_information tl (A (word, complementary_information) :: possibility)
												| _ when possibility = [] -> [F]
												| _ -> failwith "match_information : wrong information"
											in aux t (match_information information [] :: list_token)
										else
											aux t ([F] :: list_token)
	in
	List.rev (aux (sentence_to_list s) [])

let print_token_list =
	List.iter (fun x -> match x with
												| D (s, l) -> print_string "D "; print_string s; print_string " "; print_char_list l; print_string " ||  "
												| N (s, l) -> print_string "N "; print_string s; print_string " "; print_char_list l; print_string " ||  "
												| A (s, l) -> print_string "A "; print_string s; print_string " "; print_char_list l; print_string " ||  "
												| F -> print_string "F "; print_string " ||  "
											)

let print_token_list_list =
	List.iter (fun x -> print_token_list x; print_newline ())

type grammar = | Production of (Trie.trie * grammar list) | E_production

(* 
D -> A -> N -> A
			-> E
  -> N -> A
	   -> E
*)
let gn_grammar = 
	Production (determinants_dictionary, [Production (adjectif_dictionary, [Production (nom_dictionary, [Production (adjectif_dictionary, [E_production])
																										 ;E_production
																										]
																					   )
																			]
													 )
										;Production (nom_dictionary, [Production (adjectif_dictionary, [E_production]);
																	  E_production
																	 ]
													)
										]
				)

(* match trie with
| Leaf (b,i) when word = "" -> (b,i)
| Leaf (b,i) -> (false, [])
| Node ((b,i), l) when word = "" -> (b,i)
| Node ((b,i), l) -> 
	let rec aux l =
		match l with
		| [] -> (false, [])
		| (c, t) :: tl -> if c = word.[0] then trie_search t (word_without_first_char word) else aux tl
	in aux l *)

let rec has_E_production grammar =
	List.exists (fun x -> match x with | E_production -> true | _ -> false) grammar

let get_first_word token_list =
	match token_list with
	| [] -> failwith "get_first_word : empty list"
	| D (s, l) :: t -> s
	| N (s, l) :: t -> s
	| A (s, l) :: t -> s
	| F :: t -> failwith "get_first_word : F"

let rec search_grammar grammar (token_list:token list) =
	match grammar with
	| E_production -> token_list = []
	| Production (_, x) when token_list = [] -> has_E_production x
	| Production (trie, lst) ->
		let rec aux l =
			match l with
			| [] -> false
			| (E_production) :: t -> token_list = [] || aux t
			| (Production (trie, prods)) :: t -> if fst ( Trie.trie_search trie (get_first_word token_list) ) then
													match prods with
													| [] -> token_list = [] || aux t
													| h :: tl -> search_grammar h (List.tl token_list) || aux tl || aux t
												 else
													aux t
		in
		aux lst



let test = search_grammar gn_grammar [D ("le", ['m'; 's']); A ("beau", ['m'; 's']); N ("chat", ['m'; 's']); A ("rouge", ['e'; 's'])] |> print_bool
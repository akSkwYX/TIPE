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

let list_to_array l = List.to_seq l |> Array.of_seq

let option_to_string o =
	match o with
	| None -> failwith "option_to_string : None"
	| Some x -> x

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
(* Orthographic correction and file manipulation *)

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
(* let determinants_dictionary = read_dictionnary "Liste_determinants.txt"
let nom_dictionary = read_dictionnary "Liste_nom_communs.txt"
let adjectif_dictionary = read_dictionnary "Liste_adjectifs.txt" *)

let sentence_to_list s =
	let rec aux s_l l =
		match s_l with
		| [] -> l
		| ' ' :: t -> aux t l
		| '\'' :: t ->
			let rec find_word l w length_w =
				match l with
				| [] -> w, length_w
				| ' ' :: t | '\'' :: t -> w, length_w
				| c :: t -> find_word t (c::w) (length_w + 1)
			in
			let w, length_w = find_word (List.tl s_l) [] 0
			in
			aux (list_without_x_last_char (length_w+1) s_l) ((string_of_list (w @ ['\'']))::l)
		| c :: t ->
			let rec find_word l w length_w =
				match l with
				| [] -> w, length_w
				| ' ' :: t | '\'' :: t -> w, length_w
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

(* ---------------------------------------------------------------------------------------------------------------------------------------------------------------- *)
(* Token *)

type word_classe =
	| S
	| GV
	| GN
	| MultipleAdj
	| Pronom_sujet
	| Sujet
	| Verbe
	| Determinant
	| Nom
	| Adjectif

type token = 
	| Token of ( word_classe * (string * char list) )
	| Unknown

let sentence_to_token_list s =
	let rec aux list_word list_token =
		match list_word with
		| [] -> list_token
		| word :: t -> let (is_word, information) = Trie.trie_search dictionnary word in
										if is_word then
											let rec match_information i possibility =
												match i with
												| [] -> possibility
												| ('D'::complementary_information) :: tl -> match_information tl (Token ( Determinant, (word, complementary_information) ) :: possibility)
												| ('N'::complementary_information) :: tl -> match_information tl (Token ( Nom, (word, complementary_information) ) :: possibility)
												| ('A'::complementary_information) :: tl -> match_information tl (Token ( Adjectif, (word, complementary_information) ) :: possibility)
												| _ when possibility = [] -> [Unknown]
												| _ -> failwith "match_information : wrong information"
											in aux t (match_information information [] :: list_token)
										else
											aux t ([Unknown] :: list_token)
	in
	List.rev (aux (sentence_to_list s) [])

let print_token_list =
	List.iter (fun x -> match x with
									| Token (Determinant, (s, l)) -> print_string "D "; print_string s; print_string " "; print_char_list l; print_string " ||  "
									| Token (Nom, (s, l)) -> print_string "N "; print_string s; print_string " "; print_char_list l; print_string " ||  "
									| Token (Adjectif, (s, l)) -> print_string "A "; print_string s; print_string " "; print_char_list l; print_string " ||  "
									| Token (S, (s, l)) -> print_string "S : "
									| Token (GV, (s, l)) -> print_string "GV : "
									| Token (GN, (s, l)) -> print_string "GN : "
									| Token (MultipleAdj, (s, l)) -> print_string "MA : "
									| Token (Pronom_sujet, (s, l)) -> print_string "PS "; print_string s; print_string " "; print_char_list l; print_string " ||  "
									| Token (Sujet, (s, l)) -> print_string "SU "; print_string s; print_string " "; print_char_list l; print_string " ||  "
									| Token (Verbe, (s, l)) -> print_string "V "; print_string s; print_string " "; print_char_list l; print_string " ||  "
									| Unknown -> print_string "F "; print_string " ||  "
			)

let print_token_list_list =
	List.iter (fun x -> print_token_list x; print_newline ())

let get_word token =
	match token with
	| Token (Determinant, (s, l)) -> s
	| Token (Nom, (s, l)) -> s
	| Token (Adjectif, (s, l)) -> s
	| Token (Sujet, (s, l)) -> s
	| Token (Verbe, (s, l)) -> s
	| Token (Pronom_sujet, (s, l)) -> s
	| Token (S, (s, l)) -> failwith "get_word : trying to get a word from a S"
	| Token (GV, (s, l)) -> failwith "get_word : trying to get a word from a GV"
	| Token (GN, (s, l)) -> failwith "get_word : trying to get a word from a GN"
	| Token (MultipleAdj, (s, l)) -> failwith "get_word : trying to get a word from a MultipleAdj"
	| Unknown -> failwith "get_word : Unknown"

let get_word_classe token =
	match token with
	| Token (Determinant, (s, l)) -> Determinant
	| Token (Nom, (s, l)) -> Nom
	| Token (Adjectif, (s, l)) -> Adjectif
	| Token (Sujet, (s, l)) -> Sujet
	| Token (Verbe, (s, l)) -> Verbe
	| Token (Pronom_sujet, (s, l)) -> Pronom_sujet
	| Token (S, (s, l)) -> S
	| Token (GV, (s, l)) -> GV
	| Token (GN, (s, l)) -> GN
	| Token (MultipleAdj, (s, l)) -> MultipleAdj
	| Unknown -> failwith "get_word_classe : Unknown"

let all_possibility (l:token list list) :token list list =
	let rec first_floor_course l acc =
		match l with
		| [] -> acc
		| [h] :: t -> first_floor_course t (List.map (fun x -> h :: x) acc)
		| h :: t ->
			let rec second_floor_course l =
				match l with
				| [] -> []
				| hd :: tl -> List.map (fun x -> hd :: x) acc @ second_floor_course tl
			in
			first_floor_course t (second_floor_course h)
	in
	List.map (List.rev) (first_floor_course l [[]])

(* let test = "le chat rouge" |> sentence_to_token_list
let () = test |> print_token_list_list; print_newline ()
let () = test |> all_possibility |> print_token_list_list *)

(* ---------------------------------------------------------------------------------------------------------------------------------------------------------------- *)
(* Grammar *)

type syntax_tree =
	| Node of word_classe * syntax_tree list
	| Leaf of string

type item =
	{ 
	t_a : token array;
	mutable indice : int;
	length : int;
	}

let pass sentence state =
	if sentence.indice > sentence.length then
		failwith "pass : empty sentence"
	else 
		if state = get_word_classe sentence.t_a.(sentence.indice) then 
			sentence.indice <- sentence.indice + 1 
		else 
			failwith "pass : wrong state"

let rec get_syntax_tree sentence =
	if sentence.indice > sentence.length then
		failwith "get_syntax_tree : empty sentence"
	else
		get_verbal_group sentence

and get_verbal_group s =
	Node (GV, [get_subject s; get_verb s])

and get_subject s =
	if get_word_classe s.t_a.(s.indice) = Determinant then
		Node (GN, [get_nominal_group s])
	else if get_word_classe s.t_a.(s.indice) = Pronom_sujet then
		Node (Pronom_sujet, [get_pronom_sujet s])
	else
		failwith "get_subject : wrong state"

and get_nominal_group s =
	Node (GN, [get_determinant s; get_adjectifs s; get_nom s; get_adjectifs s])

and get_determinant s =
	let det = get_word s.t_a.(s.indice) in
	pass s Determinant;
	Leaf det

and get_nom s =
	let nom = get_word s.t_a.(s.indice) in
	pass s Nom;
	Leaf nom

and get_adjectifs s =
	if get_word_classe s.t_a.(s.indice) = Adjectif then
		if get_word_classe s.t_a.(s.indice + 1) = Adjectif then
			let adj = get_word s.t_a.(s.indice) in
			pass s Adjectif;
			Node (MultipleAdj, [Leaf adj; get_adjectifs s])
		else
			Leaf (get_word s.t_a.(s.indice))
	else
		failwith "get_adjectifs : wrong state"

and get_verb s =
	let verb = get_word s.t_a.(s.indice) in
	pass s Verbe;
	Leaf verb

and get_pronom_sujet s =
	let pronom = get_word s.t_a.(s.indice) in
	pass s Pronom_sujet;
	Leaf pronom

(* 
type production =
	| P of (word_classe * production list)
	| End of string

type grammar = production

let gn_grammar =
	P (Determinant, [P (Adjectif, [P (Nom, [End "Groupe nominal : Déterminant Adjectif Nom";
											 P (Adjectif, [End "Groupe nominal : Déterminant Adjectif Nom Adjectif"])])]);
					  P (Nom, [End "Groupe nominal : Déterminant Nom";
					   		   P (Adjectif, [End "Groupe nominal : Déterminant Nom"])])])

let rec recognize_by_grammar (grammar:grammar) (sentence:token list) :bool =
	match grammar, sentence with
	| End _, _ -> sentence = []
	| P (wc, lst), [] -> false
	| P (wc, lst), tk :: tl ->
		if get_word_classe tk = wc then
			let rec aux l =
				match l with
				| [] -> false
				| h :: t -> recognize_by_grammar h tl || aux t
			in aux lst
		else
			false

let verify_sentence_by_grammar s =
	let token_list = sentence_to_token_list s in
	List.exists (fun x -> recognize_by_grammar gn_grammar x) (all_possibility token_list)


let test = In_channel.input_line In_channel.stdin |> option_to_string |> verify_sentence_by_grammar |> print_bool
 *)
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
			| Node of (bool * (string list) list) * (char * trie) list
			| Leaf of bool * (string list) list

		let trie_create = Leaf (false, [])

		let rec trie_insert trie word information =
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
				| h :: t -> print_string_list h; aux t
			in aux l
	end

(* ---------------------------------------------------------------------------------------------------------------------------------------------------------------- *)
(* Orthographic correction and file manipulation *)

let read_dictionnary file =
	let file = open_in file in
	let rec read_lines trie =
		let line = In_channel.input_line file |> Option.map (fun x -> String.split_on_char ',' x) in
		match line with
		| None -> trie
		| Some (word::information) -> read_lines ( Trie.trie_insert trie word information )
		| Some [] -> failwith "read_dictionnary : empty line"
	in
	read_lines Trie.trie_create

let dictionnary = read_dictionnary "Dictionnary.txt"

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
	| Token of ( word_classe * (string * string list) )
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
												| ("D"::complementary_information) :: tl -> match_information tl (Token ( Determinant, (word, complementary_information) ) :: possibility)
												| ("N"::complementary_information) :: tl -> match_information tl (Token ( Nom, (word, complementary_information) ) :: possibility)
												| ("A"::complementary_information) :: tl -> match_information tl (Token ( Adjectif, (word, complementary_information) ) :: possibility)
												| ("V"::complementary_information) :: tl -> match_information tl (Token ( Verbe, (word, complementary_information) ) :: possibility)
												| ("Os"::complementary_information) :: tl -> match_information tl (Token ( Pronom_sujet, (word, complementary_information) ) :: possibility)
												| _ when possibility = [] -> [Unknown]
												| _ -> failwith "match_information : wrong information"
											in aux t (match_information information [] :: list_token)
										else
											aux t ([Unknown] :: list_token)
	in
	List.rev (aux (sentence_to_list s) [])

let print_token_list =
	List.iter (fun x -> match x with
									| Token (Determinant, (s, l)) -> print_string "D "; print_string s; print_string " "; print_string_list l; print_string " ||  "
									| Token (Nom, (s, l)) -> print_string "N "; print_string s; print_string " "; print_string_list l; print_string " ||  "
									| Token (Adjectif, (s, l)) -> print_string "A "; print_string s; print_string " "; print_string_list l; print_string " ||  "
									| Token (S, (s, l)) -> print_string "S : "
									| Token (GV, (s, l)) -> print_string "GV : "
									| Token (GN, (s, l)) -> print_string "GN : "
									| Token (MultipleAdj, (s, l)) -> print_string "MA : "
									| Token (Pronom_sujet, (s, l)) -> print_string "PS "; print_string s; print_string " "; print_string_list l; print_string " ||  "
									| Token (Sujet, (s, l)) -> print_string "SU "; print_string s; print_string " "; print_string_list l; print_string " ||  "
									| Token (Verbe, (s, l)) -> print_string "V "; print_string s; print_string " "; print_string_list l; print_string " ||  "
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

let word_classe_to_string wc =
	match wc with
	| S -> "S"
	| GV -> "GV"
	| GN -> "GN"
	| MultipleAdj -> "MultipleAdj"
	| Pronom_sujet -> "Pronom\\_sujet"
	| Sujet -> "Sujet"
	| Verbe -> "Verbe"
	| Determinant -> "Determinant"
	| Nom -> "Nom"
	| Adjectif -> "Adjectif"

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

let rec syntax_tree_in_tex_aux file tree =
	match tree with
	| Leaf s -> Printf.fprintf file "child {node {%s}}" s
	| Node (wc, l) ->
		Printf.fprintf file "child { node {%s} " (word_classe_to_string wc);
		List.iter (fun x -> syntax_tree_in_tex_aux file x) l;
		Printf.fprintf file "}"

let syntax_tree_in_tex tree file =
	Printf.fprintf file "\\begin{center}\n\\begin{tikzpicture}\n\\node{S}[sibling distance = 3cm, level distance = 3cm]\n";
	syntax_tree_in_tex_aux file tree;
	Printf.fprintf file ";\n\\end{tikzpicture}\n\\end{center}\n"

let syntax_tree_list_in_tex tree_list =
	let file = open_out "syntax_tree.tex" in
	Printf.fprintf file "\\documentclass{article}\n\\usepackage{tikz}\n\\begin{document}\n";
	List.iter (fun x -> syntax_tree_in_tex x file) tree_list;
	Printf.fprintf file "\\end{document}";
	close_out file;
	Sys.command "pdflatex syntax_tree.tex"

let pass sentence state =
	if sentence.indice > sentence.length then
		failwith "pass : empty sentence"
	else 
		if state = get_word_classe sentence.t_a.(sentence.indice) then 
			(sentence.indice <- sentence.indice + 1;
			true)
		else 
			false

let rec get_syntax_tree sentence =
	if sentence.indice > sentence.length then
		Leaf "get\\_syntax\\_tree : empty sentence"
	else
		get_verbal_group sentence

and get_verbal_group s =
	let subject_tree = get_subject s in
	let verb_tree = get_verb s in
	Node (GV, [subject_tree; verb_tree])

and get_subject s =
	if get_word_classe s.t_a.(s.indice) = Determinant then
		Node (GN, [get_nominal_group s])
	else if get_word_classe s.t_a.(s.indice) = Pronom_sujet then
		Node (Pronom_sujet, [get_pronom_sujet s])
	else
		Leaf "get\\_subject : wrong state"

and get_nominal_group s =
	let det_tree = get_determinant s in
	print_string "get_nominal_group det : "; print_int s.indice; print_newline ();
	let adj_tree = get_adjectifs s in
	print_string "get_nominal_group adj : "; print_int s.indice; print_newline ();
	let nom_tree = get_nom s in
	print_string "get_nominal_group nom : "; print_int s.indice; print_newline ();
	let adj_tree_2 = get_adjectifs s in
	print_string "get_nominal_group adj_2 : "; print_int s.indice; print_newline ();
	Node (GN, [det_tree; adj_tree; nom_tree; adj_tree_2])

and get_determinant s =
	let det = get_word s.t_a.(s.indice) in
	if pass s Determinant then
		Node (Determinant, [Leaf det])
	else
		Leaf "get\\_determinant : wrong state"

and get_nom s =
	let nom = get_word s.t_a.(s.indice) in
	if pass s Nom then
		Node (Nom, [Leaf nom])
	else
		Leaf "get\\_nom : wrong state"

and get_adjectifs s =
	if get_word_classe s.t_a.(s.indice) = Adjectif then
		let adj = get_word s.t_a.(s.indice) in
		if get_word_classe s.t_a.(s.indice + 1) = Adjectif then
			if pass s Adjectif then
				Node (MultipleAdj, [Leaf adj; get_adjectifs s])
			else
				Leaf "get\\_adjectifs : waiting for adjective and doesn't get it"
		else
			let _ = pass s Adjectif in
			Node (Adjectif, [Leaf (adj)])
	else
		Leaf ""

and get_verb s =
	print_string "get_verb : "; print_int s.indice; print_newline ();
	let verb = get_word s.t_a.(s.indice) in
	if pass s Verbe then
		Node (Verbe, [Leaf verb])
	else
		Leaf "get\\_verb : wrong state"

and get_pronom_sujet s =
	let pronom = get_word s.t_a.(s.indice) in
	if pass s Pronom_sujet then
		Node (Pronom_sujet, [Leaf pronom])
	else
		Leaf "get\\_pronom_sujet : wrong state"

let string_to_item s =
	let token_list = sentence_to_token_list s |> all_possibility in
	let rec aux l =
		match l with
		| [] -> []
		| h :: t -> let token_array = h |> list_to_array in { t_a = token_array; indice = 0; length = Array.length token_array } :: aux t
	in
	aux token_list

let item_list_to_syntax_tree_list item =
	List.map get_syntax_tree item

let string_to_syntax_tree_list s =
	s |> string_to_item |> item_list_to_syntax_tree_list

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

let test = In_channel.input_line In_channel.stdin |> option_to_string |> string_to_syntax_tree_list |> syntax_tree_list_in_tex
(* 
TO REMOVE BUT WE DON'T KNOW WHAT CAN HAPPEN

let rec orthographic_correction l =
	match l with
	| [] -> []
	| h :: t -> let is_word = Trie.trie_search dictionnary h in
						  is_word :: (orthographic_correction t)

let rec verify_sentence l =
	match l with
	| [] -> true
	| h :: t -> if h then verify_sentence t else false
 *)
let ( @. ) = Fun.compose

let word_without_first_char word =
	String.sub word 1 (String.length word - 1)

let word_without_last_char word =
	String.sub word 0 (String.length word - 1)

let string_without_x_last_char string x =
	String.sub string 0 (String.length string - x)

let char_list_of_string string =
	string |> String.to_seq |> List.of_seq

let string_of_char_list list =
	list |> List.to_seq |> String.of_seq

let print_char_list = List.iter (fun x -> print_char x; print_string " | ")

let print_string_list = List.iter (fun x -> print_string x; print_string " | ")

let print_bool = print_endline @. string_of_bool

let array_of_list l = List.to_seq l |> Array.of_seq

let string_of_string_list l =
	match l with
	| [] -> ""
	| _ -> string_without_x_last_char (List.fold_left (fun acc x -> if x <> "_" then acc ^ x ^  ", " else acc ^ "\\_" ^ ", ") "" l) 2

let string_of_option o =
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

		(* What a good structure in order to represent a dictionnary
		   Contains words and some information on words *)
		type trie =
			| Node of (bool * (string list) list) * (char * trie) list
			| Leaf of bool * (string list) list

		let trie_create = Leaf (false, [])

		let rec trie_insert trie word information =
			match trie with
			(* Finding word ending on a Leaf so just adding a Leaf *)
			| Leaf (b,i) when word = "" -> Leaf (true, information::i)
			(* Reaching a Leaf but word isn't end so adding all remain letter in the trie *)
			| Leaf (b,i) -> trie_insert (Node ((b,i), [(word.[0], Leaf (false, []))])) word information
			(* End of word reached inside the trie so just adding information to the node of the word *)
			| Node ((b,i), list) when word = "" -> Node ((true, information::i), list)
			(* Reccursive inserting *)
			| Node ((b,i), list) ->
				let rec aux l head =
					match l with
					| [] -> Node ( (b,i), (word.[0], ( trie_insert trie_create (word_without_first_char word) information) )::list )
					| (c, t) :: tail when c = word.[0] -> Node ( (b,i), ( (c, trie_insert t (word_without_first_char word) information) :: head @ tail ) )
					| h :: tail -> aux tail (h :: head)
				in aux list []
				
		(* Just browse the trie but this have a fcking good complexity *)
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

		(* Just a print function for printing a node, quite useless maybe remove *)
		let print_trie_node (b, l) =
			b |> string_of_bool |> print_string; print_string " ";
			let rec aux l =
				match l with
				| [] -> print_newline ()
				| h :: t -> print_string_list h; aux t
			in aux l
	end

(* ---------------------------------------------------------------------------------------------------------------------------------------------------------------- *)
(* file manipulation *)

(* Read a .txt of dictionary with first the word and after more informations on word separated by a comma *)
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

(* ---------------------------------------------------------------------------------------------------------------------------------------------------------------- *)
(* Sentence transformations and tokenization so *)

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
	| Unknown

type token = 
	| Token of ( word_classe * (string * string list) )
	| Unknown of string


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
			aux (list_without_x_last_char (length_w+1) s_l) ((string_of_char_list (w @ ['\'']))::l)
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
			aux (list_without_x_last_char length_w s_l) ((string_of_char_list w)::l)
	in
	aux (List.rev (char_list_of_string s) ) []


(* Using precedent function and transforming the sentence to a token list list which is usefull for manipuling the sentence after *)
let sentence_to_token_list s =
	let rec aux list_word list_token =
		match list_word with
		| [] -> list_token
		| word :: t -> 
			(* Searching in dictionnary if word exists and its information if it does *)
			let (is_word, information) = Trie.trie_search dictionnary word in
			if is_word then
				let rec match_information i possibility =
					match i with
					| [] -> possibility
					| ("D"::complementary_information) :: tl -> match_information tl (Token ( Determinant, (word, complementary_information) ) :: possibility)
					| ("N"::complementary_information) :: tl -> match_information tl (Token ( Nom, (word, complementary_information) ) :: possibility)
					| ("A"::complementary_information) :: tl -> match_information tl (Token ( Adjectif, (word, complementary_information) ) :: possibility)
					| ("V"::complementary_information) :: tl -> match_information tl (Token ( Verbe, (word, complementary_information) ) :: possibility)
					| ("Os"::complementary_information) :: tl -> match_information tl (Token ( Pronom_sujet, (word, complementary_information) ) :: possibility)
					| _ when possibility = [] -> [Unknown "Unknown"]
					| _ -> failwith "match_information : wrong information"
				in aux t (match_information information [] :: list_token)
			else
				aux t ([Unknown word] :: list_token)
	in
	List.rev (aux (sentence_to_list s) [])

let string_of_token t =
	match t with
	| Token (Determinant, (s, l)) ->  "D " ^ s ^ " " ^ (string_of_string_list l)
	| Token (Nom, (s, l)) ->  "N " ^ s ^ " " ^ (string_of_string_list l)
	| Token (Adjectif, (s, l)) ->  "A " ^ s ^ " " ^ (string_of_string_list l)
	| Token (S, (s, l)) ->  "S : "
	| Token (GV, (s, l)) ->  "GV : "
	| Token (GN, (s, l)) ->  "GN : "
	| Token (MultipleAdj, (s, l)) ->  "MA : "
	| Token (Pronom_sujet, (s, l)) ->  "PS " ^ s ^ " " ^ (string_of_string_list l)
	| Token (Sujet, (s, l)) ->  "SU " ^ s ^ " " ^ (string_of_string_list l)
	| Token (Verbe, (s, l)) ->  "V " ^ s ^ " " ^ (string_of_string_list l)
	| Token (Unknown, (s, l)) ->  failwith "string_of_token : Unknown token declared as Token"
	| Unknown w ->  "Unknown " ^ w

let print_token_list =
	List.iter (fun x -> print_string (string_of_token x); print_string " | ")

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
	| Token (Unknown, (s, l)) -> failwith "get_word : Unknown token declared as Token"
	| Unknown w -> w

let get_information token =
	match token with
	| Token (Determinant, (s, l)) -> l
	| Token (Nom, (s, l)) -> l
	| Token (Adjectif, (s, l)) -> l
	| Token (Sujet, (s, l)) -> l
	| Token (Verbe, (s, l)) -> l
	| Token (Pronom_sujet, (s, l)) -> l
	| Token (GN, (s, l)) -> l
	| Token (MultipleAdj, (s, l)) -> l
	| Token (S, (s, l)) -> failwith "get_information : trying to get a information from a S"
	| Token (GV, (s, l)) -> failwith "get_information : trying to get a information from a GV"
	| Token (Unknown, (s, l)) -> failwith "get_information : Unknown token declared as Token"
	| Unknown _ -> failwith "get_information : tring to get a information from a Unknown"

let get_gender token =
	match token with
	| Token (Adjectif, (_, l)) -> List.nth l 0 (* Wanting catching error but no success trying later *)
	| Token (Nom, (_, l)) -> List.nth l 0
	| Token (Determinant, (_, l)) -> List.nth l 0
	| Token (Pronom_sujet, (_, l)) -> List.nth l 1
	| _ -> failwith "get_gender : not something which have a gender"

let get_number token =
	match token with
	| Token (Adjectif, (_, l)) -> List.nth l 1 (* Wanting catching error but no success trying later *)
	| Token (Nom, (_, l)) -> List.nth l 1
	| Token (Determinant, (_, l)) -> List.nth l 1
	| Token (Pronom_sujet, (_, l)) -> List.nth l 2
	| _ -> failwith "get_number : not something which have a number"

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
	| Token (Unknown, (s, l)) -> failwith "get_word_classe : Unknown token declared as Token"
	| Unknown _ -> Unknown

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
	| Unknown -> "Unknown"

(* Returning all sentence possible with all meaning of each word
Exemple : "rouge" can be a noun or an adjective *)
let all_possibility (l:token list list) :token list list =
	(* Firstly itterating over the token list of list of possibility for words *)
	let rec first_floor_course l acc =
		match l with
		| [] -> acc
		| [h] :: t -> first_floor_course t (List.map (fun x -> h :: x) acc)
		| h :: t ->
			(* Secondly itterating over the possibilitys of words *)
			let rec second_floor_course l =
				match l with
				| [] -> []
				(* For each possibility, adding this possibility to all sentence possible already found *)
				| hd :: tl -> List.map (fun x -> hd :: x) acc @ second_floor_course tl
			in
			first_floor_course t (second_floor_course h)
	in
	List.map (List.rev) (first_floor_course l [[]])

(* ---------------------------------------------------------------------------------------------------------------------------------------------------------------- *)
(* Grammar *)

type error =
	| Empty_sentence
	| Unrecognized of string
	| Missing of (word_classe * string)
	| Accord of (word_classe * string) * (word_classe * string)
	| Conjuguaison of string
	| No (* Idk if it's really good to do this but i need it *)

let string_of_error e =
	match e with
	| Empty_sentence -> "Empty sentence"
	| Unrecognized s -> "Unrecognized : " ^ s
	| Missing (wc, w) -> "Missing " ^ (word_classe_to_string wc) ^ " got : " ^ w
	| Accord ((wc1, w1), (wc2, w2)) -> "Accord between " ^ (word_classe_to_string wc1) ^ " " ^ w1 ^ " and " ^ (word_classe_to_string wc2) ^ " " ^ w2
	| Conjuguaison s -> "Conjuguaison : " ^ s
	| No -> "No"


type syntax_tree =
	| Node of word_classe * string list * syntax_tree list
	| Leaf of string
	| Error of error

let rec syntax_tree_in_tex_aux file tree =
	match tree with
	| Leaf s -> Printf.fprintf file "child {node {%s}}" s
	| Error e -> Printf.fprintf file "child {node {%s}}" (string_of_error e)
	| Node (wc, [], l) ->
		Printf.fprintf file "child { node {%s} " (word_classe_to_string wc);
		List.iter (fun x -> syntax_tree_in_tex_aux file x) l;
		Printf.fprintf file "}"
	| Node (wc, i, l) ->
		Printf.fprintf file "child { node {%s \\\\ %s} " (word_classe_to_string wc) (string_of_string_list i);
		List.iter (fun x -> syntax_tree_in_tex_aux file x) l;
		Printf.fprintf file "}"

let syntax_tree_in_tex tree file =
	Printf.fprintf file "\\begin{center}\n\\begin{tikzpicture}\n\\node{Sentence}[sibling distance = 3cm, level distance = 3cm, align=center]\n";
	syntax_tree_in_tex_aux file tree;
	Printf.fprintf file ";\n\\end{tikzpicture}\n\\end{center}\n"

let syntax_tree_list_in_tex tree_list =
	let file = open_out "syntax_tree.tex" in
	Printf.fprintf file "\\documentclass{article}\n\\usepackage{tikz}\n\\begin{document}\n";
	List.iter (fun x -> syntax_tree_in_tex x file) tree_list;
	Printf.fprintf file "\\end{document}";
	close_out file;
	Sys.command "pdflatex syntax_tree.tex"

(* Represente the sentence which will be itterated over with indice being the index of where is the correction actually *)
type item =
	{ 
	t_a : token array;
	mutable indice : int;
	length : int;
	}

let check_gender g1 g2 =
	match g1, g2 with
	| x, y when x = y -> true, x
	| "m", "e" | "e", "m" -> true, "m"
	| "f", "e" | "e", "f" -> true, "f"
	| _ -> false, ""

let check_number n1 n2 =
	match n1, n2 with
	| x, y when x = y -> true, x
	| "s", "i" | "i", "s" -> true, "s"
	| "p", "i" | "i", "p" -> true, "p"
	| _ -> false, ""

(* Checking gender and number compatibility for adjectives, nouns, determinants, NOT FOR OTHERS *)
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

let check_det_noun det_informations noun_informations =
	let (success, result_informations) = check_gender_number det_informations noun_informations in
	if success then
		(true, result_informations)
	else
		(false, [])

(* On suppose que les arbres sont déjà bien accordé *)
let check_adjectives adj_informations_1 adj_informations_2 =
	match adj_informations_1, adj_informations_2 with
	| _, [] -> true, adj_informations_1
	| [], _ -> true, adj_informations_2
	| _ ->
		let (success, result_informations) = check_gender_number adj_informations_1 adj_informations_2 in
		if success then
			(true, result_informations)
		else
			(false, [])

let check_noun_adjective noun_informations adj_informations =
	match noun_informations, adj_informations with
	| _, [] -> (true, noun_informations)
	| _ ->
		let (success, result_informations) = check_gender_number noun_informations adj_informations in
		if success then
			(true, result_informations)
		else
			(false, [])
	
let check_nominal_group det_tree adj_tree nom_tree adj_tree_2 =
	let get_first_adjectif_of_tree tree =
		match tree with
		| Node (MultipleAdj, _, [Leaf adj; _]) -> adj
		| Node (Adjectif, _, [Leaf adj]) -> adj
		| _ -> failwith "get_first_adjectif_of_tree : doesn't match an Leaf adj tree"
	in
	match adj_tree, adj_tree_2 with
	| Node (MultipleAdj, informations_adj_1, _), Node (MultipleAdj, informations_adj_2, _)
	| Node (MultipleAdj, informations_adj_1, _), Node (Adjectif, informations_adj_2, _)
	| Node (Adjectif, informations_adj_1, _), Node (MultipleAdj, informations_adj_2, _)
	| Node (Adjectif, informations_adj_1, _), Node (Adjectif, informations_adj_2, _)
		->	let (success_adjectives, result_informations_adjectives) = check_adjectives informations_adj_1 informations_adj_2 in
			if success_adjectives then
				match det_tree, nom_tree with
				| Node (Determinant, informations_det, [Leaf det]), Node (Nom, informations_nom, [Leaf nom])
					->	let (success_det_noun, result_informations_det_noun) = check_det_noun informations_det informations_nom in
						if success_det_noun then
							let (success_adjectives_noun, result_informations_adjectives_noun) = check_noun_adjective result_informations_det_noun result_informations_adjectives in
							if success_adjectives_noun then
								(true, result_informations_adjectives_noun, No)
							else
								let first_adj_tree_1 = get_first_adjectif_of_tree adj_tree in
								if first_adj_tree_1 = "" then
									(false, [], Accord ((Adjectif, (get_first_adjectif_of_tree adj_tree_2)), (Nom, nom)))
								else
									(false, [], Accord ((Adjectif, first_adj_tree_1), (Nom, nom)))
						else
							(false, [], Accord ((Determinant, det), (Nom, nom)))
				| _ -> failwith "check_nominal_group : Doesn't receive a Determinant and a Nom"
			else
				(false, [], Accord ((Adjectif, (get_first_adjectif_of_tree adj_tree)), (Adjectif, (get_first_adjectif_of_tree adj_tree_2))))
	| _ -> failwith "check_nominal_group : Doesn't receive a correct Adjectif tree"

(* Increasing the index of sentence which is in correcting and return if it receive the expected state *)
let pass sentence =
	sentence.indice <- sentence.indice + 1

(* First call of mutual recursivity for checking *)
let rec get_syntax_tree sentence =
	if sentence.indice > sentence.length then
		Error Empty_sentence
	else
		get_verbal_group sentence

and get_verbal_group s =
	let subject_tree = get_subject s in
	match subject_tree with
	| Error e -> Error e
	| _ ->
		let verb_tree = get_verb s in
		match verb_tree with
		| Error e -> Error e
		| _ ->
			Node (GV, ["Not implemented yet"], [subject_tree; verb_tree])

and get_subject s =
	if s.indice >= s.length then
		Error Empty_sentence
	else
		begin
		if get_word_classe s.t_a.(s.indice) = Determinant then
			let nominal_group_tree = get_nominal_group s in
			match nominal_group_tree with
			| Error e -> Error e
			| Node (GN, informations, _) -> Node (Sujet, informations, [nominal_group_tree])
			| _ -> failwith "get_subject : wrong tree, waiting for a GN"
		else if get_word_classe s.t_a.(s.indice) = Pronom_sujet then
			let pronom_sujet_tree = get_pronom_sujet s in
			match pronom_sujet_tree with
			| Error e -> Error e
			| Node (Pronom_sujet, informations, _) -> Node (Sujet, informations, [pronom_sujet_tree])
			| _ -> failwith "get_subject : wrong tree, waiting for a Pronom_sujet"
		else
			Error (Missing (Sujet, get_word s.t_a.(s.indice)))
		end

and get_nominal_group s =
	let det_tree = get_determinant s in
	match det_tree with
	| Error e -> Error e
	| _ ->
		let adj_tree = get_adjectifs s in
		match adj_tree with
		| Error e -> Error e
		| _ ->
			let nom_tree = get_nom s in
			match nom_tree with
			| Error e -> Error e
			| _ ->
				let adj_tree_2 = get_adjectifs s in
				match adj_tree_2 with
				| Error e -> Error e
				| _ ->
					let (success, informations, error) = check_nominal_group det_tree adj_tree nom_tree adj_tree_2 in
					if success then
						match adj_tree, adj_tree_2 with
						| Node (Adjectif, [], [Leaf ""]), Node (Adjectif, [], [Leaf ""]) -> Node (GN, informations, [det_tree; nom_tree])
						| Node (Adjectif, [], [Leaf ""]), _ -> Node (GN, informations, [det_tree; nom_tree; adj_tree_2])
						| _, Node (Adjectif, [], [Leaf ""]) -> Node (GN, informations, [det_tree; adj_tree; nom_tree])
						| _ -> Node (GN, informations, [det_tree; adj_tree; nom_tree; adj_tree_2])
					else
						Error error

and get_determinant s =
	if s.indice >= s.length then
		Error Empty_sentence
	else
		begin
		let token = s.t_a.(s.indice) in
		match token with
		| Unknown w -> Error ( Unrecognized w )
		| Token (Determinant, (det, informations)) -> pass s; Node (Determinant, informations, [Leaf det])
		| Token (_, (word, _)) -> Error (Missing (Determinant, word))
		end

and get_nom s =
	if s.indice >= s.length then
		Error Empty_sentence
	else
		begin
		let token = s.t_a.(s.indice) in
		match token with
		| Unknown w -> Error ( Unrecognized w )
		| Token (Nom, (nom, informations)) -> pass s; Node (Nom, informations, [Leaf nom])
		| Token (_, (word, _)) -> Error (Missing (Nom, word))
		end

and get_adjectifs s =
	if s.indice >= s.length then
		Error Empty_sentence
	else if get_word_classe s.t_a.(s.indice) = Adjectif then
		let token = s.t_a.(s.indice) in
		if (s.indice + 1) < s.length && get_word_classe s.t_a.(s.indice + 1) = Adjectif then
			match token with
			| Unknown w -> Error ( Unrecognized w )
			| Token (Adjectif, (adj, informations)) ->
				begin
				pass s;
				let next = get_adjectifs s in
				match next with
				| Node (Adjectif, informations_next, l)
				| Node (MultipleAdj, informations_next, l) ->
					let (success, result_informations) = check_adjectives informations informations_next in
						if success then
							Node (MultipleAdj, result_informations, [Node (Adjectif, informations, [Leaf adj]); next])
						else
							Error (Accord ((Adjectif, adj), (Adjectif, get_word s.t_a.(s.indice + 1))))
				| _ -> next
				end
			| Token (_, (word, _)) -> Error (Missing (Adjectif, word))
		else
			match token with
			| Unknown w -> Error ( Unrecognized w )
			| Token (Adjectif, (adj, informations)) -> pass s; Node (Adjectif, informations, [Leaf adj])
			| Token (_, (word, _)) -> Error (Missing (Adjectif, word))
	else
		Node (Adjectif, [], [Leaf ""])

and get_verb s =
	if s.indice >= s.length then
		Error Empty_sentence
	else
		begin
		let token = s.t_a.(s.indice) in
		match token with
		| Unknown w -> Error ( Unrecognized w )
		| Token (Verbe, (verb, informations)) -> pass s; Node (Verbe, informations, [Leaf verb])
		| Token (_, (word, _)) -> Error (Missing (Verbe, word))
		end

and get_pronom_sujet s =
	if s.indice >= s.length then
		Error Empty_sentence
	else
		begin
		let token = s.t_a.(s.indice) in
		match token with
		| Unknown w -> Error ( Unrecognized w )
		| Token (Pronom_sujet, (pronom, informations)) -> pass s; Node (Pronom_sujet, informations, [Leaf pronom])
		| Token (_, (word, _)) -> Error (Missing (Pronom_sujet, word))
		end

let string_to_item s =
	let token_list = sentence_to_token_list s |> all_possibility in
	let rec aux l =
		match l with
		| [] -> []
		| h :: t -> let token_array = h |> array_of_list in { t_a = token_array; indice = 0; length = Array.length token_array } :: aux t
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

let test = In_channel.input_line In_channel.stdin |> string_of_option |> string_to_syntax_tree_list |> syntax_tree_list_in_tex
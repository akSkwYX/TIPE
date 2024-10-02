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
			| Node of bool * (char * trie) list
			| Leaf of bool

		let trie_create = Leaf false

		let rec trie_insert trie word =
			match trie with
			| Leaf b when word = "" -> Leaf true
			| Leaf b -> trie_insert (Node (b, [(word.[0], Leaf false)])) word
			| Node (b, list) when word = "" -> Node (true, list)
			| Node (b, list) ->
				let rec aux l head =
					match l with
					| [] -> Node ( b, (word.[0], ( trie_insert trie_create (word_without_first_char word) ) )::list )
					| (c, t) :: tail when c = word.[0] -> Node ( b, ( (c, trie_insert t (word_without_first_char word)) :: head @ tail ) )
					| h :: tail -> aux tail (h :: head)
				in aux list []
				
		let rec trie_search trie word  =
			match trie with
			| Leaf b when word = "" -> b
			| Leaf b -> false
			| Node (b, l) when word = "" -> b
			| Node (b, l) -> 
				let rec aux l =
					match l with
					| [] -> false
					| (c, t) :: tl -> if c = word.[0] then trie_search t (word_without_first_char word) else aux tl
				in aux l

		let rec print_trie trie =
			match trie with
			| Leaf b -> b |> string_of_bool |> print_string; print_string " "; print_newline ()
			| Node (b, l) ->
				b |> string_of_bool |> print_string; print_string " ";
				let rec aux l =
					match l with
					| [] -> print_newline ()
					| (c, t) :: tl -> print_char c; print_string " "; print_trie t; aux tl
				in aux l

	end

(* ---------------------------------------------------------------------------------------------------------------------------------------------------------------- *)

let read_dictionnary file =
	let file = open_in file in
	let rec read_lines trie =
		let line = In_channel.input_line file in
		match line with
		| None -> trie
		| Some word -> read_lines (Trie.trie_insert trie (word_without_last_char word))
	in
	read_lines Trie.trie_create

let trie = read_dictionnary "francais.txt"

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
	| h :: t -> let is_word = Trie.trie_search trie h in
						  is_word :: (orthographic_correction t)

let rec verify_sentence l =
	match l with
	| [] -> true
	| h :: t -> if h then verify_sentence t else false

let option_to_string o =
	match o with
	| None -> failwith "option_to_string : None"
	| Some x -> x

let orthographic_checker =
	In_channel.input_line In_channel.stdin |> option_to_string |> sentence_to_list |> orthographic_correction |> verify_sentence |> print_bool

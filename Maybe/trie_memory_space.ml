let word_without_first_char word =
	String.sub word 1 (String.length word - 1)

let word_without_last_char word =
	String.sub word 0 (String.length word - 1)

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
			| Leaf b -> b
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
let () = "abaissezz" |> Trie.trie_search trie |> string_of_bool |> print_endline
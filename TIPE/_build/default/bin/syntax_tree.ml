type syntax_tree =
	| Node of Word_classe.word_classe * string list * syntax_tree list
	| Leaf of string
	| Error of Checkings.error
	| Empty

let get_children = function
	| Node (_, _, children) -> children
	| _ -> []

let get_word_classe = function
	| Node (word_classe, _, _) -> word_classe
	| _ -> Word_classe.Unknown

let get_information = function
	| Node (_, information, _) -> information
	| _ -> []

let print_syntax_tree tree =
	let rec print_tree tree depth =
		let rec print_depth depth =
			if depth = 0 then ()
			else (print_string "  "; print_depth (depth - 1))
		in
		let rec print_information information =
			match information with
			| [] -> ()
			| head :: tail -> print_string head; print_string " "; print_information tail
		in
		match tree with
		| Node (word_classe, information, children) ->
			print_depth depth;
			print_string (Word_classe.word_class_to_short_string word_classe);
			print_string " ";
			print_information information;
			print_newline ();
			List.iter (fun child -> print_tree child (depth + 1)) children
		| Leaf (word) ->
			print_depth depth;
			print_string word;
			print_newline ()
		| Error (error) ->
			print_depth depth;
			print_string (Checkings.string_of_error error);
			print_newline ()
		| Empty ->
      print_depth depth;
      print_string "Empty";
	in
	print_tree tree 0

let distinct tree_list = List.fold_left (fun acc x -> if List.mem x acc then acc else x :: acc) [] tree_list
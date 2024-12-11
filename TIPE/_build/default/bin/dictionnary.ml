let dictionnary_path = "dictionnarys/Dictionnary.txt"

(** 
  Reads a dictionary file and returns two tries: one where lines are inserted based on the first word and the other one on the second word.

  @param file The path to the dictionary file.
  @return A tuple of two tries: the first trie is indexed by the first word in each line, and the second trie is indexed by the second word in each line.

  The dictionary file is expected to have lines with words separated by commas. The function processes the lines and inserts them into the tries based on specific patterns.

  The function raises an exception if:
  - The input list is empty.
  - A line in the dictionary file is empty.
  - A line in the dictionary file has a wrong format.
*)
let read_dictionnary file =
	let aux (s:string list) :string list =
    let do_print = ref false in
		let rec aux2 current_string_list result =
			match current_string_list with
			| [] -> List.rev result
			| ("Ip" as first_possibility) :: ("Is" as other_possibility) :: t
			| ("Ip" as first_possibility) :: ("Iq" as other_possibility) :: t
			| ("Ip" as first_possibility) :: ("If" as other_possibility) :: t
			| ("Ip" as first_possibility) :: ("Sp" as other_possibility) :: t
			| ("Ip" as first_possibility) :: ("Sq" as other_possibility) :: t
			| ("Is" as first_possibility) :: ("Ip" as other_possibility) :: t
			| ("Is" as first_possibility) :: ("Iq" as other_possibility) :: t
			| ("Is" as first_possibility) :: ("If" as other_possibility) :: t
			| ("Is" as first_possibility) :: ("Sp" as other_possibility) :: t
			| ("Is" as first_possibility) :: ("Sq" as other_possibility) :: t
			| ("Iq" as first_possibility) :: ("Ip" as other_possibility) :: t
			| ("Iq" as first_possibility) :: ("Is" as other_possibility) :: t
			| ("Iq" as first_possibility) :: ("If" as other_possibility) :: t
			| ("Iq" as first_possibility) :: ("Sp" as other_possibility) :: t
			| ("Iq" as first_possibility) :: ("Sq" as other_possibility) :: t
			| ("If" as first_possibility) :: ("Ip" as other_possibility) :: t
			| ("If" as first_possibility) :: ("Is" as other_possibility) :: t
			| ("If" as first_possibility) :: ("Iq" as other_possibility) :: t
			| ("If" as first_possibility) :: ("Sp" as other_possibility) :: t
			| ("If" as first_possibility) :: ("Sq" as other_possibility) :: t
			| ("Sp" as first_possibility) :: ("Ip" as other_possibility) :: t
			| ("Sp" as first_possibility) :: ("Sq" as other_possibility) :: t
			| ("1s" as first_possibility) :: ("2s" as other_possibility) :: t
			| ("1s" as first_possibility) :: ("3s" as other_possibility) :: t
				-> aux2 t ((first_possibility ^ "," ^ other_possibility) :: result)
			| ("Ip,Sp" as first_possibility) :: ("Sq" as other_possibility) :: t
			| ("Iq,Sp" as first_possibility) :: ("Sq" as other_possibility) :: t
				-> aux2 t ((first_possibility ^ "," ^ other_possibility) :: result)
			| h::t -> aux2 t (h::result)
		in
		match s with
		| [] -> failwith "Dictionnary - read_dictionnary - aux : empty list"
    | "travaille"::_::"V"::t -> do_print := true; aux2 s []
		| _::_::"V"::t -> aux2 s []
		| _ -> s
	in
	let file = open_in file in
  (* Reads the file and return 2 trie, one where lines are inserted based on the first word and the other one on the second word*)
	let rec read_lines (trie1, trie2) =
		let line = In_channel.input_line file |> Option.map (String.split_on_char ',') |> Option.map aux in
		match line with
		| None -> (trie1, trie2)
		| Some (word::radical::information) -> read_lines ( (Trie.trie_insert trie1 word (radical::information) ), (Trie.trie_insert trie2 radical (word::information) ) )
		| Some [] -> failwith "read_dictionnary : empty line"
    | Some _ -> failwith "read_dictionnary : wrong format"
	in
	read_lines (Trie.trie_create, Trie.trie_create)

let dictionnary, rad_dictionnary = read_dictionnary dictionnary_path
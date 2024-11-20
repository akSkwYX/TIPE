let dictionnary_path = "dictionnarys/Dictionnary.txt"

let read_dictionnary file =
	(* Transform the line received in the correct format of informations *)
	let aux (s:string) :string list =
		let rec aux2 s l =
			match s with
			| [] -> List.rev l
			| _ -> 
				let rec find_word l w length_w =
					match l with
					| [] -> w, length_w
					| ',' :: t -> w, length_w
					| c :: t -> find_word t (c::w) (length_w + 1)
				in
				let char_list, length_w = find_word s [] 0 in
				let w = List.rev char_list |> Utility.string_of_char_list in
				match l, w with
				| ("Ip" as first_possibility) :: t, ("Is" as other_possibility)
				| ("Ip" as first_possibility) :: t, ("Iq" as other_possibility)
				| ("Ip" as first_possibility) :: t, ("If" as other_possibility)
				| ("Ip" as first_possibility) :: t, ("Sp" as other_possibility)
				| ("Ip" as first_possibility) :: t, ("Sq" as other_possibility)
				| ("Is" as first_possibility) :: t, ("Ip" as other_possibility)
				| ("Is" as first_possibility) :: t, ("Iq" as other_possibility)
				| ("Is" as first_possibility) :: t, ("If" as other_possibility)
				| ("Is" as first_possibility) :: t, ("Sp" as other_possibility)
				| ("Is" as first_possibility) :: t, ("Sq" as other_possibility)
				| ("Iq" as first_possibility) :: t, ("Ip" as other_possibility)
				| ("Iq" as first_possibility) :: t, ("Is" as other_possibility)
				| ("Iq" as first_possibility) :: t, ("If" as other_possibility)
				| ("Iq" as first_possibility) :: t, ("Sp" as other_possibility)
				| ("Iq" as first_possibility) :: t, ("Sq" as other_possibility)
				| ("If" as first_possibility) :: t, ("Ip" as other_possibility)
				| ("If" as first_possibility) :: t, ("Is" as other_possibility)
				| ("If" as first_possibility) :: t, ("Iq" as other_possibility)
				| ("If" as first_possibility) :: t, ("Sp" as other_possibility)
				| ("If" as first_possibility) :: t, ("Sq" as other_possibility)
				| ("Sp" as first_possibility) :: t, ("Ip" as other_possibility)
				| ("Sp" as first_possibility) :: t, ("Sq" as other_possibility)
				| ("1s" as first_possibility) :: t, ("2s" as other_possibility)
				| ("1s" as first_possibility) :: t, ("3s" as other_possibility)
					-> aux2 (Utility.list_without_x_last_char (length_w+1) s) ((first_possibility ^ "," ^ other_possibility) :: t)
				| ("Ip,Sp" as first_possibility :: t), ("Sq" as other_possibility)
				| ("Iq,Sp" as first_possibility :: t), ("Sq" as other_possibility)
					-> aux2 (Utility.list_without_x_last_char (length_w+1) s) ((first_possibility ^ "," ^ other_possibility) :: t)
				| _ -> aux2 (Utility.list_without_x_last_char (length_w+1) s) (w :: l)
		in
		aux2 (Utility.char_list_of_string s) []
	in
	let file = open_in file in
  (* Reads the file and return 2 trie, one where lines are inserted based on the first word and the other one on the second word*)
	let rec read_lines (trie1, trie2) =
		let line = In_channel.input_line file |> Option.map aux in
		match line with
		| None -> (trie1, trie2)
		| Some (word::radical::information) -> read_lines ( (Trie.trie_insert trie1 word (radical::information) ), (Trie.trie_insert trie2 radical (word::information) ) )
		| Some [] -> failwith "read_dictionnary : empty line"
    | Some _ -> failwith "read_dictionnary : wrong format"
	in
	read_lines (Trie.trie_create, Trie.trie_create)

let dictionnary, rad_dictionnary = read_dictionnary dictionnary_path
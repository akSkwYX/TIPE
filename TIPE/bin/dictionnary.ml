let dictionnary_path = "dictionnarys/linux_dictionnary.txt"

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
	let aux s =
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
		| _::_::_::"V"::t -> aux2 s []
		| _ -> s
	in

	let file = open_in file in
	let rec read_lines (trie1, trie2) =
		let line = In_channel.input_line file |> Option.map (String.split_on_char ',') |> Option.map aux in
		match line with
		| None -> (trie1, trie2)
		| Some ("#"::_) -> read_lines (trie1, trie2)
		| Some (frequency::word::radical::information) -> read_lines ( (Trie.trie_insert trie1 word (radical::information) (int_of_string frequency)), (Trie.trie_insert trie2 radical (word::information) (int_of_string frequency)) )
		| Some [] -> failwith "read_dictionnary : empty line"
    | Some _ -> failwith "read_dictionnary : wrong format"
	in
	read_lines (Trie.trie_create, Trie.trie_create)

let update_dictionnary string_list =
  let file_to_read = In_channel.open_bin dictionnary_path in
	let rec get_list_line_to_change res i file_content =
		let line = In_channel.input_line file_to_read in
		match line with
    | None -> In_channel.close file_to_read; (res, file_content)
    | Some s ->
      begin
        if List.exists ((=) (Utility.string_without_x_first_char 2 s)) string_list then
					begin
						let k = String.index s ',' in
          	get_list_line_to_change ((int_of_string (String.sub s 0 k), i) :: res) (i+1) (s::file_content)
					end
				else
          get_list_line_to_change res (i+1) (s::file_content)
      end
  in
  let (line_to_change, file_content) = get_list_line_to_change [] 0 [] in
  let file_to_write = Out_channel.open_bin dictionnary_path in
  let rec rewrite_file i line_to_change file_content =
    match file_content with
    | [] -> Out_channel.close file_to_write
    | h :: t ->
      let freq = List.fold_left (fun acc (freq, x) -> if x = i then freq else acc) (-1) line_to_change in
      if freq = -1 then
        begin
          Out_channel.output_string file_to_write (h ^ "\n"); rewrite_file (i+1) line_to_change t
        end
      else
        begin
          let s = string_of_int (freq + 1) ^ (Utility.string_without_x_first_char (String.length @@ string_of_int (freq + 1)) h) ^ "\n" in
          Out_channel.seek file_to_write Int64.zero; Out_channel.output_string file_to_write s; rewrite_file (i+1) line_to_change t
        end
  in
  rewrite_file 0 line_to_change file_content

let dictionnary, rad_dictionnary = read_dictionnary dictionnary_path
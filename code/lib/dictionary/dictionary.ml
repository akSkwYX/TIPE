let dictionary_path = "dictionarys/linux_dictionary_eschyle.txt"

let read_dictionary file =
  let dictionary = In_channel.open_bin file in
  In_channel.fold_lines (fun (trie, trie_rad) string_line -> 
    let list_line = String.split_on_char ',' string_line in
    match list_line with
    | frequency :: word :: rad_word :: tags -> 
      let trie = Trie.insert trie word (frequency::rad_word::tags) in
      let trie_rad = Trie.insert trie_rad rad_word (frequency::word::tags) in
      (trie, trie_rad)
    | _ -> (trie, trie_rad)
  ) (Trie.create, Trie.create) dictionary

let find dictionary word =
  Trie.search dictionary word

let update_dictionary line_list =
  let dictionary_in = In_channel.open_bin dictionary_path in
  let string_list = In_channel.input_lines dictionary_in in
  let dictionary_out = Out_channel.open_text dictionary_path in
  List.iter (
    fun s ->
      let c = Utility.count ((=) s) line_list in
      if c <> 0 then
        let s_list = String.split_on_char ',' s in
        let s' =
          match s_list with
          | h :: t -> String.concat "," ( (string_of_int (int_of_string h + c)) :: t )
          | [] -> failwith "dictionary.ml/update_dictionary : Empty string"
        in
        Out_channel.output_string dictionary_out (s' ^ "\n")
      else
        Out_channel.output_string dictionary_out (s ^ "\n")
  ) string_list

let dictionary, rad_dictionary = read_dictionary dictionary_path

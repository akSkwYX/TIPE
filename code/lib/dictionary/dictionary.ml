let dictionary_path = "dictionarys/linux_dictionary.txt"

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

let dictionary, rad_dictionary = read_dictionary dictionary_path

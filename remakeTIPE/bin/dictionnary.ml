let dictionnary_path = "dictionnarys/linux_dictionnary.txt"

let read_dictionnary file =
  let dictionnary = In_channel.open_bin file in
  In_channel.fold_lines (fun (trie, trie_rad) string_line -> 
    let list_line = String.split_on_char ',' string_line in
    match list_line with
    | frequency :: word :: rad_word :: tags -> 
      let trie = Trie.insert trie word (frequency::rad_word::tags) in
      let trie_rad = Trie.insert trie_rad rad_word (frequency::word::tags) in
      (trie, trie_rad)
    | _ -> (trie, trie_rad)
  ) (Trie.create, Trie.create) dictionnary

let find dictionnary word =
  Trie.search dictionnary word

let dictionnary, rad_dictionnary = read_dictionnary dictionnary_path

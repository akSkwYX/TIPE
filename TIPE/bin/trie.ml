type trie =
| Node of (bool * (string list) list) * (char * trie) list
| Leaf of bool * (string list) list

(** [trie_create] is a trie data structure initialized as a leaf node.
  The leaf node is represented by a tuple containing a boolean and an empty list.
  The boolean indicates whether the node represents the end of a word.
*)
let trie_create = Leaf (false, [])

(** 
  Recursively inserts a word and its associated information into a trie.

  @param trie The trie into which the word and information will be inserted.
  @param word The word to be inserted into the trie.
  @param information The information to be associated with the word in the trie.

  @return A new trie with the word and information inserted.

  The function works as follows:
  - If the current trie node is a Leaf and the word is empty, it adds the information to the Leaf.
  - If the current trie node is a Leaf but the word is not empty, it creates a new Node with the remaining letters of the word.
  - If the current trie node is a Node and the word is empty, it adds the information to the Node.
  - If the current trie node is a Node and the word is not empty, it recursively inserts the remaining letters of the word into the appropriate child node.
*)
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
      | [] -> Node ( (b,i), (word.[0], ( trie_insert trie_create (Utility.word_without_first_char word) information) )::list )
      | (c, t) :: tail when c = word.[0] -> Node ( (b,i), ( (c, trie_insert t (Utility.word_without_first_char word) information) :: head @ tail ) )
      | h :: tail -> aux tail (h :: head)
    in aux list []
    

(** 
  [trie_search trie word] searches for the given [word] in the [trie].
  @param trie The trie data structure to search within.
  @param word The word to search for in the trie.
  @return A tuple [(bool, list)] where the boolean indicates if the word is found,
      and the list contains additional information associated with the word.
  @example
  let trie = Node ((false, []), [('a', Leaf (true, [1]))])
  let result = trie_search trie "a"
  (* result is (true, [1]) *)
*)
let rec trie_search trie word  =
  match trie with
  | Leaf (b,i) when word = "" -> (b,i)
  | Leaf (b,i) -> (false, [])
  | Node ((b,i), l) when word = "" -> (b,i)
  | Node ((b,i), l) -> 
    let rec aux l =
      match l with
      | [] -> (false, [])
      | (c, t) :: tl -> if c = word.[0] then trie_search t (Utility.word_without_first_char word) else aux tl
    in aux l


(** 
  [print_trie_node (b, l)] prints a representation of a trie node.
  The node is represented as a tuple [(b, l)], where [b] is a boolean
  and [l] is a list of lists of strings. The boolean [b] is printed
  first, followed by a space, and then each list of strings in [l] is
  printed on a new line.

  @param (b,l) a tuple containing a boolean and a list of lists of strings
*)
let print_trie_node (b, l) =
  b |> string_of_bool |> print_string; print_string " ";
  let rec aux l =
    match l with
    | [] -> print_newline ()
    | h :: t -> Utility.print_string_list h; aux t
  in aux l
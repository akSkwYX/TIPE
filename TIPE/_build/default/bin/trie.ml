type trie =
| Node of (bool * (string list) list) * (char * trie) list
| Leaf of bool * (string list) list

let trie_create = Leaf (false, [])

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
    
(* Just browse the trie but this have a fcking good complexity *)
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

(* Just a print function for printing a node, quite useless maybe remove *)
let print_trie_node (b, l) =
  b |> string_of_bool |> print_string; print_string " ";
  let rec aux l =
    match l with
    | [] -> print_newline ()
    | h :: t -> Utility.print_string_list h; aux t
  in aux l
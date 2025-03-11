(**
  Root of children
  Node of tags list * children
  Leaf of tags list
*)
type trie =
  | Root of (char * trie) list
  | Node of (string list list) * (char * trie) list
  | Leaf of string list list

(** Create an empty trie *)
let create = Root []

(**
  Insert an element in a trie.

  insert_child recursively iterate on the children of a node to insert the word in
  the right branch.
*)
let rec insert trie word tags =
  let rec insert_child children word tags =
    match children with
    | [] -> [(word.[0], insert (Leaf []) (Utility.string_tail word) tags)]
    | (c, t) :: q ->
      if c = word.[0] then
        (c, insert t (Utility.string_tail word) tags) :: q 
      else 
        (c, t) :: insert_child q word tags
  in
  match trie with
  | Leaf (list) ->
    if word = "" then
      Leaf (tags :: list)
    else
      Node (list, [(word.[0], insert (Leaf []) (Utility.string_tail word) tags)])
  | Node (list, children) ->
    if word = "" then
      Node (tags :: list, children)
    else
      Node (list, insert_child children word tags)
  | Root children ->
    if word = "" then
      Root children
    else
      Root (insert_child children word tags)

(**
   Return the tags of a word in a trie : Empty list if the word is not in the trie.

   search_child recursively iterate on the children of a node to find the good branch.
*)
let rec search trie word =
  let rec search_child children word =
    match children with
    | [] -> []
    | (c, t) :: q ->
      if c = word.[0] then
        search t (Utility.string_tail word)
      else
        search_child q word
  in
  match trie with
  | Leaf list ->
    if word = "" then
      list
    else
      []
  | Node (tags, children) ->
    if word = "" then
      tags
    else
      search_child children word
  | Root children ->
    if word = "" then
      []
    else
      search_child children word

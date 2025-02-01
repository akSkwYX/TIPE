type trie =
| Node of (bool * (int * string list) list) * (char * trie) list
| Leaf of bool * (int * string list) list

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
let rec trie_insert trie word information frequency =
  match trie with
  (* Finding word ending on a Leaf so just adding a Leaf *)
  | Leaf (b,i) when word = "" -> Leaf (true, (frequency, information)::i)
  (* Reaching a Leaf but word isn't end so adding all remain letter in the trie *)
  | Leaf (b,i) -> trie_insert (Node ((b,i), [(word.[0], Leaf (false, []))])) word information frequency
  (* End of word reached inside the trie so just adding information to the node of the word *)
  | Node ((b,i), list) when word = "" -> Node ((true, (frequency, information)::i), list)
  (* Reccursive inserting *)
  | Node ((b,i), list) ->
    let rec aux l head =
      match l with
      | [] -> Node ( (b,i), (word.[0], ( trie_insert trie_create (Utility.string_without_first_char word) information frequency) )::list )
      | (c, t) :: tail when c = word.[0] -> Node ( (b,i), ( (c, trie_insert t (Utility.string_without_first_char word) information frequency) :: head @ tail ) )
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
      | (c, t) :: tl -> if c = word.[0] then trie_search t (Utility.string_without_first_char word) else aux tl
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
    | (freq, info) :: t -> print_int freq; print_string " "; Utility.print_string_list info; print_string " ; "; aux t
  in aux l

let rec print_root_trie trie =
  match trie with
  | Node ((b,i), l) -> print_string (string_of_bool b); print_string "; "; List.iter (fun (freq, info) -> print_int freq; print_string ", "; Utility.print_string_list info) i; print_newline (); List.iter (fun (c, t) -> print_char c; print_string ", ") l; print_newline()
  | Leaf (b, i) -> print_string (string_of_bool b); print_string "; "; List.iter (fun (freq, info) -> print_int freq; print_string ", "; Utility.print_string_list info) i; print_newline ()


(* USELESS SERIALIZATION *)


let format_key ((b, l):bool * (int * string list) list) :string =
  let s = if b then "t" else "f" in
  s ^ List.fold_left (fun acc (freq, info) -> acc ^ "(" ^ (string_of_int freq) ^ " " ^ (String.concat "," info) ^ ") " ) " " l

let deformat_key (s:string) :bool * (int * string list) list =
  let b = s.[0] = 't' in
  if not b then (b, [])
  else
    let l = String.sub s 2 (String.length s - 2) in
    let rec aux l =
      match l with
      | "" -> []
      | _ -> 
        let i = String.index l '(' in
        let j = String.index l ')' in
        let freq = int_of_string (String.sub l (i+1) 1) in
        let info = String.split_on_char ',' (String.sub l (i+3) (j-i-3)) in
        if j = String.length l - 1 then [(freq, info)]
        else (freq, info) :: aux (String.sub l (j+2) (String.length l - j - 2))
    in
    (b, aux l)

let number_of_children trie =
  match trie with
  | Node (_, l) -> List.length l
  | Leaf (_, _) -> 0

let serialize_trie trie =
  let file = open_out "dictionnarys/serialized_trie.txt" in
  let rec aux (char, trie) =
    match trie with
    | Node ((b, i), l) ->
      begin
        output_string file (String.make 1 char ^ " [" ^ format_key (b, i) ^ "| " ^ string_of_int (number_of_children trie) ^ "]\n");
        List.iter aux l
      end
    | Leaf (b, i) -> output_string file (String.make 1 char ^ " [" ^ format_key (b, i) ^ "| " ^ "0" ^ "]\n")
  in
  aux ('R', trie)

let deserialize_trie () =
  let file = open_in "dictionnarys/serialized_trie.txt" in
  let rec aux line =
    let char = line.[0] in
    let i = String.index line '[' in
    let j = String.index line ']' in
    let k = String.index line '|' in
    let (b, i) = deformat_key (String.sub line (i+1) (k-i-2)) in
    let nbr_of_children = int_of_string (String.sub line (k+2) (j-k-2)) in
    if nbr_of_children = 0 then
      (char, Leaf (b, i))
    else
      begin
        let rec aux2 i res =
          if i = 0 then res
          else
            aux2 (i-1) ((aux (input_line file)) :: res)
        in
        (char, Node ((b, i), aux2 nbr_of_children []))
      end
  in
  aux (input_line file)
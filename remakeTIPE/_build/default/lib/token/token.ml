type token =
  | SENTENCE of string * string list
  | VERBAL_GROUP of string * string list
  | NOMINAL_GROUP of string * string list
  | SUBJECT of string * string list
  | VERB of string * string list
  | NOUN of string * string list
  | ADJECTIVE of string * string list
  | DETERMINER of string * string list
  | PERSONAL_PRONOUN_SUBJECT of string * string list
  | EPSILON
  | UNKNOWN of string

let get_word token =
  match token with
  | SENTENCE (word, _) -> word
  | VERBAL_GROUP (word, _) -> word
  | NOMINAL_GROUP (word, _) -> word
  | SUBJECT (word, _) -> word
  | VERB (word, _) -> word
  | NOUN (word, _) -> word
  | ADJECTIVE (word, _) -> word
  | DETERMINER (word, _) -> word
  | PERSONAL_PRONOUN_SUBJECT (word, _) -> word
  | EPSILON -> ""
  | UNKNOWN (word) -> word

let get_tags token =
  match token with
  | SENTENCE (_, tags) -> tags
  | VERBAL_GROUP (_, tags) -> tags
  | NOMINAL_GROUP (_, tags) -> tags
  | SUBJECT (_, tags) -> tags
  | VERB (_, tags) -> tags
  | NOUN (_, tags) -> tags
  | ADJECTIVE (_, tags) -> tags
  | DETERMINER (_, tags) -> tags
  | PERSONAL_PRONOUN_SUBJECT (_, tags) -> tags
  | EPSILON -> []
  | UNKNOWN _ -> []

let get_word_class token =
  match token with
  | SENTENCE _ -> "S"
  | VERBAL_GROUP _ -> "VG"
  | NOMINAL_GROUP _ -> "NG"
  | SUBJECT _ -> "Su"
  | VERB _ -> "V"
  | NOUN _ -> "N"
  | ADJECTIVE _ -> "A"
  | DETERMINER _ -> "D"
  | PERSONAL_PRONOUN_SUBJECT _ -> "Os"
  | EPSILON -> "epsilon"
  | UNKNOWN _ -> "unknown"

let create word_class word tags =
  match word_class with
  | "S" -> SENTENCE (word, tags)
  | "VG" -> VERBAL_GROUP (word, tags)
  | "NG" -> NOMINAL_GROUP (word, tags)
  | "Su" -> SUBJECT (word, tags)
  | "V" -> VERB (word, tags)
  | "N" -> NOUN (word, tags)
  | "A" -> ADJECTIVE (word, tags)
  | "D" -> DETERMINER (word, tags)
  | "Os" -> PERSONAL_PRONOUN_SUBJECT (word, tags)
  | "epsilon" -> EPSILON
  | _ -> UNKNOWN word

let format_tags token =
  List.fold_left (fun acc tag -> match tag with | "_" -> acc ^ ", \\_" | s -> acc ^ ", " ^ s) "" (get_tags token)

(** Print a token *)
let print_token token =
  Printf.printf "%s : %s : %s\n" (get_word_class token) (get_word token) (String.concat ", " (get_tags token))

(**
  Split a sentence into a list of words on spaces and apostrophes.

  Iterating on all character of string, keeping in the acc the list of already
  seen word and the word which is currently being iterate.
  When encounter a " " or "\'" add the word to the list in the acc and reset
  the current word
*)
let sentence_to_string_list s =
  String.fold_left (fun (res, word) c -> match c with
    | ' ' -> (word :: res, "")
    | '\'' -> ((word ^ "\'") :: res, "")
    | _ -> (res, word ^ (String.make 1 c))
  ) ([], "") s |> (fun (res, word) -> word :: res)

(**
  Convert a word and its tags into a token.
*)
let word_tags_to_token word tags =
  let word_class = Tags.get_word_class tags in
  match word_class with
  | "V" -> VERB (word, tags)
  | "N" -> NOUN (word, tags)
  | "A" -> ADJECTIVE (word, tags)
  | "D" -> DETERMINER (word, tags)
  | "Os" -> PERSONAL_PRONOUN_SUBJECT (word, tags)
  | _ -> UNKNOWN word

(**
  Make a token list from a string (a word of the sentence). 

  NB : It's a list of token because a word can have multiple sense so 
  multiple tags so multiple token.
*)
let string_to_token_list s =
  let tags_list = Dictionary.find Dictionary.dictionary s in
  let rec make_tokens tags_list token_list =
    match tags_list with
    | [] -> token_list
    | h :: t -> make_tokens t (word_tags_to_token s h :: token_list)
  in
  make_tokens tags_list []

(**
  Make a list of token list (each word have multiple token) from
  a string (sentence).
*)
let sentence_to_token_list_list s =
  s |> sentence_to_string_list |> List.rev_map string_to_token_list

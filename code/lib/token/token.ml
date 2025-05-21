type token =
  | SENTENCE of string * string list
  | VERBAL_GROUP of string * string list
  | NOMINAL_GROUP of string * string list
  | COD of string * string list
  | COI of string * string list
  | AUX_VERB of string * string list
  | SUBJECT of string * string list
  | VERB of string * string list
  | PRE_VERB_PRONOUN of string * string list
  | NOUN of string * string list
  | PROPER_NOUN of string * string list
  | PREPOSITION of string * string list
  | ADJECTIVE of string * string list
  | DETERMINER of string * string list
  | PERSONAL_PRONOUN_SUBJECT of string * string list
  | EPSILON
  | UNKNOWN of string
  | TEMP of string * string list (* should not appear in any case in the final result *)

let is_equal t1 t2 =
  match t1, t2 with
  | SENTENCE (word1, tags1), SENTENCE (word2, tags2)
  | VERBAL_GROUP (word1, tags1), VERBAL_GROUP (word2, tags2)
  | NOMINAL_GROUP (word1, tags1), NOMINAL_GROUP (word2, tags2)
  | COD (word1, tags1), COD (word2, tags2)
  | COI (word1, tags1), COI (word2, tags2)
  | AUX_VERB (word1, tags1), AUX_VERB (word2, tags2)
  | SUBJECT (word1, tags1), SUBJECT (word2, tags2)
  | VERB (word1, tags1), VERB (word2, tags2)
  | NOUN (word1, tags1), NOUN (word2, tags2)
  | PROPER_NOUN (word1, tags1), PROPER_NOUN (word2, tags2)
  | PREPOSITION (word1, tags1), PREPOSITION (word2, tags2)
  | PRE_VERB_PRONOUN (word1, tags1), PRE_VERB_PRONOUN (word2, tags2)
  | ADJECTIVE (word1, tags1), ADJECTIVE (word2, tags2)
  | DETERMINER (word1, tags1), DETERMINER (word2, tags2)
  | PERSONAL_PRONOUN_SUBJECT (word1, tags1), PERSONAL_PRONOUN_SUBJECT (word2, tags2) ->
    String.equal word1 word2 && Tags.is_equal tags1 tags2
  | EPSILON, EPSILON -> true
  | UNKNOWN w1, UNKNOWN w2 -> String.equal w1 w2
  | _ -> false

let get_word token =
  match token with
  | SENTENCE (word, _)
  | VERBAL_GROUP (word, _)
  | NOMINAL_GROUP (word, _)
  | COD (word, _)
  | COI (word, _)
  | AUX_VERB (word, _)
  | SUBJECT (word, _)
  | VERB (word, _)
  | NOUN (word, _)
  | PROPER_NOUN (word, _)
  | PREPOSITION (word, _)
  | PRE_VERB_PRONOUN (word, _)
  | ADJECTIVE (word, _)
  | DETERMINER (word, _)
  | PERSONAL_PRONOUN_SUBJECT (word, _)
  | UNKNOWN (word)
  | TEMP (word, _) -> word
  | EPSILON -> ""

let get_tags token =
  match token with
  | SENTENCE (_, tags)
  | VERBAL_GROUP (_, tags)
  | NOMINAL_GROUP (_, tags)
  | COD (_, tags)
  | COI (_, tags)
  | AUX_VERB (_, tags)
  | SUBJECT (_, tags)
  | VERB (_, tags)
  | NOUN (_, tags)
  | PROPER_NOUN (_, tags)
  | PREPOSITION (_, tags)
  | PRE_VERB_PRONOUN (_, tags)
  | ADJECTIVE (_, tags)
  | DETERMINER (_, tags)
  | PERSONAL_PRONOUN_SUBJECT (_, tags)
  | TEMP (_, tags) -> tags
  | EPSILON -> []
  | UNKNOWN _ -> []

let get_word_class token =
  match token with
  | SENTENCE _ -> "S"
  | VERBAL_GROUP _ -> "VG"
  | NOMINAL_GROUP _ -> "NG"
  | COD _ -> "COD"
  | COI _ -> "COI"
  | AUX_VERB _ -> "AuxV"
  | SUBJECT _ -> "Su"
  | VERB _ -> "V"
  | NOUN _ -> "N"
  | PROPER_NOUN _ -> "M"
  | PREPOSITION _ -> "R"
  | PRE_VERB_PRONOUN _ -> "Ov"
  | ADJECTIVE _ -> "A"
  | DETERMINER _ -> "D"
  | PERSONAL_PRONOUN_SUBJECT _ -> "Os"
  | EPSILON -> "epsilon"
  | UNKNOWN _ -> "unknown"
  | TEMP _ -> "temp"

let create word_class word tags =
  match word_class with
  | "S" -> SENTENCE (word, tags)
  | "VG" -> VERBAL_GROUP (word, tags)
  | "NG" -> NOMINAL_GROUP (word, tags)
  | "COD" -> COD (word, tags)
  | "COI" -> COI (word, tags)
  | "AuxV" -> AUX_VERB (word, tags)
  | "Su" -> SUBJECT (word, tags)
  | "V" -> VERB (word, tags)
  | "N" -> NOUN (word, tags)
  | "M" -> PROPER_NOUN (word, tags)
  | "R" -> PREPOSITION (word, tags)
  | "Ov" -> PRE_VERB_PRONOUN (word, tags)
  | "A" -> ADJECTIVE (word, tags)
  | "D" -> DETERMINER (word, tags)
  | "Os" -> PERSONAL_PRONOUN_SUBJECT (word, tags)
  | "epsilon" -> EPSILON
  | "temp" -> TEMP (word, tags)
  | _ -> UNKNOWN word

let format_tags token =
  List.fold_left (fun acc tag -> match tag with | "_" -> acc ^ ", \\_" | s -> acc ^ ", " ^ s) "" (get_tags token)

(** Print a token *)
let print_token token =
  Printf.printf "%s : %s : %s\n" (get_word_class token) (get_word token) (String.concat ", " (get_tags token))

let token_to_line token =
  match get_tags token with
  | frequency :: rad_word :: tags -> String.concat "," (frequency :: (get_word token) :: rad_word :: tags)
  | _ -> failwith "token.ml/token_to_line : Fail"

let frequencyDistance t1 t2 =
  abs (Tags.get_frequency (get_tags t1) - Tags.get_frequency (get_tags t2))

(**
  Convert a word and its tags into a token.
*)
let word_tags_to_token word tags =
  let word_class = Tags.get_word_class tags in
  match word_class with
  | "V" -> VERB (word, tags)
  | "N" -> NOUN (word, tags)
  | "M" -> PROPER_NOUN (word, tags)
  | "R" -> PREPOSITION (word, tags)
  | "A" -> ADJECTIVE (word, tags)
  | "D" -> DETERMINER (word, tags)
  | "Os" -> PERSONAL_PRONOUN_SUBJECT (word, tags)
  | "Ov" -> PRE_VERB_PRONOUN (word, tags)
  | _ -> UNKNOWN word 

(**
Make a token list from a string (a word of the sentence).
NB : It's a list of token because a word can have multiple sense so multiple tags so multiple token. *)
let rec make_tokens_from_tags_list word token_list tags_list  =
  match tags_list with
  | [] -> token_list
  | h :: t -> make_tokens_from_tags_list word (word_tags_to_token word h :: token_list) t

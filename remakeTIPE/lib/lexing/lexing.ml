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

let string_to_token_list s =
  let tags_list = Dictionary.find Dictionary.dictionary s in
  match tags_list with
  | [] -> Corrections.correction_ort s
  | _ -> Token.make_tokens_from_tags_list s [] tags_list 

(**
  Make a list of token list (each word have multiple token) from
  a string (sentence).
*)
let sentence_to_token_list_list s =
  s |> sentence_to_string_list |> List.rev_map string_to_token_list

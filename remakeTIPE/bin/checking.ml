let check_gender g1 g2 =
  match g1, g2 with
  | x, y when x = y -> true, x
  | "m", "e" | "e", "m" -> true, "m"
  | "f", "e" | "e", "f" -> true, "f"
  | _ -> false, ""

let check_number n1 n2 =
  match n1, n2 with
  | x, y when x = y -> true, x
  | "s", "i" | "i", "s" -> true, "s"
  | "p", "i" | "i", "p" -> true, "p"
  | _ -> false, ""

let get_person_subject subject =
  match subject with
  | SUBJECT (word, tags) ->
    match tags with
    | 

let check_subject_verb subject verb =
  match subject, verb with
  | Node (SUBJECT (word_subject, tags_subject), children_subject), Node (VERB (word_verb, tags_verb), children_verb) ->
    match children_subject with
    | Node (NOMINAL_GROUP (word_nominal_group, tags_nominal_group), children_nominal_group) ->
      let (is_correct, result_person) = check_person_subject_verb (Tags.get_person_nominal_group tags_nominal_group) (Tags.get_person_verb tags_verb) in

  | _ -> false

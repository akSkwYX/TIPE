open Syntax_tree

let check_gender g1 g2 =
  match g1, g2 with
  | x, y when x = y -> x
  | "m", "e" | "e", "m" -> "m"
  | "f", "e" | "e", "f" -> "f"
  | _ -> ""

let check_number n1 n2 =
  match n1, n2 with
  | x, y when x = y -> x
  | "s", "i" | "i", "s" -> "s"
  | "p", "i" | "i", "p" -> "p"
  | _ -> ""

let check_person plist1 plist2 =
  List.filter (fun x -> List.mem x plist2) plist1

let check_subject_verb subject verb =
  match subject, verb with
  | Node (SUBJECT (word_subject, tags_subject), children_subject), Node (VERB (word_verb, tags_verb), children_verb) ->
    begin
    let person_verb = Tags.get_person_verb tags_verb in
    match children_subject with
    | [Node (NOMINAL_GROUP (word_nominal_group, tags_nominal_group), children_nominal_group)] ->
      let person_nominal_group = Tags.get_person_nominal_group tags_nominal_group in
      let person = check_person person_nominal_group person_verb in
      person
    | [Node (PERSONAL_PRONOUN_SUBJECT (_, tags_personal_pronoun_subject), _)] ->
      let person_personal_pronoun_subject = Tags.get_person_personal_pronoun_subject tags_personal_pronoun_subject in
      let person = check_person [person_personal_pronoun_subject] person_verb in
      person
    | _ -> []
    end
  | _ -> []

let check_adjectives adjective1 adjective2 =
  match adjective1, adjective2 with
  | Node (ADJECTIVE (_, tags_adjective1), _), Node (ADJECTIVE (_, tags_adjective2), _) ->
    begin
    let gender_adjective1 = Tags.get_gender_default tags_adjective1 in
    let gender_adjective2 = Tags.get_gender_default tags_adjective2 in
    let number_adjective1 = Tags.get_number_default tags_adjective1 in
    let number_adjective2 = Tags.get_number_default tags_adjective2 in
    [ check_gender gender_adjective1 gender_adjective2; check_number number_adjective1 number_adjective2 ]
    end
  | _ -> [ ""; "" ]

let check_determiner_noun determiner noun =
  match determiner, noun with
  | Node (DETERMINER (_, tags_determiner), _), Node (NOUN (_, tags_noun), _) ->
    begin
    let gender_determiner = Tags.get_gender_default tags_determiner in
    let number_determiner = Tags.get_number_default tags_determiner in
    let gender_noun = Tags.get_gender_default tags_noun in
    let number_noun = Tags.get_number_default tags_noun in
    ( check_gender gender_determiner gender_noun, check_number number_determiner number_noun )
    end
  | _ -> ("", "") 

let check_adjective_noun adjective noun =
  match adjective, noun with
  | Node (ADJECTIVE (_, tags_adjective), _), Node (NOUN (_, tags_noun), _) ->
    begin
    let gender_adjective = Tags.get_gender_default tags_adjective in
    let gender_noun = Tags.get_gender_default tags_noun in
    let number_adjective = Tags.get_number_default tags_adjective in
    let number_noun = Tags.get_number_default tags_noun in
    ( check_gender gender_adjective gender_noun, check_number number_adjective number_noun )
    end
  | _ -> ("", "")

let check_nominal_group determiner adjective1 noun adjective2 =
  (* gender determiner noun and number determiner noun *)
  let (gdn, ndn)= check_determiner_noun determiner noun in
  match adjective1, adjective2 with
  | Node (ADJECTIVE ("", []), _), Node (ADJECTIVE ("", []), _) -> [ gdn; ndn ]
  | Node (ADJECTIVE ("", []), _), adjective | adjective, Node (ADJECTIVE ("", []), _) ->
    let (gan, nan) = check_adjective_noun adjective noun in
    [ check_gender gdn gan; check_number ndn nan ]
  | _ ->
    let (ga1n, na1n) = check_adjective_noun adjective1 noun in
    let (ga2n, na2n) = check_adjective_noun adjective2 noun in
    [ check_gender (check_gender gdn ga1n) ga2n; check_number (check_number ndn na1n) na2n ]

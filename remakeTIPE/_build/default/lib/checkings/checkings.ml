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

let is_begining_by_vowel word =
  let first_letter = word.[0] in
  match first_letter with
  | 'a' | 'e' | 'i' | 'o' | 'u' | 'y' -> true
  | _ -> false

let silent_h_words = [
  "habile"; "habileté"; "habit"; "habitat"; "habitude"; "haleine"; "hallucination";
  "halogène"; "haltère"; "hebdomadaire"; "hématome"; "hémicycle"; "hémorragie";
  "hélicoptère"; "hélice"; "héritage"; "héroïne"; "héroïsme"; "héraldique"; "hérédité";
  "herbage"; "herbivore"; "herpès"; "heure"; "hibiscus"; "hippodrome"; "homéopathie";
  "homicide"; "homme"; "honnêteté"; "hôpital"; "horaire"; "horizon"; "horoscope";
  "horreur"; "hospice"; "hôtel"; "huile"; "humanité"; "humidité"; "humilité"; "humour";
  "humeur"; "hurluberlu"; "hydratant"; "hydraulique"; "hydrogène"; "hymne"; "hypnose";
  "hypocrite"; "hypothèse"; "hypoténuse"; "hystérie"
]

let is_begining_by_silent_h word =
  word.[0] = 'h' && List.exists ((=) word) silent_h_words

let can_be_elison word =
  match word with
  | "le" | "la" | "de" | "ce" | "me" | "te" | "se" | "ne" | "je"
  | "ma" | "ta" | "sa" | "quelque" | "que" -> true
  | _ -> false

let is_elison word =
  match word with
  | "l'" | "d'" | "c'" | "m'" | "t'" | "s'" | "n'" | "j'"
  | "qu'" | "quelqu'" -> true
  | _ -> false

let check_elison word1 word2 =
  not (can_be_elison word1 && 
       (is_begining_by_vowel word2 || is_begining_by_silent_h word2))
  &&
  not (is_elison word1 && 
      not (is_begining_by_vowel word2 || is_begining_by_silent_h word2))

let is_infinitive token =
  Tags.is_infinitive (Token.get_tags token)

let preposition_coi = [
  "à"; "de"; "pour"; "envers"; "contre"; "sur"; "avec"; "selon";
  "d’après"; "parmi"; "entre"; "vers"; "auprès de"; "quant à"
]

let is_preposition_coi token =
  List.exists ((=) (Token.get_word token)) preposition_coi
  
let check_subject_simple_verb_elison subject verb =
  match subject, verb with
  | Node (SUBJECT (word_subject, _), [Node (PERSONAL_PRONOUN_SUBJECT _, _)]), Node (VERB (_, tags_verb), _) ->
    check_elison word_subject (Tags.get_root tags_verb)
  | _ -> true

(*TODO : Imperative*)

let check_subject_simple_verb subject verb =
  if not (check_subject_simple_verb_elison subject verb) then [] else
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

let is_auxiliary token =
  match token with
  | Token.VERB (word_verb, tags_verb) ->
    let root = Tags.get_root tags_verb in
    root = "être" || root = "avoir"
  | _ -> false

let is_past_participle token =
  match token with
  | Token.VERB (word_verb, tags_verb) ->
    Tags.is_past_participle tags_verb
  | _ -> false

let check_subject_past_participle subject verb =
  match subject, verb with
  | Node (SUBJECT (_, subject_tags), _), Node (VERB (_, verb_tags), _) ->
    begin
    let subject_gender = Tags.get_gender_default subject_tags in
    let subject_number = Tags.get_number_default subject_tags in
    let verb_gender = Tags.get_gender_past_participle verb_tags in
    let verb_number = Tags.get_number_past_participle verb_tags in
    match [ check_gender subject_gender verb_gender; check_number subject_number verb_number ] with
    | [""; ""] | [""; _] | [_; ""] -> []
    | [g; n] -> [g; n]
    | _ -> failwith "WTF IS HAPPENING !"
    end
  | _ -> []

let check_pronoun_verb_elison pronoun verb =
  match pronoun, verb with
  | Empty, Node (VERB _, _) -> true
  | Node (PRE_VERB_PRONOUN (pronoun_word, _), _), Node (VERB (_, verb_tags), _) ->
    check_elison pronoun_word (Tags.get_root verb_tags)
  | _ -> false

let check_pronoun_past_participle_agreement pronoun verb =
  match pronoun, verb with
  | Node (PRE_VERB_PRONOUN (pronoun_word, pronoun_tags), _), Node (VERB (verb_word, verb_tags), _) ->
    begin
    let pronoun_gender = Tags.get_gender_default pronoun_tags in
    let pronoun_number = Tags.get_number_default pronoun_tags in
    let verb_gender = Tags.get_gender_past_participle verb_tags in
    let verb_number = Tags.get_number_past_participle verb_tags in
    match [ check_gender pronoun_gender verb_gender ; check_number pronoun_number verb_number ] with
    | [""; ""] | [""; _] | [_; ""] -> []
    | [g; n] -> [g; n]
    | _ -> failwith "WTF IS HAPPENING !"
    end
  | _ -> []

let check_past_participle subject pronoun auxiliary verb =
  if not ( is_past_participle (Syntax_tree.get_token verb) ) then [] else
  match auxiliary, verb with
  | Node (VERB (_, auxiliary_tags), _), Node (VERB (_, verb_tags), _) ->
    begin
    let auxiliary_type = Tags.get_root auxiliary_tags in
    let verb_auxiliary_type = Tags.get_auxiliary_type verb_tags in
    if auxiliary_type <> verb_auxiliary_type then [] else
    if not ( check_pronoun_verb_elison pronoun verb ) then [] else
    if auxiliary_type = "être" then
      check_subject_past_participle subject verb
    else
      match pronoun with
      | Empty -> 
        begin
        let gender_participle = Tags.get_gender_past_participle verb_tags in
        let number_participle = Tags.get_number_past_participle verb_tags in
        match gender_participle, number_participle with
        | "m", "s" -> [ "m"; "s" ]
        | _ -> []
        end
      (*TODO : handle case of coi pronoun*)
      | Node (PRE_VERB_PRONOUN _, _) -> check_pronoun_past_participle_agreement pronoun verb
      | _ -> []
    end
  | _ -> []

let check_auxiliary_past_participle auxiliary past_participle =
  let () = Syntax_tree.print_syntax_tree auxiliary in
  let () = Syntax_tree.print_syntax_tree past_participle in
  if not (is_auxiliary (Syntax_tree.get_token auxiliary)) then [] else
  if not (is_past_participle (Syntax_tree.get_token past_participle)) then [] else
  match auxiliary, past_participle with
  | Node (VERB (_, auxiliary_tags), _), Node (VERB (_, past_participle_tags), _) ->
    let auxiliary_type = Tags.get_root auxiliary_tags in
    let verb_auxiliary_type = Tags.get_auxiliary_type past_participle_tags in
    if auxiliary_type <> verb_auxiliary_type then [] else
    Tags.merge_auxiliary_past_participle auxiliary_tags past_participle_tags
  | _ -> []

let check_pronoun_verb pronoun verb =
  match pronoun, verb with
  | Node (PRE_VERB_PRONOUN (wpronoun, _), _), Node (VERB (_, tverb), _) ->
    if not (check_elison wpronoun (Tags.get_root tverb)) then [] else [""]
  | Empty, Node (VERB _, _) -> [""]
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
  | Node (DETERMINER (wdet, tags_determiner), _), Node (NOUN (wnoun, tags_noun), _) ->
    begin
    if can_be_elison wdet && (is_begining_by_vowel (Tags.get_root tags_noun) || is_begining_by_silent_h (Tags.get_root tags_noun)) then
      [""; ""]
    else if is_elison wdet && not (is_begining_by_vowel (Tags.get_root tags_noun) || is_begining_by_silent_h (Tags.get_root tags_noun)) then
      [""; ""]
    else
      let gender_determiner = Tags.get_gender_default tags_determiner in
      let number_determiner = Tags.get_number_default tags_determiner in
      let gender_noun = Tags.get_gender_default tags_noun in
      let number_noun = Tags.get_number_default tags_noun in
      [ check_gender gender_determiner gender_noun; check_number number_determiner number_noun ]
    end
  | _ -> [ ""; "" ] 

let check_adjective_noun adjective noun =
  match adjective, noun with
  | Node (ADJECTIVE (_, tags_adjective), _), Node (NOUN (_, tags_noun), _) ->
    begin
    let gender_adjective = Tags.get_gender_default tags_adjective in
    let gender_noun = Tags.get_gender_default tags_noun in
    let number_adjective = Tags.get_number_default tags_adjective in
    let number_noun = Tags.get_number_default tags_noun in
    [ check_gender gender_adjective gender_noun; check_number number_adjective number_noun ]
    end
  | _ -> [ ""; "" ]

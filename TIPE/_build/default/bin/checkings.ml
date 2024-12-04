type error =
	| Empty_sentence
	| Unknown_word of string (* Word unknown *)
	| Missing of (Word_classe.word_classe * string) (* Word_class missing * word got *)
	| Accord of (Word_classe.word_classe * string) * (Word_classe.word_classe * string)
	| Conjuguaison of (Word_classe.word_classe * string * (string list) ) * (string * string list) (* (Word classe of subject * subject * informations of subject) * (Verb * informations of verb) *)
	| No

let string_of_error e =
  match e with
  | Empty_sentence -> "Empty sentence"
  | Unknown_word s -> "Unknown word : " ^ s
  | Missing (wc, w) -> "Missing " ^ (Word_classe.word_classe_to_string wc) ^ " got : " ^ w
  | Accord ((wc1, w1), (wc2, w2)) -> "Accord between " ^ (Word_classe.word_classe_to_string wc1) ^ " and " ^ (Word_classe.word_classe_to_string wc2) ^ " : " ^ w1 ^ " and " ^ w2
  | Conjuguaison ((w1, str, info), (verb, info_verb)) ->
    "Conjugaison of " ^ verb ^ " with " ^ Word_classe.word_classe_to_string w1 ^ " : " ^ str ^ 
    "\nVerb person is : " ^ Utility.string_of_string_list (Token.get_verb_person (Token.Token (Word_classe.Verbe, (verb, info_verb))) ) ^
    "\nPerson should match the gender and number of the subject : " ^ Utility.string_of_string_list info
  | No -> "No"

(** 
  Checks the gender compatibility between two given genders.

  @param g1 The first gender to check.
  @param g2 The second gender to check.
  @return A tuple where the first element is a boolean indicating if the genders are compatible,
          and the second element is the resulting gender if they are compatible, or an empty string if not.
*)
let check_gender g1 g2 =
  match g1, g2 with
  | x, y when x = y -> true, x
  | "m", "e" | "e", "m" -> true, "m"
  | "f", "e" | "e", "f" -> true, "f"
  | _ -> false, ""

(** 
  Checks the relationship between two numbers represented as strings.

  @param n1 The first number as a string.
  @param n2 The second number as a string.
  @return A tuple where the first element is a boolean indicating if the numbers have a valid relationship,
          and the second element is the resulting string based on the relationship.
*)
  let check_number n1 n2 =
    match n1, n2 with
    | x, y when x = y -> true, x
    | "s", "i" | "i", "s" -> true, "s"
    | "p", "i" | "i", "p" -> true, "p"
    | _ -> false, ""

(** 
  [check_gender_number informations_1 informations_2] checks if the gender and number information 
  in [informations_1] and [informations_2] match. 

  @param informations_1 A list of strings representing gender and number information.
  @param informations_2 A list of strings representing gender and number information.
  @return A tuple where the first element is a boolean indicating if both gender and number match, 
          and the second element is a list containing the resulting gender and number if they match, 
          or an empty list if they do not match.
*)
  let check_gender_number (informations_1:string list) (informations_2:string list) :(bool * string list) =
    match informations_1, informations_2 with
    | [], [] -> true, []
    | gender_1 :: number_1 :: [], gender_2 :: number_2 :: [] ->
      let (same_gender, result_gender) = check_gender gender_1 gender_2 in
      let (same_number, result_number) = check_number number_1 number_2 in
      if same_gender && same_number then
        (true, result_gender :: [result_number])
      else
        (false, [])
    | _ -> (false, [])

(** 
[check_det_noun det_token noun_token] checks the agreement between a determinant and a noun.

@param det_token The token representing the determinant.
@param noun_token The token representing the noun.

@return A tuple where the first element is a boolean indicating whether the determinant and noun agree in gender and number, 
        and the second element is a list of resulting information if they agree, or an empty list if they do not.

@raise Failure if the provided tokens are not a correct Determinant and a Nom.
*)
let check_det_noun det_token noun_token =
  match det_token, noun_token with
  | Token.Token (Word_classe.Determinant, (det, rad_det::det_informations)), Token.Token (Word_classe.Nom, (noun, rad_noun::noun_informations)) 
    ->
      begin
        let (success, result_informations) = check_gender_number det_informations noun_informations in
        if success then
          (true, result_informations)
        else
          (false, [])
      end
  | _, _ -> failwith "check_det_noun : Doesn't receive a correct Determinant and a Nom"

(** 
  [check_adjectives adj_token_1 adj_token_2] checks if two adjective tokens are compatible in terms of gender and number.
  
  @param adj_token_1 The first adjective token to be checked.
  @param adj_token_2 The second adjective token to be checked.
  
  @return A tuple where the first element is a boolean indicating whether the adjectives are compatible, 
          and the second element is a list of adjective information if they are compatible, or an empty list if they are not.
  
  @raise Failure if either of the tokens is not an adjective.
*)
let check_adjectives adj_token_1 adj_token_2 =
  match adj_token_1, adj_token_2 with
  | Token.Token (Word_classe.Adjectif, (adj1, rad_adj_1::adj_informations_1)), Token.Token (Word_classe.Adjectif, (adj2, rad_adj_2::adj_informations_2))
    ->
      begin
        match adj_informations_1, adj_informations_2 with
        | _, [] -> true, adj_informations_1
        | [], _ -> true, adj_informations_2
        | _ ->
          let (success, result_informations) = check_gender_number adj_informations_1 adj_informations_2 in
          if success then
            (true, result_informations)
          else
            (false, [])
      end
  | _, _ -> failwith "check_adjectives : Doesn't receive a correct Adjectif"

(** 
  [check_noun_adjective noun_token adj_token] checks if the given noun and adjective tokens agree in gender and number.
  
  @param noun_token The token representing the noun, expected to be of type [Token.Token (Word_classe.Nom, ...)].
  @param adj_token The token representing the adjective, expected to be of type [Token.Token (Word_classe.Adjectif, ...)] or [Token.Token (Word_classe.MultipleAdj, ...)].
  
  @return A tuple [(bool, informations)] where:
    - [bool] indicates whether the noun and adjective agree in gender and number.
    - [informations] is a list of resulting information if the agreement check is successful, otherwise an empty list.
  
  @raise Failure if the provided tokens are not of the correct types (i.e., not a noun and an adjective).
*)
let check_noun_adjective noun_token adj_token =
  match noun_token, adj_token with
  | Token.Token (Word_classe.Nom, (noun, rad_noun::noun_informations)), Token.Token (Word_classe.Adjectif, (adj, rad_adj::adj_informations))
  | Token.Token (Word_classe.Nom, (noun, rad_noun::noun_informations)), Token.Token (Word_classe.MultipleAdj, (adj, ((rad_adj::_) as adj_informations) ))
    ->
      begin
        match noun_informations, adj_informations with
        | _, [] -> (true, noun_informations)
        | _ ->
          let (success, result_informations) = check_gender_number noun_informations adj_informations in
          if success then
            (true, result_informations)
          else
            (false, [])
      end
  | _, _ -> failwith "check_noun_adjective : Doesn't receive a correct Nom and a Adjectif"

(** 
  [check_subject_verb subject_token verb_token] checks if the subject and verb tokens agree in person and number.
  
  @param subject_token The token representing the subject, expected to be of type [Token.Token (Word_classe.Sujet, (subject, subject_informations))].
  @param verb_token The token representing the verb, expected to be of type [Token.Token (Word_classe.Verbe, (verb, rad_verb::verb_informations))].
  
  @return A tuple [(bool, subject_informations)] where the boolean indicates if the subject and verb agree, and [subject_informations] contains details about the subject.
  
  @raise Failure if the tokens do not match the expected patterns for a subject and a verb, or if the verb does not have the correct person and number information.
  
  The function works by extracting the person and number information from the subject and verb tokens, and then recursively checking if they match.
  
  Example:
  - If the subject is first person singular ("O1", "s") and the verb is also first person singular ("1s"), the function returns (true, subject_informations).
  - If the subject is third person plural ("O3", "p") and the verb is third person plural ("3p"), the function returns (true, subject_informations).
  - If there is no match, the function returns (false, []).
*)
let check_subject_verb subject_token verb_token =
  match subject_token, verb_token with
  | Token.Token (Word_classe.Sujet, (subject, subject_informations)), Token.Token (Word_classe.Verbe, (verb, rad_verb::verb_informations))
    ->
      begin
        let list_verb_person = verb_token |> Token.get_verb_person in
        match subject_informations, list_verb_person with
        | rad_subject :: person :: _ :: number :: [], h :: t ->
          begin
          let rec check_all_person_verb list_verb_person =
            match person, number, list_verb_person with
            | "O1", "s", ("1s"::t) -> (true, subject_informations)
            | "O1", "p", ("1p"::t) -> (true, subject_informations)
            | "O2", "s", ("2s"::t) -> (true, subject_informations)
            | "O2", "p", ("2p"::t) -> (true, subject_informations)
            | "O3", "s", ("3s"::t) -> (true, subject_informations)
            | "O3", "p", ("3p"::t) -> (true, subject_informations)
            | _, _, (h::t) -> check_all_person_verb t
            | _ -> (false, [])
          in
          check_all_person_verb list_verb_person
          end
        | gender :: number :: [], h :: t ->
          begin
            let rec check_all_person_verb list_verb_person =
              match number, list_verb_person with
              | "s", ("3s"::t) -> (true, subject_informations)
              | "p", ("3p"::t) -> (true, subject_informations)
              | _, (h::t) -> check_all_person_verb t
              | _ -> (false, [])
            in
            check_all_person_verb list_verb_person
          end
        | rad_subject :: person :: _ :: number :: [], _ -> failwith "check_subject_verb : Doesn't receive a correct Verb"
        | person :: number :: [], _ -> failwith "check_subject_verb : Doesn't receive a correct Verb"
        | _ -> failwith "check_subject_verb : Doesn't receive a correct Subject"
      end
  | _, _ -> failwith "check_subject_verb : Doesn't receive a correct Sujet and a Verbe"

(** 
  Checks if a verb is conjugated based on its information.

  @param verb_informations A list of strings representing the verb information.

  @return true if the verb is conjugated, false otherwise.

  @raise Failure if the input list does not match the expected format.
*)
let is_conjugue verb_informations =
  match verb_informations with
    | rad_verb :: intransitif :: transitif_direct :: transitif_indirect :: pronominal :: impersonnel :: auxiliaire_etre :: auxiliaire_avoir :: "Y" :: []
    | rad_verb :: intransitif :: transitif_direct :: transitif_indirect :: pronominal :: impersonnel :: auxiliaire_etre :: auxiliaire_avoir :: "P" :: []
    | rad_verb :: intransitif :: transitif_direct :: transitif_indirect :: pronominal :: impersonnel :: auxiliaire_etre :: auxiliaire_avoir :: "Q" :: _
      -> false
    | rad_verb :: intransitif :: transitif_direct :: transitif_indirect :: pronominal :: impersonnel :: auxiliaire_etre :: auxiliaire_avoir :: temps :: person :: []
      -> true
    | _ -> failwith "is_conjugue : Doesn't receive a correct Verb"

(** 
  [check_verbal_group subject_token verb_token] checks the agreement between a subject and a verb.
  
  @param subject_token The token representing the subject, expected to be of type [Token.Token (Word_classe.Sujet, (subject, informations_subject))].
  @param verb_token The token representing the verb, expected to be of type [Token.Token (Word_classe.Verbe, (verb, informations_verb))].
  
  @return A tuple containing:
    - A boolean indicating whether the subject and verb agree.
    - A list of result informations if the agreement check is successful.
    - An error type [No] if the agreement check is successful, or [Conjuguaison] with relevant details if it fails.
  
  @raise Failure if the input tokens do not match the expected subject and verb types, or if the subject informations do not match the expected patterns in [get_person].
*)
let check_verbal_group subject_token verb_token =
  let get_person informations =
    match informations with
    | g :: n :: [] -> ["O3"; g; n]
    | rad_subject :: "O1" :: g :: n :: [] -> ["O1"; g; n]
    | _ -> failwith "get_person : informations doesn't match a gn or a pronuoun subjet"
  in
  match subject_token, verb_token with
  | Token.Token (Word_classe.Sujet, (subject, informations_subject)), Token.Token (Word_classe.Verbe, (verb, informations_verb))
    ->
      begin
        if is_conjugue informations_verb then
          let (success, result_informations) = check_subject_verb subject_token verb_token in
          if success then
            (true, result_informations, No)
          else
            (false, [], Conjuguaison ((Word_classe.Sujet, subject, get_person informations_subject), (verb, informations_verb)))
        else
          (false, [], Conjuguaison ((Word_classe.Sujet, subject, get_person informations_subject), (verb, informations_verb)))
      end
  | _, _ -> failwith "check_verbal_group : Doesn't receive a correct Sujet and a Verbe"
  
(** 
  [check_nominal_group det_token adj_token noun_token adj_token_2] checks the agreement between a determinant, 
  one or two adjectives, and a noun. The function takes four tokens as input: a determinant token, an adjective token, 
  a noun token, and a second adjective token. It returns a tuple containing a boolean indicating success, 
  a list of result informations, and an optional error type.

  The function matches the input tokens against several patterns to ensure they are of the correct word classes 
  (Determinant, Adjectif, Nom, MultipleAdj). It then performs agreement checks between the determinant and the noun, 
  the first adjective and the noun, and the second adjective and the noun, if applicable.

  @param det_token The determinant token to be checked.
  @param adj_token The first adjective token to be checked.
  @param noun_token The noun token to be checked.
  @param adj_token_2 The second adjective token to be checked.

  @return A tuple containing:
    - A boolean indicating whether the agreement checks were successful.
    - A list of result informations from the agreement checks.
    - An optional error type indicating the type of agreement error, if any.

  @raise Failure if the input tokens do not match the expected patterns.
*)
let check_nominal_group det_token adj_token noun_token adj_token_2 =
  match det_token, adj_token, noun_token, adj_token_2 with
  | Token.Token (Word_classe.Determinant, (det, rad_det::det_informations)),
    Token.Token (Word_classe.Adjectif, (adj, rad_adj::adj_informations)),
    Token.Token (Word_classe.Nom, (noun, rad_noun::noun_informations)),
    Token.Token (Word_classe.Adjectif, (adj_2, rad_adj2::adj_informations_2))

  | Token.Token (Word_classe.Determinant, (det, rad_det::det_informations)),
    Token.Token (Word_classe.MultipleAdj, (adj, ((rad_adj::_) as adj_informations))),
    Token.Token (Word_classe.Nom, (noun, rad_noun::noun_informations)),
    Token.Token (Word_classe.Adjectif, (adj_2, rad_adj2::adj_informations_2))

  | Token.Token (Word_classe.Determinant, (det, rad_det::det_informations)),
    Token.Token (Word_classe.Adjectif, (adj, rad_adj::adj_informations)),
    Token.Token (Word_classe.Nom, (noun, rad_noun::noun_informations)),
    Token.Token (Word_classe.MultipleAdj, (adj_2, ((rad_adj2::_) as adj_informations_2)))

  | Token.Token (Word_classe.Determinant, (det, rad_det::det_informations)),
    Token.Token (Word_classe.MultipleAdj, (adj, ((rad_adj::_) as adj_informations))),
    Token.Token (Word_classe.Nom, (noun, rad_noun::noun_informations)),
    Token.Token (Word_classe.MultipleAdj, (adj_2, ((rad_adj2::_) as adj_informations_2)))
    ->
      begin
        let (success_det_noun, result_informations_det_noun) = check_det_noun det_token noun_token in
        if success_det_noun then
          let (success_adj1_noun, result_informations_adj1_noun) = check_noun_adjective noun_token adj_token in
          if success_adj1_noun then
            let (success_adj2_noun, result_informations_adj2_noun) = check_noun_adjective noun_token adj_token_2 in
            if success_adj2_noun then
              (true, result_informations_det_noun, No)
            else
              (false, [], Accord ((Word_classe.Adjectif, adj_2), (Word_classe.Nom, noun)))
          else
            (false, [], Accord ((Word_classe.Adjectif, adj), (Word_classe.Nom, noun)))
        else
          (false, [], Accord ((Word_classe.Determinant, det), (Word_classe.Nom, noun)))
      end
  | Token.Token (Word_classe.Determinant, (det, rad_det::det_informations)),
    Token.Token (Word_classe.Adjectif, ("", [])),
    Token.Token (Word_classe.Nom, (noun, rad_noun::noun_informations)),
    Token.Token (Word_classe.Adjectif, ("", []))
    ->
      begin
        let (success_det_noun, result_informations_det_noun) = check_det_noun det_token noun_token in
        if success_det_noun then
          (true, result_informations_det_noun, No)
        else
          (false, [], Accord ((Word_classe.Determinant, det), (Word_classe.Nom, noun)))
      end
  | Token.Token (Word_classe.Determinant, (det, rad_det::det_informations)),
    Token.Token (Word_classe.Adjectif, (adj, rad_adj::adj_informations)),
    Token.Token (Word_classe.Nom, (noun, rad_noun::noun_informations)),
    Token.Token (Word_classe.Adjectif, ("", []))

  | Token.Token (Word_classe.Determinant, (det, rad_det::det_informations)),
    Token.Token (Word_classe.MultipleAdj, (adj, ((rad_adj::_) as adj_informations))),
    Token.Token (Word_classe.Nom, (noun, rad_noun::noun_informations)),
    Token.Token (Word_classe.Adjectif, ("", []))
    ->
      begin
        let (success_det_noun, result_informations_det_noun) = check_det_noun det_token noun_token in
        if success_det_noun then
          let (success_adj_noun, result_informations_adj_noun) = check_noun_adjective noun_token adj_token in
          if success_adj_noun then
            (true, result_informations_det_noun, No)
          else
            (false, [], Accord ((Word_classe.Adjectif, adj), (Word_classe.Nom, noun)))
        else
          (false, [], Accord ((Word_classe.Determinant, det), (Word_classe.Nom, noun)))
      end
  | Token.Token (Word_classe.Determinant, (det, rad_det::det_informations)),
    Token.Token (Word_classe.Adjectif, ("", [])),
    Token.Token (Word_classe.Nom, (noun, rad_noun::noun_informations)),
    Token.Token (Word_classe.Adjectif, (adj_2, rad_adj2::adj_informations_2))
    
  | Token.Token (Word_classe.Determinant, (det, rad_det::det_informations)),
    Token.Token (Word_classe.Adjectif, ("", [])),
    Token.Token (Word_classe.Nom, (noun, rad_noun::noun_informations)),
    Token.Token (Word_classe.MultipleAdj, (adj_2, ((rad_adj2::_) as adj_informations_2)))
    ->
      begin
        let (success_det_noun, result_informations_det_noun) = check_det_noun det_token noun_token in
        if success_det_noun then
          let (success_adj_noun, result_informations_adj_noun) = check_noun_adjective noun_token adj_token_2 in
          if success_adj_noun then
            (true, result_informations_det_noun, No)
          else
            (false, [], Accord ((Word_classe.Adjectif, adj_2), (Word_classe.Nom, noun)))
        else
          (false, [], Accord ((Word_classe.Determinant, det), (Word_classe.Nom, noun)))
      end
  | _, _, _, _ -> failwith "check_nominal_group : Doesn't receive a correct Determinant, Adjectif, Nom and Adjectif"
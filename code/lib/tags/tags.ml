let is_equal t1 t2 =
  List.for_all2 String.equal t1 t2

let get_word_class tags =
  match tags with
  | _ :: _ :: wc :: _ -> wc
  | _ -> "U"

let get_frequency tags =
  match tags with
  | frequency :: root :: tl -> int_of_string frequency
  | _ -> failwith "tags.ml/get_frequency : tags don't match format [frequency; root; ...]"

let get_word tags =
  match tags with
  | _ :: word :: _ -> word
  | _ -> failwith "tags.ml/get_word : tags not match format [frequency; root; wc; ...]"

let get_root tags =
  match tags with
  | frequency :: root :: tl -> root
  | _ -> failwith "tags.ml/get_root : tags not match format [frequency; root; ...]"

let get_gender_default tags =
  match tags with
  | _ :: _ :: _ :: gender :: _ :: [] -> gender
  | gender :: _ :: [] -> gender
  | _ -> failwith "tags.ml/get_gender_default : tags not match format [frequency; root; wc; gender; ... ] or [gender; number]"

let get_number_default tags =
  match tags with
  | _ :: _ :: _ :: _ :: number :: [] -> number 
  | _ :: number :: [] -> number
  | _ -> failwith "tags.ml/get_number_default : tags not match format [frequency; root; wc; gender; number; ... ] or [gender; number]"

let get_gender_nominal_group tags =
  match tags with
  | gender :: number :: [] -> gender
  | _ -> failwith "tags.ml/get_gender_nominal_group : nominal_group tags not match format [gender; number]"

let get_number_nominal_group tags =
  match tags with
  | gender :: number :: [] -> number
  | _ -> failwith "tags.ml/get_number_nominal_group : nominal_group tags not match format [gender; number]"

let get_person_nominal_group tags =
  let number = get_number_nominal_group tags in
  match number with
  | "s" -> ["3s"]
  | "p" -> ["3p"]
  | "i" -> ["3s"; "3p"]
  | _ -> failwith "tags.ml/get_person_nominal_group : nominal_group number not match"

let get_person_personal_pronoun_subject tags =
  match tags with
  | frequency :: root :: wc :: person :: gender :: number :: [] ->
    begin
    match person, number with
    | "O1", "s" -> "1s"
    | "O1", "p" -> "1p"
    | "O2", "s" -> "2s"
    | "O2", "p" -> "2p"
    | "O3", "s" -> "3s"
    | "O3", "p" -> "3p"
    | _ -> failwith "tags.ml/get_person_personal_pronoun_subject : personnal_pronoun_subject person and number not match"
    end
  | _ -> failwith "tags.ml/get_person_personnal_pronoun_subject : personnal_pronoun_subject tags not match format [frequency; root; wc; person; gender; number]"

let get_gender_personnal_pronoun_subject tags =
  match tags with
  | frequency :: root :: wc :: person :: gender :: number :: [] -> gender
  | _ -> failwith "tags.ml/get_gender_personnal_pronoun_subject : personnal_pronoun_subject tags not match format [frequency; root; wc; person; gender; number]"

let get_number_personnal_pronoun_subject tags =
  match tags with
  | frequency :: root :: wc :: person :: gender :: number :: [] -> number
  | _ -> failwith "tags.ml/get_number_personnal_pronoun_subject : personnal_pronoun_subject tags not match format [frequency; root; wc; person; gender; number]"

(**
  check if the entry string (a tag) is a person tag i.e. is in ["1s", "2s"; "3s"; ...]
*)
let is_person string =
  List.mem string ["1s"; "2s"; "3s"; "1p"; "2p"; "3p"]

let get_person_verb tags =
  List.filter is_person tags

let is_infinitive tags =
  List.exists ((=) "Y") tags

let is_past_participle tags =
  List.exists ((=) "Q") tags

let get_auxiliary_type tags =
  let rec aux l =
    match l with
    | [] -> failwith "tags.ml/get_auxiliary_type : tags are not tags of a past participle"
    | "Q" :: "a" :: _ -> "avoir"
    | "Q" :: _ -> "être"
    | _ :: tl -> aux tl
  in
  aux tags

let get_gender_past_participle tags =
  let rec aux l =
    match l with
    | [] -> failwith "tags.ml/get_gender_past_participle : tags don't match format [...; \"Q\"; (\"a\"); gender; number; ...]"
    | "Q" :: "a" :: gender :: _ -> gender
    | "Q" :: gender :: _ -> gender
    | _ :: tl -> aux tl
  in
  aux tags

let get_number_past_participle tags =
  let rec aux l =
    match l with
    | [] -> failwith "tags.ml/get_number_past_participle : tags don't match format [...; \"Q\"; (\"a\"); gender; number; ...]"
    | "Q" :: "a" :: gender :: number :: _ -> number 
    | "Q" :: gender :: number :: _ -> number
    | _ :: tl -> aux tl
  in
  aux tags

let merge_auxiliary_past_participle auxiliary_tags past_participle_tags =
  match past_participle_tags with
  | frequency :: root :: wc :: tl -> tl @ (get_person_verb auxiliary_tags)
  | _ -> failwith "tags.ml/merge_auxiliary_past_participle : past_participle_tags don't match format [frequency; root; wc; ...]"

let replace_word tags root_word =
  match tags with
  | f :: w :: tl -> f :: root_word :: tl
  | _ -> failwith "tags.ml/replace_root : tags not match format [frequency; root; ...]"

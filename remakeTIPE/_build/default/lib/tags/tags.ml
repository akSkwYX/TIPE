let is_equal t1 t2 =
  List.for_all2 String.equal t1 t2

let get_word_class tags =
  match tags with
  | _ :: _ :: wc :: _ -> wc
  | _ -> "U"

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
  | frequency :: root :: wc :: person :: gender :: number :: [] -> person
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

let replace_word tags root_word =
  match tags with
  | f :: w :: tl -> f :: root_word :: tl
  | _ -> failwith "tags.ml/replace_root : tags not match format [frequency; root; ...]"

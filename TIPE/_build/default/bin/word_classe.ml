type word_classe =
	| S
	| GV
	| GN
	| MultipleAdj
	| Pronom_sujet
	| Sujet
	| Verbe
	| Determinant
	| Nom
	| Adjectif
	| Unknown

let word_classe_to_string wc =
  match wc with
  | S -> "S"
  | GV -> "GV"
  | GN -> "GN"
  | MultipleAdj -> "MultipleAdj"
  | Pronom_sujet -> "Pronom\\_sujet"
  | Sujet -> "Sujet"
  | Verbe -> "Verbe"
  | Determinant -> "Determinant"
  | Nom -> "Nom"
  | Adjectif -> "Adjectif"
  | Unknown -> "Unknown"

let word_class_to_short_string wc =
  match wc with
  | S -> "S"
  | GV -> "GV"
  | GN -> "GN"
  | MultipleAdj -> "As"
  | Pronom_sujet -> "Os"
  | Sujet -> "S"
  | Verbe -> "V"
  | Determinant -> "D"
  | Nom -> "N"
  | Adjectif -> "A"
  | Unknown -> "U"

let print_word_classe wc =
  print_string (word_classe_to_string wc)
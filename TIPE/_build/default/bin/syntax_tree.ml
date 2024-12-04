type syntax_tree =
	| Node of Word_classe.word_classe * string list * syntax_tree list
	| Leaf of string
	| Error of Checkings.error
	| Empty
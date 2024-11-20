(* ------------------------------------------------------------------ *)
(*     Hearth of project : Detecting errors and making correction     *)
(* ------------------------------------------------------------------ *)


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

type syntax_tree =
	| Node of Word_classe.word_classe * string list * syntax_tree list
	| Leaf of string
	| Error of error
	| Empty

let rec syntax_tree_in_tex_aux file tree =
	match tree with
	| Empty -> ()
	| Leaf s -> Printf.fprintf file "child {node {%s}}" s
	| Error e -> Printf.fprintf file "child {node {%s}}" (string_of_error e)
	| Node (wc, [], l) ->
		Printf.fprintf file "child { node {%s} " (Word_classe.word_classe_to_string wc);
		List.iter (fun x -> syntax_tree_in_tex_aux file x) l;
		Printf.fprintf file "}"
	| Node (wc, i, l) ->
		Printf.fprintf file "child { node {%s \\\\ %s} " (Word_classe.word_classe_to_string wc) (Utility.string_of_string_list i);
		List.iter (fun x -> syntax_tree_in_tex_aux file x) l;
		Printf.fprintf file "}"

let syntax_tree_in_tex tree file =
	Printf.fprintf file "\\begin{center}\n\\begin{tikzpicture}\n\\node{Sentence}[sibling distance = 3cm, level distance = 3cm, align=center]\n";
	syntax_tree_in_tex_aux file tree;
	Printf.fprintf file ";\n\\end{tikzpicture}\n\\end{center}\n"

let syntax_tree_list_in_tex tree_list =
	Sys.chdir "results";
  let file = open_out_gen [Open_append] 0 "result.tex" in
	List.iter (fun x -> syntax_tree_in_tex x file) tree_list;
  close_out file;
	Sys.chdir ".."

let is_a_success_tree tree =
	match tree with
	| Empty | Error _ -> false
	| _ -> true

let get_results tree_list =
	let filtered_list = List.filter is_a_success_tree tree_list in
	match filtered_list with
	| [] -> syntax_tree_list_in_tex tree_list; print_string "Not a correct sentence"
	| h :: t -> syntax_tree_list_in_tex filtered_list; print_string "Correct sentence"
	


(* Represente the sentence which will be itterated over with indice being the index of where is the correction actually *)
type item =
	{ 
	t_a : Token.token array;
	mutable indice : int;
	length : int;
	}

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

(* Checking gender and number compatibility for adjectives, nouns, determinants, NOT FOR OTHERS *)
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

let check_subject_verb subject_token verb_token =
	match subject_token, verb_token with
	| Token.Token (Word_classe.Sujet, (subject, subject_informations)), Token.Token (Word_classe.Verbe, (verb, rad_verb::verb_informations))
		->
			begin
				let list_verb_person = verb_token |> Token.get_verb_person in
				match subject_informations, list_verb_person with
				| person :: _ :: number :: [], h :: t ->
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
				| person :: _ :: number :: [], _ -> failwith "check_subject_verb : Doesn't receive a correct Verb"
				| _ -> failwith "check_subject_verb : Doesn't receive a correct Subject"
			end
	| _, _ -> failwith "check_subject_verb : Doesn't receive a correct Sujet and a Verbe"

let is_conjugue verb_informations =
  match verb_informations with
		| intransitif :: transitif_direct :: transitif_indirect :: pronominal :: impersonnel :: auxiliaire_etre :: auxiliaire_avoir :: "Y" :: []
		| intransitif :: transitif_direct :: transitif_indirect :: pronominal :: impersonnel :: auxiliaire_etre :: auxiliaire_avoir :: "P" :: []
		| intransitif :: transitif_direct :: transitif_indirect :: pronominal :: impersonnel :: auxiliaire_etre :: auxiliaire_avoir :: "Q" :: _
			-> false
		| intransitif :: transitif_direct :: transitif_indirect :: pronominal :: impersonnel :: auxiliaire_etre :: auxiliaire_avoir :: temps :: person :: []
			-> true
		| _ -> failwith "get_verb_person : Doesn't receive a correct Verb"

let check_verbal_group subject_token verb_token =
	let get_person informations =
		match informations with
		| g :: "s" :: [] -> ["O3"; g; "s"]
		| g :: "p" :: [] -> ["O3"; g; "p"]
		| _ -> failwith "get_person : informations doesn't match a gn"
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

(* Increasing the index of sentence which is currently being corrected *)
let pass sentence =
	sentence.indice <- sentence.indice + 1

(* First call of mutual recursivity for checking *)
let rec get_syntax_tree sentence =
	if sentence.indice > sentence.length then
		Error Empty_sentence
	else
		let (verbal_group_tree, verbal_group_token) = get_verbal_group sentence in
		verbal_group_tree

and get_verbal_group sentence =
	let (subject_tree, subject_token) = get_subject sentence in
	match subject_tree with
	| Error e -> (Error e, subject_token)
	| _ ->
		let (verb_tree, verb_token) = get_verb sentence in
		match verb_tree with
		| Error e -> (Error e, verb_token)
		| _ ->
			let (success, informations, error) = check_verbal_group subject_token verb_token in
			let token = Token.Token ( Word_classe.GV, ((Token.get_word subject_token) ^ " " ^ (Token.get_word verb_token), informations) ) in
			if success then
				(Node (Word_classe.GV, informations, [subject_tree; verb_tree]), token)
			else
				( Error error, token )

and get_subject s =
	if s.indice >= s.length then
		(Error Empty_sentence, Token.Token (Word_classe.Sujet, ("", [])))
	else
		begin
		let wc_token = Token.get_word_classe s.t_a.(s.indice) in
    match wc_token with
    | Word_classe.Determinant | Word_classe.Nom | Word_classe.Adjectif
      ->
      begin
        let (nominal_group_tree, nominal_group_token) = get_nominal_group s in
        match nominal_group_tree with
        | Error e -> (Error e, nominal_group_token)
        | Node (Word_classe.GN, informations, _) -> (Node (Word_classe.Sujet, informations, [nominal_group_tree]), nominal_group_token)
        | _ -> failwith "get_subject : wrong tree, waiting for a GN"
      end
		| Word_classe.Pronom_sujet
      ->
      begin
        let (pronom_sujet_tree, pronom_subject_token) = get_pronom_sujet s in
        match pronom_sujet_tree with
        | Error e -> (Error e, pronom_subject_token)
        | Node (Word_classe.Pronom_sujet, informations, _) -> (Node (Word_classe.Sujet, informations, [pronom_sujet_tree]), pronom_subject_token)
        | _ -> failwith "get_subject : wrong tree, waiting for a Pronom_sujet"
      end
		| _ -> (Error (Missing (Word_classe.Sujet, Token.get_word s.t_a.(s.indice))), (Token.Token (Word_classe.Sujet, (Token.get_word s.t_a.(s.indice), []))) )
		end

and get_nominal_group s =
	let (det_tree, det_token) = get_determinant s in
	match det_tree with
	| Error e -> (Error e, det_token)
	| _ ->
		let (adj_tree, adj_token) = get_adjectifs s in
		match adj_tree with
		| Error e -> (Error e, adj_token)
		| _ ->
			let (noun_tree, noun_token) = get_nom s in
			match noun_tree with
			| Error e -> (Error e, noun_token)
			| _ ->
				let (adj_tree_2, adj_token_2) = get_adjectifs s in
				match adj_tree_2 with
				| Error e -> (Error e, adj_token_2)
				| _ ->
					let (success, informations, error) = check_nominal_group det_token adj_token noun_token adj_token_2 in
					let token = Token.Token (Word_classe.GN, (Token.get_word det_token ^ " " ^ Token.get_word adj_token ^ " " ^ Token.get_word noun_token ^ " " ^ Token.get_word adj_token_2, informations)) in
					if success then
						match adj_tree, adj_tree_2 with
						| Node (Word_classe.Adjectif, [], [Leaf ""]), Node (Word_classe.Adjectif, [], [Leaf ""]) -> (Node (Word_classe.GN, informations, [det_tree; noun_tree]), token)
						| Node (Word_classe.Adjectif, [], [Leaf ""]), _ -> (Node (Word_classe.GN, informations, [det_tree; noun_tree; adj_tree_2]), token)
						| _, Node (Word_classe.Adjectif, [], [Leaf ""]) -> (Node (Word_classe.GN, informations, [det_tree; adj_tree; noun_tree]), token)
						| _ -> (Node (Word_classe.GN, informations, [det_tree; adj_tree; noun_tree; adj_tree_2]), token)
					else
						(Error error, token)

and get_determinant s =
	if s.indice >= s.length then
		(Error Empty_sentence, Token.Token (Word_classe.Determinant, ("", [])))
	else
		begin
		let token = s.t_a.(s.indice) in
		match token with
		| Unknown w -> (Error ( Unknown_word w ), token)
		| Token (Word_classe.Determinant, (det, informations)) -> pass s; (Node (Word_classe.Determinant, informations, [Leaf det]), token)
		| Token (_, (word, _)) -> (Error (Missing (Word_classe.Determinant, word)), token)
		end

and get_nom s =
	if s.indice >= s.length then
		(Error Empty_sentence, Token.Token (Word_classe.Nom, ("", [])))
	else
		begin
		let token = s.t_a.(s.indice) in
		match token with
		| Unknown w -> (Error ( Unknown_word w ), token)
		| Token (Word_classe.Nom, (nom, informations)) -> pass s; (Node (Word_classe.Nom, informations, [Leaf nom]), token)
		| Token (_, (word, _)) -> (Error (Missing (Word_classe.Nom, word)), token)
		end

and get_adjectifs s =
	let make_adj_tree_list adj_list adj_informations_list =
		let rec aux adj_list adj_informations_list result =
			match adj_list, adj_informations_list with
			| [], [] -> result
			| h :: t, h_info :: t_info -> aux t t_info (Node (Word_classe.Adjectif, h_info, [Leaf h]) :: result)
			| _ -> failwith "make_adj_tree_list : adj_list and adj_informations_list doesn't have the same length"
		in
		List.rev (aux adj_list adj_informations_list [])
	in
	let gender_number_comparison adj_informations_1 adj_informations_2 =
		match adj_informations_1, adj_informations_2 with
		| x, y when x = y -> x
		| ["m"; "s"], ["e"; "s"] | ["e"; "s"], ["m"; "s"] -> ["m"; "s"]
		| ["f"; "s"], ["e"; "s"] | ["e"; "s"], ["f"; "s"] -> ["f"; "s"]
		| ["m"; "p"], ["e"; "p"] | ["e"; "p"], ["m"; "p"] -> ["m"; "p"]
		| ["f"; "p"], ["e"; "p"] | ["e"; "p"], ["f"; "p"] -> ["f"; "p"]
		| ["m"; "s"], ["m"; "i"] | ["m"; "i"], ["m"; "s"] -> ["m"; "s"]
		| ["f"; "s"], ["f"; "i"] | ["f"; "i"], ["f"; "s"] -> ["f"; "s"]
		| ["m"; "p"], ["m"; "i"] | ["m"; "i"], ["m"; "p"] -> ["m"; "p"]
		| ["f"; "p"], ["f"; "i"] | ["f"; "i"], ["f"; "p"] -> ["f"; "p"]
		| ["e"; n], [g; "i"] | [g; "i"], ["e"; n] -> [g; n]
		| _ -> failwith "information_comparison : adj_informations_1 and adj_informations_2 doesn't match"
	in
	let rec aux' s current_adj_list current_adj_informations_list current_token =
		if s.indice >= s.length then
			match current_adj_list with
			| [] -> (Error Empty_sentence, Token.Token (Word_classe.Adjectif, ("", [])))
			| _ -> (Node (Word_classe.MultipleAdj, (List.fold_left gender_number_comparison ["e"; "i"] current_adj_informations_list), make_adj_tree_list current_adj_list current_adj_informations_list), current_token)
		else
			begin
				let token = s.t_a.(s.indice) in
				match token with
				| Unknown w -> (Error ( Unknown_word w ), token)
				| Token (Word_classe.Adjectif, (adj, informations)) 
					-> 
						begin
							pass s;
							match current_adj_list with
							| [] -> aux' s [adj] [informations] token
							| _ -> 
								let (success, result_informations) = check_adjectives token current_token in
								if success then
									aux' s (adj :: current_adj_list) (informations :: current_adj_informations_list) (Token.Token (Word_classe.MultipleAdj, (Token.get_word current_token ^ " " ^ adj, result_informations)))
								else
									(Error ( Accord ((Word_classe.Adjectif, adj), (Word_classe.Adjectif, Token.get_word current_token)) ), token)
						end
				| Token _ ->
					match current_adj_list with
					| [] -> (Empty, Token.Token (Word_classe.Adjectif, ("", [])))
					| [adj] -> (Node (Word_classe.Adjectif, (List.fold_left gender_number_comparison ["e"; "i"] current_adj_informations_list), [Leaf adj]), current_token)
					| _ -> (Node (Word_classe.MultipleAdj, (List.fold_left gender_number_comparison ["e"; "i"] current_adj_informations_list), ( make_adj_tree_list current_adj_list current_adj_informations_list ) ), current_token)
			end
	in
	aux' s [] [] ( Token.Token (Word_classe.Adjectif, ("", [])) )


and get_verb s =
	if s.indice >= s.length then
		(Error Empty_sentence, Token.Token (Word_classe.Verbe, ("", [])))
	else
		begin
		let token = s.t_a.(s.indice) in
		match token with
		| Unknown w -> (Error ( Unknown_word w ), token)
		| Token (Word_classe.Verbe, (verb, informations)) -> pass s; (Node (Word_classe.Verbe, informations, [Leaf verb]), token)
		| Token (_, (word, _)) -> (Error (Missing (Word_classe.Verbe, word)), token)
		end

and get_pronom_sujet s =
	if s.indice >= s.length then
		(Error Empty_sentence, Token.Token (Word_classe.Pronom_sujet, ("", [])))
	else
		begin
		let token = s.t_a.(s.indice) in
		match token with
		| Unknown w -> (Error ( Unknown_word w ), token)
		| Token (Word_classe.Pronom_sujet, (pronom, informations)) -> pass s; (Node (Word_classe.Pronom_sujet, informations, [Leaf pronom]), token)
		| Token (_, (word, _)) -> (Error (Missing (Word_classe.Pronom_sujet, word)), token)
		end

let string_to_item s =
	let token_list = Token.sentence_to_token_list s |> Token.all_possibility in
	let rec aux l =
		match l with
		| [] -> []
		| h :: t -> let token_array = h |> Utility.array_of_list in { t_a = token_array; indice = 0; length = Array.length token_array } :: aux t
	in
	aux token_list

let item_list_to_syntax_tree_list item =
	List.map get_syntax_tree item

let string_to_syntax_tree_list s =
	s |> string_to_item |> item_list_to_syntax_tree_list
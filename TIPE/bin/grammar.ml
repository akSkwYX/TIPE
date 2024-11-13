(* open Utility
open Trie
open Tokenization as Token
open Word_classe as WC *)

type error =
	| Empty_sentence
	| Unrecognized of string
	| Missing of (Word_classe.word_classe * string)
	| Accord of (Word_classe.word_classe * string) * (Word_classe.word_classe * string)
	| Conjuguaison of string
	| No (* Idk if it's really good to do this but i need it *)

let string_of_error e =
	match e with
	| Empty_sentence -> "Empty sentence"
	| Unrecognized s -> "Unrecognized : " ^ s
	| Missing (wc, w) -> "Missing " ^ (Word_classe.word_classe_to_string wc) ^ " got : " ^ w
	| Accord ((wc1, w1), (wc2, w2)) -> "Accord between " ^ (Word_classe.word_classe_to_string wc1) ^ " " ^ w1 ^ " and " ^ (Word_classe.word_classe_to_string wc2) ^ " " ^ w2
	| Conjuguaison s -> "Conjuguaison : " ^ s
	| No -> "No"

type syntax_tree =
	| Node of Word_classe.word_classe * string list * syntax_tree list
	| Leaf of string
	| Error of error

let rec syntax_tree_in_tex_aux file tree =
	match tree with
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
	| Error _ -> false
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

let check_det_noun det_informations noun_informations =
	let (success, result_informations) = check_gender_number det_informations noun_informations in
	if success then
		(true, result_informations)
	else
		(false, [])

(* On suppose que les arbres sont déjà bien accordé *)
let check_adjectives adj_informations_1 adj_informations_2 =
	match adj_informations_1, adj_informations_2 with
	| _, [] -> true, adj_informations_1
	| [], _ -> true, adj_informations_2
	| _ ->
		let (success, result_informations) = check_gender_number adj_informations_1 adj_informations_2 in
		if success then
			(true, result_informations)
		else
			(false, [])

let check_noun_adjective noun_informations adj_informations =
	match noun_informations, adj_informations with
	| _, [] -> (true, noun_informations)
	| _ ->
		let (success, result_informations) = check_gender_number noun_informations adj_informations in
		if success then
			(true, result_informations)
		else
			(false, [])

let check_subject_verb subject_informations verb_informations =
	let get_verb_person verb_informations =
		match verb_informations with
		| intransitif :: transitif_direct :: transitif_indirect :: pronominal :: impersonnel :: auxiliaire_etre :: auxiliaire_avoir :: "Y" :: []
		| intransitif :: transitif_direct :: transitif_indirect :: pronominal :: impersonnel :: auxiliaire_etre :: auxiliaire_avoir :: "P" :: []
		| intransitif :: transitif_direct :: transitif_indirect :: pronominal :: impersonnel :: auxiliaire_etre :: auxiliaire_avoir :: "Q" :: _
			-> failwith "get_verb_person : Not implemented yet"
		| intransitif :: transitif_direct :: transitif_indirect :: pronominal :: impersonnel :: auxiliaire_etre :: auxiliaire_avoir :: temps :: person :: []
			-> person
		| _ -> failwith "get_verb_person : Doesn't receive a correct Verb"
	in
	let list_verb_person = verb_informations |> get_verb_person |> String.split_on_char ',' in
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
	| person :: _ :: number :: [], _ -> failwith "check_subject_verb : Doesn't receive a correct Verb"
	| _ -> failwith "check_subject_verb : Doesn't receive a correct Subject"

let is_conjugue verb_informations =
  match verb_informations with
		| intransitif :: transitif_direct :: transitif_indirect :: pronominal :: impersonnel :: auxiliaire_etre :: auxiliaire_avoir :: "Y" :: []
		| intransitif :: transitif_direct :: transitif_indirect :: pronominal :: impersonnel :: auxiliaire_etre :: auxiliaire_avoir :: "P" :: []
		| intransitif :: transitif_direct :: transitif_indirect :: pronominal :: impersonnel :: auxiliaire_etre :: auxiliaire_avoir :: "Q" :: _
			-> false
		| intransitif :: transitif_direct :: transitif_indirect :: pronominal :: impersonnel :: auxiliaire_etre :: auxiliaire_avoir :: temps :: person :: []
			-> true
		| _ -> failwith "get_verb_person : Doesn't receive a correct Verb"

let check_verbal_group subject_tree verb_tree =
	let get_person informations =
		match informations with
		| g :: "s" :: [] -> ["O3"; g; "s"]
		| g :: "p" :: [] -> ["O3"; g; "p"]
		| _ -> failwith "get_person : informations doesn't match a gn"
	in
	match subject_tree, verb_tree with
	| Node (Word_classe.Sujet, informations_subject, [ Node (Word_classe.Pronom_sujet, _, _) ] ), Node (Word_classe.Verbe, informations_verb, [ Leaf verb ]) ->
    if is_conjugue informations_verb then
      begin
        let (success, informations) = check_subject_verb informations_subject informations_verb in
        if success then
          (true, informations, No)
        else
          (false, [], Conjuguaison verb)
      end
    else
      (false, [], Conjuguaison verb)
	| Node (Word_classe.Sujet, informations_subject, [ Node (Word_classe.GN, informations_gn, _) ]), Node (Word_classe.Verbe, informations_verb, [ Leaf verb ]) ->
    if is_conjugue informations_verb then
      begin 
        let (success, informations) = check_subject_verb (get_person informations_subject) informations_verb in
        if success then
          (true, informations, No)
        else
          (false, [], Conjuguaison verb)
      end
    else
      (false, [], Conjuguaison verb)
	| _ -> syntax_tree_list_in_tex [subject_tree; verb_tree]; failwith "check_verbal_group : Doesn't receive a correct Sujet and a Verbe"
	
let check_nominal_group det_tree adj_tree nom_tree adj_tree_2 =
	let get_first_adjectif_of_tree tree =
		match tree with
		| Node (Word_classe.MultipleAdj, _, [Leaf adj; _]) -> adj
		| Node (Word_classe.Adjectif, _, [Leaf adj]) -> adj
		| _ -> failwith "get_first_adjectif_of_tree : doesn't match an Leaf adj tree"
	in
	match adj_tree, adj_tree_2 with
	| Node (Word_classe.MultipleAdj, informations_adj_1, _), Node (Word_classe.MultipleAdj, informations_adj_2, _)
	| Node (Word_classe.MultipleAdj, informations_adj_1, _), Node (Word_classe.Adjectif, informations_adj_2, _)
	| Node (Word_classe.Adjectif, informations_adj_1, _), Node (Word_classe.MultipleAdj, informations_adj_2, _)
	| Node (Word_classe.Adjectif, informations_adj_1, _), Node (Word_classe.Adjectif, informations_adj_2, _)
		->	let (success_adjectives, result_informations_adjectives) = check_adjectives informations_adj_1 informations_adj_2 in
			if success_adjectives then
				match det_tree, nom_tree with
				| Node (Word_classe.Determinant, informations_det, [Leaf det]), Node (Word_classe.Nom, informations_nom, [Leaf nom])
					->	let (success_det_noun, result_informations_det_noun) = check_det_noun informations_det informations_nom in
						if success_det_noun then
							let (success_adjectives_noun, result_informations_adjectives_noun) = check_noun_adjective result_informations_det_noun result_informations_adjectives in
							if success_adjectives_noun then
								(true, result_informations_adjectives_noun, No)
							else
								let first_adj_tree_1 = get_first_adjectif_of_tree adj_tree in
								if first_adj_tree_1 = "" then
									(false, [], Accord ((Word_classe.Adjectif, (get_first_adjectif_of_tree adj_tree_2)), (Word_classe.Nom, nom)))
								else
									(false, [], Accord ((Word_classe.Adjectif, first_adj_tree_1), (Word_classe.Nom, nom)))
						else
							(false, [], Accord ((Word_classe.Determinant, det), (Word_classe.Nom, nom)))
				| _ -> failwith "check_nominal_group : Doesn't receive a Determinant and a Nom"
			else
				(false, [], Accord ((Word_classe.Adjectif, (get_first_adjectif_of_tree adj_tree)), (Word_classe.Adjectif, (get_first_adjectif_of_tree adj_tree_2))))
	| _ -> failwith "check_nominal_group : Doesn't receive a correct Adjectif tree"

(* Increasing the index of sentence which is in correcting and return if it receive the expected state *)
let pass sentence =
	sentence.indice <- sentence.indice + 1

(* First call of mutual recursivity for checking *)
let rec get_syntax_tree sentence =
	if sentence.indice > sentence.length then
		Error Empty_sentence
	else
		get_verbal_group sentence

and get_verbal_group s =
	let subject_tree = get_subject s in
	match subject_tree with
	| Error e -> Error e
	| _ ->
		let verb_tree = get_verb s in
		match verb_tree with
		| Error e -> Error e
		| _ ->
			let (success, informations, error) = check_verbal_group subject_tree verb_tree in
			if success then
				Node (Word_classe.GV, informations, [subject_tree; verb_tree])
			else
				Error error

and get_subject s =
	if s.indice >= s.length then
		Error Empty_sentence
	else
		begin
		let wc_token = Token.get_word_classe s.t_a.(s.indice) in
		if wc_token = Word_classe.Determinant || wc_token = Word_classe.Nom || wc_token = Word_classe.Adjectif then
			let nominal_group_tree = get_nominal_group s in
			match nominal_group_tree with
			| Error e -> Error e
			| Node (Word_classe.GN, informations, _) -> Node (Word_classe.Sujet, informations, [nominal_group_tree])
			| _ -> failwith "get_subject : wrong tree, waiting for a GN"
		else if Token.get_word_classe s.t_a.(s.indice) = Word_classe.Pronom_sujet then
			let pronom_sujet_tree = get_pronom_sujet s in
			match pronom_sujet_tree with
			| Error e -> Error e
			| Node (Word_classe.Pronom_sujet, informations, _) -> Node (Word_classe.Sujet, informations, [pronom_sujet_tree])
			| _ -> failwith "get_subject : wrong tree, waiting for a Pronom_sujet"
		else
			Error (Missing (Word_classe.Sujet, Token.get_word s.t_a.(s.indice)))
		end

and get_nominal_group s =
	let det_tree = get_determinant s in
	match det_tree with
	| Error e -> Error e
	| _ ->
		let adj_tree = get_adjectifs s in
		match adj_tree with
		| Error e -> Error e
		| _ ->
			let nom_tree = get_nom s in
			match nom_tree with
			| Error e -> Error e
			| _ ->
				let adj_tree_2 = get_adjectifs s in
				match adj_tree_2 with
				| Error e -> Error e
				| _ ->
					let (success, informations, error) = check_nominal_group det_tree adj_tree nom_tree adj_tree_2 in
					if success then
						match adj_tree, adj_tree_2 with
						| Node (Word_classe.Adjectif, [], [Leaf ""]), Node (Word_classe.Adjectif, [], [Leaf ""]) -> Node (Word_classe.GN, informations, [det_tree; nom_tree])
						| Node (Word_classe.Adjectif, [], [Leaf ""]), _ -> Node (Word_classe.GN, informations, [det_tree; nom_tree; adj_tree_2])
						| _, Node (Word_classe.Adjectif, [], [Leaf ""]) -> Node (Word_classe.GN, informations, [det_tree; adj_tree; nom_tree])
						| _ -> Node (Word_classe.GN, informations, [det_tree; adj_tree; nom_tree; adj_tree_2])
					else
						Error error

and get_determinant s =
	if s.indice >= s.length then
		Error Empty_sentence
	else
		begin
		let token = s.t_a.(s.indice) in
		match token with
		| Unknown w -> Error ( Unrecognized w )
		| Token (Word_classe.Determinant, (det, informations)) -> pass s; Node (Word_classe.Determinant, informations, [Leaf det])
		| Token (_, (word, _)) -> Error (Missing (Word_classe.Determinant, word))
		end

and get_nom s =
	if s.indice >= s.length then
		Error Empty_sentence
	else
		begin
		let token = s.t_a.(s.indice) in
		match token with
		| Unknown w -> Error ( Unrecognized w )
		| Token (Word_classe.Nom, (nom, informations)) -> pass s; Node (Word_classe.Nom, informations, [Leaf nom])
		| Token (_, (word, _)) -> Error (Missing (Word_classe.Nom, word))
		end

and get_adjectifs s =
	if s.indice >= s.length then
		Error Empty_sentence
	else if Token.get_word_classe s.t_a.(s.indice) = Adjectif then
		let token = s.t_a.(s.indice) in
		if (s.indice + 1) < s.length && Token.get_word_classe s.t_a.(s.indice + 1) = Word_classe.Adjectif then
			match token with
			| Unknown w -> Error ( Unrecognized w )
			| Token (Word_classe.Adjectif, (adj, informations)) ->
				begin
				pass s;
				let next = get_adjectifs s in
				match next with
				| Node (Word_classe.Adjectif, informations_next, l)
				| Node (Word_classe.MultipleAdj, informations_next, l) ->
					let (success, result_informations) = check_adjectives informations informations_next in
						if success then
							Node (Word_classe.MultipleAdj, result_informations, [Node (Word_classe.Adjectif, informations, [Leaf adj]); next])
						else
							Error (Accord ((Word_classe.Adjectif, adj), (Word_classe.Adjectif, Token.get_word s.t_a.(s.indice + 1))))
				| _ -> next
				end
			| Token (_, (word, _)) -> Error (Missing (Word_classe.Adjectif, word))
		else
			match token with
			| Unknown w -> Error ( Unrecognized w )
			| Token (Word_classe.Adjectif, (adj, informations)) -> pass s; Node (Word_classe.Adjectif, informations, [Leaf adj])
			| Token (_, (word, _)) -> Error (Missing (Word_classe.Adjectif, word))
	else
		Node (Word_classe.Adjectif, [], [Leaf ""])

and get_verb s =
	if s.indice >= s.length then
		Error Empty_sentence
	else
		begin
		let token = s.t_a.(s.indice) in
		match token with
		| Unknown w -> Error ( Unrecognized w )
		| Token (Word_classe.Verbe, (verb, informations)) -> pass s; Node (Word_classe.Verbe, informations, [Leaf verb])
		| Token (_, (word, _)) -> Error (Missing (Word_classe.Verbe, word))
		end

and get_pronom_sujet s =
	if s.indice >= s.length then
		Error Empty_sentence
	else
		begin
		let token = s.t_a.(s.indice) in
		match token with
		| Unknown w -> Error ( Unrecognized w )
		| Token (Word_classe.Pronom_sujet, (pronom, informations)) -> pass s; Node (Word_classe.Pronom_sujet, informations, [Leaf pronom])
		| Token (_, (word, _)) -> Error (Missing (Word_classe.Pronom_sujet, word))
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
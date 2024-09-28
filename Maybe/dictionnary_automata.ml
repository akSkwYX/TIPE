let transition_new_word word last_word =
	let word_length = String.length word in
	if word_length = 1 then [("", word.[0], word)] else
	let last_word_length = String.length last_word in
	let rec seek_first_diff i =
		if i = word_length || i = last_word_length || word.[i] <> last_word.[i] then
			i
		else
			seek_first_diff (i+1)
	in
	let rec aux word last_word i transitions =
		if i = word_length then
			transitions
		else
			aux word last_word (i+1) ((String.sub word 0 (i), word.[i], String.sub word 0 (i+1))::transitions)
	in
	aux word last_word (seek_first_diff 0) []

(* let read_dictionnary (nom_fichier:string) =
  let fichier = open_in nom_fichier in
  let rec lecture_ligne_a_ligne last_word transitions finals =
      match In_channel.input_line fichier with
      | None -> (transitions, finals)
      | Some word ->
				if word.[0] = last_first_letter then
					(
					(* print_string word; print_newline (); *)
					lecture_ligne_a_ligne dictionnary (word::list_by_first_letter) last_first_letter
					)
				else
					(
					(* print_newline (); print_newline ();
					List.iter (fun word -> print_string word; print_newline ()) list_by_first_letter ; print_newline (); *)
					list_by_first_letter::(lecture_ligne_a_ligne dictionnary [] word.[0])
					)
  in
  lecture_ligne_a_ligne [] [] 'a' *)

type state = string
type symbol = char
type transition = state * symbol * state

type dfa = {
	states : state list;
	alphabet : symbol list;
	transitions : transition list;
	start_state : state;
	accept_states : state list;
}

let transition_function transitions state symbol =
	List.find_opt (fun (s, sym, _) -> s = state && sym = symbol) transitions
	|> Option.map (fun (_, _, new_state) -> new_state)

let rec accepts dfa current_state input =
	match input with
	| [] -> List.mem current_state dfa.accept_states
	| symbol::rest ->
		match transition_function dfa.transitions current_state symbol with
		| Some new_state -> accepts dfa new_state rest
		| None -> false

let dictionnary_dfa = {
	states = [];
	alphabet = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z';
							'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M'; 'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z';
							'-'
						];
	transitions = [];
	start_state = "";
	accept_states = [];
}

let print_dictionnary = List.iter (fun l -> (List.iter (fun word -> Printf.printf "%s-" word) l; Printf.printf "\n\n") )
(* let dictionnary = read_dictionnary "francais.csv"
let () = print_dictionnary dictionnary *)

let print_transition = List.iter (fun (s1, sym, s2) -> print_string s1; print_string " "; print_char sym; print_string " "; print_string s2; print_newline ())

(* let () = transition_new_word "a" "" 
						@ transition_new_word "abaissable" "a" 
						@ transition_new_word "abaissables" "abaissable" 
						@ transition_new_word "abaissai" "abaissables" 
						|> print_transition *)
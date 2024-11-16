(* open Utility
open Word_classe as WC *)

type token = 
	| Token of ( Word_classe.word_classe * (string * string list) )
	| Unknown of string

let string_of_token t =
  match t with
  | Token (Word_classe.Determinant, (s, l)) ->  "D " ^ s ^ " " ^ (Utility.string_of_string_list l)
  | Token (Word_classe.Nom, (s, l)) ->  "N " ^ s ^ " " ^ (Utility.string_of_string_list l)
  | Token (Word_classe.Adjectif, (s, l)) ->  "A " ^ s ^ " " ^ (Utility.string_of_string_list l)
  | Token (Word_classe.S, (s, l)) ->  "S : "
  | Token (Word_classe.GV, (s, l)) ->  "GV : "
  | Token (Word_classe.GN, (s, l)) ->  "GN : "
  | Token (Word_classe.MultipleAdj, (s, l)) ->  "MA : "
  | Token (Word_classe.Pronom_sujet, (s, l)) ->  "PS " ^ s ^ " " ^ (Utility.string_of_string_list l)
  | Token (Word_classe.Sujet, (s, l)) ->  "SU " ^ s ^ " " ^ (Utility.string_of_string_list l)
  | Token (Word_classe.Verbe, (s, l)) ->  "V " ^ s ^ " " ^ (Utility.string_of_string_list l)
  | Token (Word_classe.Unknown, (s, l)) ->  failwith "string_of_token : Unknown token declared as Token"
  | Unknown w ->  "Unknown " ^ w

let print_token_list =
  List.iter (fun x -> print_string (string_of_token x); print_string " | ")

let print_token_list_list =
  List.iter (fun x -> print_token_list x; print_newline ())

let get_word token =
  match token with
  | Token (Word_classe.Determinant, (s, l)) -> s
  | Token (Word_classe.Nom, (s, l)) -> s
  | Token (Word_classe.Adjectif, (s, l)) -> s
  | Token (Word_classe.Sujet, (s, l)) -> s
  | Token (Word_classe.Verbe, (s, l)) -> s
  | Token (Word_classe.Pronom_sujet, (s, l)) -> s
  | Token (Word_classe.S, (s, l)) -> failwith "get_word : trying to get a word from a S"
  | Token (Word_classe.GV, (s, l)) -> failwith "get_word : trying to get a word from a GV"
  | Token (Word_classe.GN, (s, l)) -> failwith "get_word : trying to get a word from a GN"
  | Token (Word_classe.MultipleAdj, (s, l)) -> failwith "get_word : trying to get a word from a MultipleAdj"
  | Token (Word_classe.Unknown, (s, l)) -> failwith "get_word : Unknown token declared as Token"
  | Unknown w -> w

let get_information token =
  match token with
  | Token (Word_classe.Determinant, (s, l)) -> l
  | Token (Word_classe.Nom, (s, l)) -> l
  | Token (Word_classe.Adjectif, (s, l)) -> l
  | Token (Word_classe.Sujet, (s, l)) -> l
  | Token (Word_classe.Verbe, (s, l)) -> l
  | Token (Word_classe.Pronom_sujet, (s, l)) -> l
  | Token (Word_classe.GN, (s, l)) -> l
  | Token (Word_classe.MultipleAdj, (s, l)) -> l
  | Token (Word_classe.S, (s, l)) -> failwith "get_information : trying to get a information from a S"
  | Token (Word_classe.GV, (s, l)) -> failwith "get_information : trying to get a information from a GV"
  | Token (Word_classe.Unknown, (s, l)) -> failwith "get_information : Unknown token declared as Token"
  | Unknown _ -> failwith "get_information : tring to get a information from a Unknown"

let get_gender token =
  match token with
  | Token (Word_classe.Adjectif, (_, l)) -> List.nth l 1 (* Wanting catching error but no success trying later *)
  | Token (Word_classe.Nom, (_, l)) -> List.nth l 1
  | Token (Word_classe.Determinant, (_, l)) -> List.nth l 1
  | Token (Word_classe.Pronom_sujet, (_, l)) -> List.nth l 2
  | _ -> failwith "get_gender : not something which have a gender"

let get_number token =
  match token with
  | Token (Word_classe.Adjectif, (_, l)) -> List.nth l 2 (* Wanting catching error but no success trying later *)
  | Token (Word_classe.Nom, (_, l)) -> List.nth l 2
  | Token (Word_classe.Determinant, (_, l)) -> List.nth l 2
  | Token (Word_classe.Pronom_sujet, (_, l)) -> List.nth l 3
  | _ -> failwith "get_number : not something which have a number"

let get_word_classe token =
  match token with
  | Token (Word_classe.Determinant, (s, l)) -> Word_classe.Determinant
  | Token (Word_classe.Nom, (s, l)) -> Word_classe.Nom
  | Token (Word_classe.Adjectif, (s, l)) -> Word_classe.Adjectif
  | Token (Word_classe.Sujet, (s, l)) -> Word_classe.Sujet
  | Token (Word_classe.Verbe, (s, l)) -> Word_classe.Verbe
  | Token (Word_classe.Pronom_sujet, (s, l)) -> Word_classe.Pronom_sujet
  | Token (Word_classe.S, (s, l)) -> Word_classe.S
  | Token (Word_classe.GV, (s, l)) -> Word_classe.GV
  | Token (Word_classe.GN, (s, l)) -> Word_classe.GN
  | Token (Word_classe.MultipleAdj, (s, l)) -> Word_classe.MultipleAdj
  | Token (Word_classe.Unknown, (s, l)) -> failwith "get_word_classe : Unknown token declared as Token"
  | Unknown _ -> Unknown

let sentence_to_list s =
	let rec aux s_l l =
		match s_l with
		| [] -> l
		| ' ' :: t -> aux t l
		(* Just handeling ' case which is quite annoying more details on other case*)
		| '\'' :: t ->
			let rec find_word l w length_w =
				match l with
				| [] -> w, length_w
				| ' ' :: t | '\'' :: t -> w, length_w
				| c :: t -> find_word t (c::w) (length_w + 1)
			in
			let w, length_w = find_word (List.tl s_l) [] 0
			in
			aux (Utility.list_without_x_last_char (length_w+1) s_l) ((Utility.string_of_char_list (w @ ['\'']))::l)
		(* Finding a new word ! *)
		| c :: t ->
			(* Getting the whole word *)
			let rec find_word l w length_w =
				match l with
				| [] -> w, length_w
				| ' ' :: t | '\'' :: t -> w, length_w
				| c :: t -> find_word t (c::w) (length_w + 1)
			in
			let w, length_w = find_word s_l [] 0
			in
			(* Reccursive call with adding the whole word to list of word and jumping in the string from a step of word found length *)
			aux (Utility.list_without_x_last_char length_w s_l) ((Utility.string_of_char_list w)::l)
	in
	aux (List.rev (Utility.char_list_of_string s) ) []


(* Using precedent function and transforming the sentence to a token list list which is usefull for manipuling the sentence after *)
let sentence_to_token_list (s:string) :token list list =
  let rec match_information i possibility word =
    match i with
    | [] -> possibility
    | ("D"::complementary_information) :: tl -> match_information tl (Token ( Word_classe.Determinant, (word, complementary_information) ) :: possibility) word
    | ("N"::complementary_information) :: tl -> match_information tl (Token ( Word_classe.Nom, (word, complementary_information) ) :: possibility) word
    | ("A"::complementary_information) :: tl -> match_information tl (Token ( Word_classe.Adjectif, (word, complementary_information) ) :: possibility) word
    | ("V"::complementary_information) :: tl -> match_information tl (Token ( Word_classe.Verbe, (word, complementary_information) ) :: possibility) word
    | ("Os"::complementary_information) :: tl -> match_information tl (Token ( Word_classe.Pronom_sujet, (word, complementary_information) ) :: possibility) word
    | _ when possibility = [] -> [Unknown "Unknown"]
    | _ -> failwith "match_information : wrong information"
  in
	let get_correction_possibility_for_word (word:string) :token list =
		let rec index_loop possibility index =
			if index = String.length word then possibility
			else
				let rec letter_loop possibility letter =
					match letter with
					| '{' -> possibility
					| _ ->
            if String.get word index = letter then
              letter_loop possibility (Char.chr (Char.code letter + 1))
            else
              begin
                let modified_word = Utility.replace_char_in_string word index letter in
                let (is_word, information) = Trie.trie_search Dictionnary.dictionnary modified_word in
                if is_word then
                  letter_loop ((modified_word, information) :: possibility) (Char.chr (Char.code letter + 1))
                else
                  letter_loop possibility (Char.chr (Char.code letter + 1))
              end
				in
				index_loop (letter_loop possibility 'a' @ possibility) (index + 1)
		in
		Utility.list_list_to_list (List.map (fun (word, informations) -> match_information informations [] word) (index_loop [] 0))
	in
	let rec aux (list_word:string list) (list_token:token list list) =
		match list_word with
		| [] -> list_token
		| word :: t ->
			(* Searching in dictionnary if word exists and its information if it does *)
			let (is_word, information) = Trie.trie_search Dictionnary.dictionnary word in
			if is_word then
				(* Return all possibility for a word *)
        aux t ((match_information information [] word) :: list_token)
			else
        begin
          print_string ("Not a correct sentence : Unknown word " ^ word ^ "\nTrying to find correction\n");
          let correction_possibilitys = get_correction_possibility_for_word word in
          aux t (get_correction_possibility_for_word word :: list_token)
        end
	in
	List.rev (aux (sentence_to_list s) [])

(* Returning all sentence possible with all meaning of each word
Exemple : "rouge" can be a noun or an adjective *)
let all_possibility (l:token list list) :token list list =
	(* Firstly itterating over the token list of list of possibility for words *)
	let rec first_floor_course (l:token list list) (acc: token list list) :token list list =
		match l with
		| [] -> acc
		| [h] :: t -> first_floor_course t (List.map (fun x -> h :: x) acc)
		| h :: t ->
			(* Secondly itterating over the possibilitys of words *)
			let rec second_floor_course (l:token list) :token list list =
				match l with
				| [] -> []
				(* For each possibility, adding this possibility to all sentence possible already found *)
				| hd :: tl -> List.map (fun x -> hd :: x) acc @ second_floor_course tl
			in
			first_floor_course t (second_floor_course h)
	in
  List.map (List.rev) (first_floor_course l [[]])
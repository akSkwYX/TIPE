let ( @. ) = Fun.compose

let word_without_first_char word =
	String.sub word 1 (String.length word - 1)

let word_without_last_char word =
	String.sub word 0 (String.length word - 1)

let string_without_x_last_char string x =
	String.sub string 0 (String.length string - x)

let char_list_of_string string =
	string |> String.to_seq |> List.of_seq

let string_of_char_list list =
	list |> List.to_seq |> String.of_seq

let replace_char_in_string (word:string) (index:int) (letter:char) :string =
	let word_bytes = Bytes.of_string word in
	Bytes.set word_bytes index letter;
	Bytes.to_string word_bytes

let print_char_list = List.iter (fun x -> print_char x; print_string " | ")

let print_string_list = List.iter (fun x -> print_string x; print_string " | ")

let print_bool = print_endline @. string_of_bool

let array_of_list l = List.to_seq l |> Array.of_seq

let string_of_string_list l =
	match l with
	| [] -> ""
	| _ -> string_without_x_last_char (List.fold_left (fun acc x -> if x <> "_" then acc ^ x ^  ", " else acc ^ "\\_" ^ ", ") "" l) 2

let string_of_option o =
	match o with
	| None -> failwith "option_to_string : None"
	| Some x -> x

let rec list_without_x_last_char x list =
	match list with
	| [] -> []
	| h :: t when x = 0 -> list
	| h :: t -> list_without_x_last_char (x - 1) t

let list_list_to_list list =
  List.fold_left (fun acc x -> acc @ x) [] list

let print_string_list_list list =
  List.iter (fun x -> print_string_list x; print_newline ()) list
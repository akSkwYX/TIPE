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

let print_int_matrix m = Array.iter (fun x -> Array.iter (fun y -> print_int y; print_string " ") x; print_newline ()) m

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


(**
  [join_2_lists list1 list2] returns a list of pairs where each element of [list1]
  is paired with each element of [list2].

  @param list1 The first list.
  @param list2 The second list.
  @return A list of pairs.
*)
let rec join_2_lists list1 list2 =
  let rec pair_with_all x lst =
    match lst with
    | [] -> []
    | y::ys -> (x, y) :: pair_with_all x ys
  in
  match list1 with
  | [] -> []
  | x::xs -> (pair_with_all x list2) @ (join_2_lists xs list2)

(**
  [join_3_lists list1 list2 list3] returns a list of 3-tuple where each element of [list1], [list2], [list3] are joined together.

  @param list1 The first list.
  @param list2 The second list.
  @param list3 The third list.
  @return List of 3-tuple.


  Example:
  join_3_lists [1; 2] ["a"; "b"] [true; false] = [(1, "a", true); (1, "a", false); (1, "b", true); (1, "b", false); (2, "a", true); (2, "a", false); (2, "b", true); (2, "b", false)]
*)
let join_3_lists list1 list2 list3 =
  let rec pair_with_all x y lst =
    match lst with
    | [] -> []
    | z::zs -> (x, y, z) :: pair_with_all x y zs
  in
  let rec combine_two_lists l1 l2 =
    match l1 with
    | [] -> []
    | x::xs -> (List.concat (List.map (fun y -> pair_with_all x y list3) l2)) @ (combine_two_lists xs l2)
  in
  combine_two_lists list1 list2

(**
  [join_4_lists list1 list2 list3 list4] returns a list of 4-tuple where each element of [list1], [list2], [list3], [list4] are joined together.

  @param list1 The first list.
  @param list2 The second list.
  @param list3 The third list.
  @param list4 The fourth list.
  @return List of 4-tuple.

  
  Example:
  join_4_lists [1; 2] ["a"; "b"] [true; false] [3; 4] = [(1, "a", true, 3); (1, "a", true, 4); (1, "a", false, 3); (1, "a", false, 4); (1, "b", true, 3); (1, "b", true, 4); (1, "b", false, 3); (1, "b", false, 4); (2, "a", true, 3); (2, "a", true, 4); (2, "a", false, 3); (2, "a", false, 4); (2, "b", true, 3); (2, "b", true, 4); (2, "b", false, 3); (2, "b", false, 4)]
*)
let join_4_lists list1 list2 list3 list4 =
  let rec pair_with_all x y z lst =
    match lst with
    | [] -> []
    | w::ws -> (x, y, z, w) :: pair_with_all x y z ws
  in
  let rec combine_three_lists l1 l2 l3 =
    match l1 with
    | [] -> []
    | x::xs -> (List.concat (List.map (fun y -> List.concat (List.map (fun z -> pair_with_all x y z list4) l3)) l2)) @ (combine_three_lists xs l2 l3)
  in
  combine_three_lists list1 list2 list3
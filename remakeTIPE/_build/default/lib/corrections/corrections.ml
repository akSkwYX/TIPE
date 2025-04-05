open Syntax_tree

let rad_dict = Dictionary.rad_dictionary

let distanceLevenshtein word_1 word_2 =
  let len_1 = String.length word_1 in
  let len_2 = String.length word_2 in
  
  let matrix_distance = Array.make_matrix 2 (len_2 + 1) 0 in
  matrix_distance.(1).(0) <- 1;
  for j = 1 to len_2 do
    matrix_distance.(0).(j) <- j
  done;
  
  for i = 1 to len_1 do
    for j = 1 to len_2 do
      let cost = if word_1.[i - 1] = word_2.[j - 1] then 0 else 1 in
      matrix_distance.(1).(j) <- min (min (matrix_distance.(0).(j) + 1) (matrix_distance.(1).(j - 1) + 1)) (matrix_distance.(0).(j - 1) + cost)
    done;
    for j = 0 to len_2 do
      matrix_distance.(0).(j) <- matrix_distance.(1).(j)
    done;
    matrix_distance.(1).(0) <- i+1
  done;
  matrix_distance.(1).(len_2)

let get_possibilitys word_class word =
  List.sort (fun x y -> distanceLevenshtein (Token.get_word_class x) word - distanceLevenshtein (Token.get_word_class y) word)
  (List.filter_map (fun x -> if Tags.get_word_class x = word_class then Some (Token.create word_class word x) else None)
  (Trie.search rad_dict word))

let correction checkings_fun construct_tree_fun to_correct args =
  let to_correct_token =
    match to_correct with
    | Node (token, _) -> token
    | _ -> failwith "corrections.ml/correction : to_correct is not a node of a tree"
  in
  let res = checkings_fun to_correct_token args in
  match res with
  | [] ->
    begin
    let possibilitys = get_possibilitys (Token.get_word_class to_correct_token) (Token.get_word to_correct_token) in
    let corrections = List.filter_map
                      (fun x -> let tags_res = checkings_fun x args in
                                if tags_res <> [] then
                                  Some (construct_tree_fun tags_res x args)
                                else None
                      )
                      possibilitys
    in
    if corrections = [] then [Error "Unable to correct"] else corrections
    end
  | _ -> [construct_tree_fun res to_correct_token args]

let correct_verbal_group subject verb =
  let checkings_fun token args =
    Checkings.check_subject_verb args (Node (token, [Leaf token]))
  in
  let construct_tree_fun tags token args =
    Node (VERBAL_GROUP (Syntax_tree.st_get_word args ^ " " ^ Token.get_word token, tags),
         [args; Node (token, [Leaf token])])
  in
  correction checkings_fun construct_tree_fun verb subject


let correct_nominal_group determiner adjective noun adjective2 =
  let checkings_fun token args =
    match token, args with
    | (Token.NOUN _, (place_of_adjective, det :: adj1 :: adj2 :: [])) -> Checkings.check_nominal_group det adj1 (Node(token, [Leaf token])) adj2
    | Token.ADJECTIVE _, (place_of_adjective, det :: noun :: adj :: []) -> Checkings.check_nominal_group det (Node(token, [Leaf token])) noun adj 
    | _ -> failwith "corrections.ml/correct_nominal_group/checkings_fun : Try to correct something else than noun or adj"
  in
  let construct_tree_fun tags token args =
    match token, args with
    | Token.NOUN _, (place_of_adjective, det :: adj1 :: adj2 :: []) ->
      Node (NOMINAL_GROUP (Syntax_tree.st_get_word det ^ " " ^ Syntax_tree.st_get_word adj1 ^ " " ^ Token.get_word token ^ " " ^ Syntax_tree.st_get_word adj2, tags),
            [det; adj1; Node (token, [Leaf token]); adj2])
    | Token.ADJECTIVE _, (place_of_adjective, det :: noun :: adj :: []) ->
      if place_of_adjective = 1 then
        Node (NOMINAL_GROUP ((Syntax_tree.st_get_word det) ^ " " ^ (Token.get_word token) ^ " " ^ (Syntax_tree.st_get_word noun) ^ " " ^ (Syntax_tree.st_get_word adj), tags),
              [det; Node (token, [Leaf token]); noun; adj])
      else if place_of_adjective = 2 then
        Node (NOMINAL_GROUP (Syntax_tree.st_get_word det ^ " " ^ Syntax_tree.st_get_word adj ^ " " ^ Syntax_tree.st_get_word noun ^ " " ^ Token.get_word token, tags),
              [det; adj; noun; Node (token, [Leaf token])])
      else
        failwith "corrections.ml/correct_nominal_group/construct_tree_fun : place_of_adjective have unexpected value"
    | _ -> failwith "corrections.ml/correct_nominal_group/construct_tree_fun : Don't match the token and the args"
  in
  let res = correction checkings_fun construct_tree_fun noun (-1, determiner :: adjective :: adjective2 :: []) in
  List.fold_left (fun acc x -> match x with
    | Node (NOMINAL_GROUP _, [det; adj1; noun; adj2]) -> (correction checkings_fun construct_tree_fun adj2 (2, det :: noun :: adj1 :: [])) @ acc
    | _ -> acc
  ) []
  (List.fold_left (fun acc x ->
    match x with
    | Node (NOMINAL_GROUP _, [det; adj1; noun; adj2]) -> (correction checkings_fun construct_tree_fun adj1 (1, det :: noun :: adj2 :: [])) @ acc
    | _ -> acc
  ) [] res )


let correct_adjectives adjective1 adjective2 =
  let checkings_fun token args =
    Checkings.check_adjectives (Node (token, [Leaf token])) args
  in
  let construct_tree_fun tags token args =
    Node (ADJECTIVE (Syntax_tree.st_get_word args ^ " " ^ Token.get_word token, tags), [args; Node (token, [Leaf token])])
  in
  correction checkings_fun construct_tree_fun adjective2 adjective1

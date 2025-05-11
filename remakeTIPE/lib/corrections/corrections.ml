open Syntax_tree

let rad_dict = Dictionary.rad_dictionary
let dict = Dictionary.dictionary

(* TODO : replace with functional *)
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

let compare word x y =
  distanceLevenshtein (Token.get_word x) word - distanceLevenshtein (Token.get_word y) word

let get_possibilitys word_class root_word =
  let l =  
  List.sort (compare root_word)
  (List.filter_map (fun x -> if Tags.get_word_class x = word_class then
                               Some (Token.create word_class (Tags.get_word x) (Tags.replace_word x root_word))
                             else 
                               None
                   )
  (Dictionary.find rad_dict root_word))
  in
  List.hd l :: List.hd (List.tl l) :: List.hd (List.tl (List.tl l)) :: []

let char_for_replace = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k"; "l"; "m";
                        "n"; "o"; "p"; "q"; "r"; "s"; "t"; "u"; "v"; "w"; "x";
                        "y"; "z"; "-"; "\'"; ""]
let correction_ort word =
  let (res, _, _) = String.fold_left (
    fun (res, beg_word, end_word) c ->
      let next_end_word = if String.length end_word > 1 then String.sub end_word 1 (String.length end_word - 1) else "" in
      (List.fold_left (
        fun acc x ->
          let word1 = beg_word ^ x ^ end_word in
          let tags_list1 = Dictionary.find dict word1 in
          let word2 = beg_word ^ x ^ String.make 1 c ^ end_word in
          let tags_list2 = Dictionary.find dict word2 in
          match tags_list1, tags_list2 with
          | [], [] -> acc
          | [], _ -> Token.make_tokens_from_tags_list word2 acc tags_list2
          | _, [] -> Token.make_tokens_from_tags_list word1 acc tags_list1
          | _, _ -> Token.make_tokens_from_tags_list word2 (Token.make_tokens_from_tags_list word1 acc tags_list1) tags_list2
         ) res char_for_replace
      , beg_word ^ String.make 1 c
      , next_end_word
      )
  ) ([], "", String.sub word 1 (String.length word - 1)) word in
  res

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
    let possibilitys = get_possibilitys (Token.get_word_class to_correct_token) (Tags.get_root (Token.get_tags to_correct_token)) in
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


let correct_determiner determiner noun =
  let checkings_fun token args =
    match Checkings.check_determiner_noun (Node (token, [Leaf token])) args with
    | [""; ""] | [_; ""] | [""; _] -> []
    | [x; y] -> [x; y]
    | _ -> failwith "corrections.ml/correct_determiner : check_determiner_noun returned unexpected result"
  in
  let construct_tree_fun tags token args =
    Node (token, [Leaf token])
  in
  correction checkings_fun construct_tree_fun determiner noun

let correct_adj adjective noun =
  let checkings_fun token args =
    match Checkings.check_adjective_noun (Node (token, [Leaf token])) args with
    | [""; ""] | [_; ""] | [""; _] -> []
    | [x; y] -> [x; y]
    | _ -> failwith "corrections.ml/correct_adj : check_adjective_noun returned unexpected result"
  in
  let construct_tree_fun tags token args =
    Node (token, [Leaf token])
  in
  correction checkings_fun construct_tree_fun adjective noun

let merge_gender_number tags new_tags =
  let gender1 = Tags.get_gender_default tags in
  let gender2 = Tags.get_gender_default new_tags in
  let number1 = Tags.get_number_default tags in
  let number2 = Tags.get_number_default new_tags in
  [ Checkings.check_gender gender1 gender2; Checkings.check_number number1 number2 ]

let rec correct_adjectives_noun adjective noun =
  match adjective with
  | Node (token, [Leaf _]) ->
    correct_adj adjective noun
  | Empty -> [adjective]
  | Node (ADJECTIVE (word, tags), children) ->
    begin
    let (l: (syntax_tree list * string * string list) list) = 
      List.fold_left (
        fun acc child ->
          let res = correct_adjectives_noun child noun in
          match acc with
          | [] -> List.map (fun x -> ([x], Syntax_tree.st_get_word x, Syntax_tree.st_get_tags x)) res
          | _ ->
            begin
            List.fold_left (
              fun acc2 (tree_list, word, tags) ->
                List.fold_left (
                  fun acc3 tree ->
                    match tree with
                    | Empty -> (tree :: tree_list, word, tags) :: acc3
                    | Node (ADJECTIVE (new_word, new_tags), _) ->
                      begin
                      match merge_gender_number tags new_tags with
                      | "" :: "" :: [] | "" :: _ :: [] | _ :: "" :: [] -> acc3
                      | x :: y :: [] -> (tree :: tree_list, word ^ " " ^ new_word , [x;y]) :: acc3
                      | _ -> failwith "corrections.ml/correct_adjectives_noun : merge_gender_number returned unexpected result"
                      end
                    | _ -> failwith "corrections.ml/correct_adjectives_noun : res is not a node of an adjective tree"
                ) [] res
            ) [] acc
            end
      ) [] children
    in
    List.rev_map (fun (children, word, tags) -> 
      Node (ADJECTIVE (word, tags), List.rev children)
    ) l
    end 
  | _ -> failwith "corrections.ml/correct_adjectives_noun : adjective is not a node of an adjective tree"


(*TODO : Replace current tags with tags from checkings.check_nominal_group*)

let correct_nominal_group determiner adjective1 noun adjective2 =
  let res_determiner = correct_determiner determiner noun in
  let res_adjective1 = correct_adjectives_noun adjective1 noun in
  let res_adjective2 = correct_adjectives_noun adjective2 noun in
  let wnoun = Syntax_tree.st_get_word noun in
  let tags = [Tags.get_gender_default (Syntax_tree.st_get_tags noun);
              Tags.get_number_default (Syntax_tree.st_get_tags noun)] in
  List.fold_left (
    fun acc1 det ->
      let wdet = Syntax_tree.st_get_word det in
      match res_adjective1 with
      | [] -> 
        begin
        match res_adjective2 with
        | [] -> Node (NOMINAL_GROUP (wdet ^ " " ^ wnoun, tags), [det; noun]) :: acc1
        | _ -> List.fold_left (
                 fun acc2 adj ->
                   let wadj = Syntax_tree.st_get_word adj in
                   Node (NOMINAL_GROUP (wdet ^ " " ^ wadj ^ " " ^ wnoun, tags), [det; adj; noun]) :: acc2
               ) [] res_adjective2
        end
      | _ -> begin
             List.fold_left (
               fun acc2 adj1 ->
                 let wadj1 = Syntax_tree.st_get_word adj1 in
                 match res_adjective2 with
                 | [] -> Node (NOMINAL_GROUP (wdet ^ " " ^ wadj1 ^ " " ^ wnoun, tags), [det; adj1; noun]) :: acc2
                 | _ -> List.fold_left (
                          fun acc3 adj2 ->
                            let wadj2 = Syntax_tree.st_get_word adj2 in
                            let word = wdet ^ " " ^ wadj1 ^ " " ^ wadj2 ^ " " ^ wnoun in
                            Node (NOMINAL_GROUP (word, tags), [det; adj1; noun; adj2]) :: acc3
                        ) acc2 res_adjective2
             ) [] res_adjective1
             end
  ) [] res_determiner

let correct_adjectives adjective1 adjective2 =
  let checkings_fun token args =
    match Checkings.check_adjectives (Node (token, [Leaf token])) args with
    | [""; ""] | [_; ""] | [""; _] -> []
    | [x; y] -> [x; y]
    | _ -> failwith "corrections.ml/correct_adjectives : check_adjectives returned unexpected result"
  in
  let construct_tree_fun tags token args =
    Node (ADJECTIVE (Syntax_tree.st_get_word args ^ " " ^ Token.get_word token, tags), [args; Node (token, [Leaf token])])
  in
  correction checkings_fun construct_tree_fun adjective2 adjective1

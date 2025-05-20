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

let get_possibilitys word_class word root_word =
  List.sort (compare word)
  (List.filter_map (fun x -> if Tags.get_word_class x = word_class then
                               Some (Token.create word_class (Tags.get_word x) (Tags.replace_word x root_word))
                             else 
                               None
                   )
  (Dictionary.find rad_dict root_word))

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
          let word2 = beg_word ^ String.make 1 c ^ x ^ end_word in
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
  List.fold_left (
    fun acc x ->
      let word_to_try = x ^ word in
      let tags_list = Dictionary.find dict word_to_try in
      match tags_list with
      | [] -> acc
      | _ -> Token.make_tokens_from_tags_list word_to_try acc tags_list
  ) res char_for_replace

let correction checkings_fun construct_tree_fun to_correct =
  let to_correct_token =
    match to_correct with
    | Node (token, _) -> token
    | _ -> failwith "corrections.ml/correction : to_correct is not a node of a tree"
  in
  let res = checkings_fun to_correct_token in
  match res with
  | [] ->
    begin
    let possibilitys = get_possibilitys (Token.get_word_class to_correct_token)
                                        (Token.get_word to_correct_token)
                                        (Tags.get_root (Token.get_tags to_correct_token))
    in
    let corrections = List.filter_map
                      (fun x -> let tags_res = checkings_fun x in
                                if tags_res <> [] then
                                  Some (construct_tree_fun tags_res x)
                                else None
                      )
                      possibilitys
    in
    if corrections = [] then (Token.print_token to_correct_token; [Error "Unable to correct"]) else corrections
    end
  | _ -> [construct_tree_fun res to_correct_token]

let correct_subject_simple_verb subject verb =
  let checkings_fun token =
    Checkings.check_subject_simple_verb subject (Node (token, [Leaf token]))
  in
  let construct_tree_fun tags token =
    Node (token, [Leaf token])
  in
  correction checkings_fun construct_tree_fun verb

let correct_past_participle subject pronoun auxiliary past_participle =
  let checkings_fun token =
    Checkings.check_past_participle subject pronoun auxiliary (Node (token, [Leaf token]))
  in
  let construct_tree_fun tags token =
    Node (token, [Leaf token])
  in
  correction checkings_fun construct_tree_fun past_participle

let correct_subject_verb subject verbal_group =
  match verbal_group with
  | Node (VERBAL_GROUP (_, _), [pronoun; verb; complement]) ->
    begin
    match verb with
    | Node (AUX_VERB (_, _), [auxiliary; past_participle]) ->
      let result_correct_auxiliary = correct_subject_simple_verb subject auxiliary in
      let result_correct_participle = correct_past_participle subject pronoun auxiliary past_participle in
      let construct_tree_fun auxiliary past_participle =
        Node (AUX_VERB (Syntax_tree.st_get_word auxiliary ^ " " ^ Syntax_tree.st_get_word past_participle,
                        Checkings.check_auxiliary_past_participle auxiliary past_participle),
              [auxiliary; past_participle])
      in
      Utility.join_2 construct_tree_fun result_correct_auxiliary result_correct_participle
    | Node (VERB (_, _), _) ->
      correct_subject_simple_verb subject verb
    | _ -> failwith "corrections.ml/correct_subject_verb : verb isn't a verb or a composed verb"
    end
  | _ -> failwith "corrections.ml/correct_subject_verb : verbal_group isn't a verbal group"

let correct_pronoun_verb pronoun verb =
  match pronoun with
  | Empty -> [Empty]
  | _ ->
    let checkings_fun token =
      Checkings.check_pronoun_verb (Node (token, [Leaf token])) verb
    in
    let construct_tree_fun tags token =
      Node (token, [Leaf token])
    in
    correction checkings_fun construct_tree_fun pronoun

let get_past_participle verb =
  match verb with
  | Node (VERB (word, tags), _) ->
    begin
    let possibilitys =
      (List.filter_map (fun x -> if Tags.get_word_class x = "V" then
                                   Some (Token.create "V" (Tags.get_word x) (Tags.replace_word x (Tags.get_root tags)))
                                 else 
                                   None
                       )
      (Dictionary.find rad_dict (Tags.get_root tags)))
    in
    match List.filter (fun x -> Tags.is_past_participle (Token.get_tags x) && 
                                Tags.get_gender_past_participle (Token.get_tags x) = "m" &&
                                Tags.get_number_past_participle (Token.get_tags x) = "s") possibilitys with
    | [] -> failwith ( "corrections.ml/get_past_participle : no past participle found for verb " ^ word )
    | [x] -> x
    | _ -> failwith ( "corrections.ml/get_past_participle : more than one past participle found for verb " ^ word )
    end
  | _ -> failwith "corrections.ml/get_past_participle : verb isn't a verb"

let correct_auxiliary_verb auxiliary verb =
  let is_past_participle = Checkings.is_past_participle (Syntax_tree.get_token verb) in
  let verb_token = if not is_past_participle then get_past_participle verb else Syntax_tree.get_token verb in
  let verb_tree = Node (verb_token, [Leaf verb_token]) in
  let is_correct_auxiliary = Checkings.check_auxiliary_past_participle auxiliary verb_tree in
  let auxiliary_token =
    match is_correct_auxiliary, (Tags.get_root (Syntax_tree.st_get_tags auxiliary)) with
    | [], "être" -> Token.create "V" "avoir" ["0";"avoir";"V";"i";"t";"_";"_";"_";"_";"a";"Y"]
    | [], "avoir" -> Token.create "V" "être" ["0";"être";"V";"i";"_";"_";"_";"_";"_";"a";"Y"]
    | _, _ -> Syntax_tree.get_token auxiliary
  in
  let auxiliary_tree = Node (auxiliary_token, [Leaf auxiliary_token]) in
  [ Node (AUX_VERB (Token.get_word auxiliary_token ^ " " ^ Token.get_word verb_token, []),
        [auxiliary_tree; verb_tree]) ]

let correct_verbal_group pronoun verb complement =
  let wverb = Syntax_tree.st_get_word verb in
  let wcomplement = Syntax_tree.st_get_word complement in
  match pronoun, complement with
  | Empty, Empty -> [Node (VERBAL_GROUP (wverb, []), [pronoun; verb; complement])]
  | Empty, _ -> 
    [Node (VERBAL_GROUP (wverb ^ " " ^ Syntax_tree.st_get_word complement, []), [pronoun; verb; complement])]
  | _, Empty ->
    List.map (fun x -> Node (VERBAL_GROUP (Syntax_tree.st_get_word x ^ " " ^ wverb, []), [x; verb; complement]))
             (correct_pronoun_verb pronoun verb)
  | _, _ ->
    List.map (fun x -> Node (VERBAL_GROUP (Syntax_tree.st_get_word x ^ " " ^ wverb ^ " " ^ wcomplement, []),
                             [x; verb; complement])) (correct_pronoun_verb pronoun verb)

let correct_sentence subject verbal_group =
  match subject, verbal_group with
  | Node (SUBJECT (wsubject, _), _), Node (VERBAL_GROUP (wverbal_group, _), [pronoun; verb; complement]) ->
    let correction_verb = correct_subject_verb subject verbal_group in
    let correction_verbal_group = List.fold_left (
      fun acc verb_corr -> correct_verbal_group pronoun verb_corr complement @ acc) [] correction_verb in
    List.map (fun x -> Node (SENTENCE (wsubject ^ " " ^ Syntax_tree.st_get_word x, []),
                             [subject; x])) correction_verbal_group
  | _ -> failwith "corrections.ml/correct_sentence : subject isn't a subject or verbal_group isn't a verbal group"


let correct_determiner determiner noun =
  let checkings_fun token =
    match Checkings.check_determiner_noun (Node (token, [Leaf token])) noun with
    | [""; ""] | [_; ""] | [""; _] -> []
    | [x; y] -> [x; y]
    | _ -> failwith "corrections.ml/correct_determiner : check_determiner_noun returned unexpected result"
  in
  let construct_tree_fun tags token =
    Node (token, [Leaf token])
  in
  correction checkings_fun construct_tree_fun determiner

let correct_adj adjective noun =
  let checkings_fun token =
    match Checkings.check_adjective_noun (Node (token, [Leaf token])) noun with
    | [""; ""] | [_; ""] | [""; _] -> []
    | [x; y] -> [x; y]
    | _ -> failwith "corrections.ml/correct_adj : check_adjective_noun returned unexpected result"
  in
  let construct_tree_fun tags token =
    Node (token, [Leaf token])
  in
  correction checkings_fun construct_tree_fun adjective

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
(*TODO : Change how the merge of tree is done*)

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
                            let word = wdet ^ " " ^ wadj1 ^ " " ^ wnoun ^ " " ^ wadj2  in
                            Node (NOMINAL_GROUP (word, tags), [det; adj1; noun; adj2]) :: acc3
                        ) acc2 res_adjective2
             ) [] res_adjective1
             end
  ) [] res_determiner

let get_infinitive token =
  let possibilitys = get_possibilitys "V" (Token.get_word token) (Tags.get_root (Token.get_tags token)) in
  match List.filter (Checkings.is_infinitive) possibilitys with
  | [] -> failwith ( "corrections.ml/get_infinitive : no infinitive found for verb " ^ Token.get_word token )
  | [x] -> x
  | _ -> failwith ( "corrections.ml/get_infinitive : more than one infinitive found for verb " ^ Token.get_word token )

let correct_cod cod =
  match cod with
  | Empty -> [Empty]
  | Node (COD _, children) ->
    begin
    match children with
    | [Node (NOMINAL_GROUP _, [det; adj1; noun; adj2])] ->
      List.map (fun gn -> Node (COD (Syntax_tree.st_get_word gn, []), [gn])) (correct_nominal_group det adj1 noun adj2)
    | [Node (VERB (word, tags) as token, _)] ->
      begin
      if Checkings.is_infinitive token then
        [Node (COD (word, []), [Node (token, [Leaf token])])]
      else
        let infinitive = get_infinitive token in
        [Node (COD (Token.get_word infinitive, []),
               [Node (infinitive, [Leaf infinitive])])]
      end
    | [Node (PROPER_NOUN _, _)] ->
      [cod]
    | _ -> failwith "corrections.ml/correct_cod : children of cod is not a nominal group or a verb or a proper noun"
    end
  | _ -> failwith "corrections.ml/correct_cod : don't receive a cod"

let correct_coi preposition coi =
  [Node (COI (Syntax_tree.st_get_word preposition ^ " " ^ Syntax_tree.st_get_word coi, []),
         [preposition; coi])]

let correct_adjectives adjective1 adjective2 =
  let checkings_fun token =
    match Checkings.check_adjectives (Node (token, [Leaf token])) adjective1 with
    | [""; ""] | [_; ""] | [""; _] -> []
    | [x; y] -> [x; y]
    | _ -> failwith "corrections.ml/correct_adjectives : check_adjectives returned unexpected result"
  in
  let adjective1_word = Syntax_tree.st_get_word adjective1 in
  let construct_tree_fun tags token =
    Node (ADJECTIVE (adjective1_word ^ " " ^ Token.get_word token, tags), [adjective1; Node (token, [Leaf token])])
  in
  correction checkings_fun construct_tree_fun adjective2

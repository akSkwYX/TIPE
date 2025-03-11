open Syntax_tree

let rad_dict = Dictionnary.rad_dictionnary

let correct_verbal_group subject verb =
  match subject, verb with
  | Node (SUBJECT (subject_word, subject_tags), _), Node (VERB (verb_word, verb_tags), _) ->
    [Node (VERBAL_GROUP (subject_word ^ " " ^ verb_word, subject_tags @ verb_tags), [subject; verb])]
  | _ -> [Error "Incorrect verbal group"]

let correct_nominal_group determiner adjective noun adjective2 =
  match determiner, adjective, noun, adjective2 with
  | Node (DETERMINER (determiner_word, determiner_tags), _), Node (ADJECTIVE (adjective1_word, adjective1_tags), _),
    Node (NOUN (noun_word, noun_tags), _), Node (ADJECTIVE (adjective2_word, adjective2_tags), _) ->
    [Node (NOMINAL_GROUP (determiner_word ^ " " ^ adjective1_word ^ " " ^ noun_word ^ " " ^ adjective2_word,
                          determiner_tags @ adjective1_tags @ noun_tags @ adjective2_tags),
           [determiner; adjective; noun; adjective2])]
  | _ -> [Error "Incorrect nominal group"]

let correct_adjectives adjective1 adjective2 =
  match adjective1, adjective2 with
  | Node (ADJECTIVE (adjective1_word, adjective1_tags), _), Node (ADJECTIVE (adjective2_word, adjective2_tags), _) ->
    [Node (ADJECTIVE (adjective1_word ^ " " ^ adjective2_word, adjective1_tags @ adjective2_tags), [adjective1; adjective2])]
  | _ -> [Error "Incorrect adjectives"]

type syntax_tree =
  | Node of Token.token * syntax_tree list
  | Leaf of Token.token
  | Error of string
  | Empty

let new_syntax_tree = Empty

let is_equal t1 t2 =
  match t1, t2 with
  | Node (token1, _), Node (token2, _) ->
    Token.is_equal token1 token2
  | Leaf token1, Leaf token2 -> Token.is_equal token1 token2
  | Error s1, Error s2 -> s1 = s2
  | Empty, Empty -> true
  | _ -> false

let rec contains_error tree_list =
  match tree_list with
  | [] -> []
  | Error s :: _ -> [Error s]
  | _ :: t -> contains_error t

let print_syntax_tree tree =
  let rec print_syntax_tree_aux tree depth =
    match tree with
    | Node (token, children) ->
      print_string (String.make (depth * 2) ' '); Token.print_token token; print_newline ();
      List.iter (fun child -> print_syntax_tree_aux child (depth + 1)) children
    | Leaf token ->
      print_string (String.make (depth * 2) ' '); Token.print_token token; print_newline ();
    | Error message ->
      print_string (String.make (depth * 2) ' '); print_endline message
    | Empty ->
      print_string (String.make (depth * 2) ' '); print_endline "Empty"
  in
  print_syntax_tree_aux tree 0

let st_get_word syntax_tree =
  match syntax_tree with
  | Node (token, _) -> Token.get_word token
  | Leaf token -> Token.get_word token
  | Error s -> s
  | Empty -> ""

let st_get_tags syntax_tree =
  match syntax_tree with
  | Node (token, _) -> Token.get_tags token
  | Leaf token -> Token.get_tags token
  | Error _ -> failwith "syntax_tree.ml/st_get_tags : trying to get tags from an Error"
  | Empty -> []

let get_token syntax_tree =
  match syntax_tree with
  | Node (token, _) -> token
  | Leaf token -> token
  | Error s -> print_endline s; failwith "syntax_tree.ml/get_token : trying to get token from an Error"
  | Empty -> failwith "syntax_tree.ml/get_token : trying to get token from an Empty"

let st_print_tex file syntax_tree =
  let () = Printf.fprintf file "\\begin{tikzpicture}[scale=0.5]\n" in
  let rec aux syntax_tree =
    match syntax_tree with
    | Empty -> Printf.fprintf file "[.{Empty} ]"
    | Error s -> Printf.fprintf file "[.{%s} ]" s
    | Leaf token -> Printf.fprintf file "[.{%s} ]" (Token.get_word token)
    | Node (token, children) ->
      Printf.fprintf file "[.{%s : %s \\\\ %s}\n" (Token.get_word_class token) (Token.get_word token) (Token.format_tags token);
      List.iter aux children;
      Printf.fprintf file " ]\n"
  in
  match syntax_tree with
  | Empty -> Printf.fprintf file "\\Tree [.{Empty} ]\n\\end{tikzpicture}\n"
  | Error s -> Printf.fprintf file "\\Tree [.{%s} ]\n\\end{tikzpicture}\n" s
  | Leaf token -> Printf.fprintf file "\\Tree [.{%s} ]\n\\end{tikzpicture}\n" (Token.get_word token)
  | Node (token, children) ->
    Printf.fprintf file
      "\\Tree [.{%s : %s \\\\ %s }\n"
      (Token.get_word_class token) (Token.get_word token) (Token.format_tags token);
    List.iter aux children;
    Printf.fprintf file " ]\n\\end{tikzpicture}\n\n"

let print_syntax_tree_list syntax_tree_list =
  let file = open_out "results/syntax_tree.tex" in
  let () = Printf.fprintf file 
"\\documentclass[landscape]{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}

\\usepackage[margin=1in]{geometry}
\\usepackage{tikz-qtree}
\\usetikzlibrary{trees}
\\begin{document}
\\tikzset{font=\\small,
edge from parent fork down,
level distance=1.75cm,
every tree node/.style={align=center}
}

\\centering" in
  List.iter (st_print_tex file) syntax_tree_list;
  let () = Printf.fprintf file "\n\\end{document}\n" in
  close_out file;
  Sys.command "pdflatex -interaction=nonstopmode -output-directory=results results/syntax_tree.tex > results/output.log"

let prepare_result_file =
  let file = open_out "results/syntax_tree.tex" in
  let () = Printf.fprintf file "\\documentclass{article}\n\\usepackage{tikz}\n\\begin{document}\n" in
  close_out file

let end_result_file =
  let file = open_out_gen [Open_append] 0o666 "results/syntax_tree.tex" in
  let () = Printf.fprintf file "\\end{document}\n" in
  close_out file;
  Sys.command "pdflatex -interaction=nonstopmode -output-directory=results results/syntax_tree.tex > results/output.log"

let print_syntax_tree_list_add syntax_tree_list =
  let file = open_out_gen [Open_creat; Open_text; Open_append] 0o666 "results/syntax_tree.tex" in
  List.iter (st_print_tex file) syntax_tree_list;
  close_out file

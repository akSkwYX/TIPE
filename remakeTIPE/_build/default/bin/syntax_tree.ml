type syntax_tree =
  | Node of Token.token * syntax_tree list
  | Leaf of Token.token
  | Error of string
  | Empty

let new_syntax_tree = Empty

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
    | Empty -> ()
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
  | Error _ -> []
  | Empty -> []

let st_print_tex file syntax_tree =
  let () = Printf.fprintf file "\\begin{center}\n\\begin{tikzpicture}\n" in
  let rec aux syntax_tree =
    match syntax_tree with
    | Empty -> Printf.fprintf file "child{node{Empty}}\n"
    | Error s -> Printf.fprintf file "child{node{%s}}\n" s
    | Leaf token -> Printf.fprintf file "child{node{%s}}\n" (Token.get_word token)
    | Node (token, children) ->
      Printf.fprintf file "child{node{%s \\\\ %s}\n" (Token.get_word token) (String.concat ", " (Token.get_tags token));
      List.iter aux children;
      Printf.fprintf file "}\n"
  in
  match syntax_tree with
  | Empty -> Printf.fprintf file "\\node{Empty}\n \\end{tikzpicture}\n\\end{center}\n"
  | Error s -> Printf.fprintf file "\\node{%s}\n \\end{tikzpicture}\n\\end{center}\n" s
  | Leaf token -> Printf.fprintf file "\\node{%s}\n \\end{tikzpicture}\n\\end{center}\n" (Token.get_word token)
  | Node (token, children) ->
    Printf.fprintf file
      "\\node{node{%s \\\\ %s}[sibling distance=4cm, level distance = 3cm, align=center]\n"
      (Token.get_word token) (String.concat ", " (Token.get_tags token));
    List.iter aux children;
    Printf.fprintf file "\\end{tikzpicture}\n\\end{center}\n\n"

let print_syntax_tree_list syntax_tree_list =
  let file = open_out "results/syntax_tree.tex" in
  let () = Printf.fprintf file "\\documentclass{article}\n\\usepackage{tikz}\n\\begin{document}\n" in
  List.iter (st_print_tex file) syntax_tree_list;
  let () = Printf.fprintf file "\\end{document}\n" in
  close_out file;
  Sys.command "pdflatex -output-directory=results results/syntax_tree.tex > results/output.log"

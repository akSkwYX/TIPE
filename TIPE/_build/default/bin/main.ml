(* open Utility
open Trie
open Tokenization
open Word_classe
open Grammar
open Dictionnary *)

let init_tex file_name folder_path =
	Sys.chdir folder_path;
	let file = open_out file_name in
	print_string (Sys.getcwd ());
	Printf.fprintf file "\\documentclass{article}\n\\usepackage{tikz}\n\\begin{document}\n";
	close_out file;
	Sys.chdir "..";
	print_string (Sys.getcwd ())

let end_tex file_path =
	let file = open_out file_path in
	Printf.fprintf file "\\end{document}\n";
	close_out file

let compile_tex file_name folder_path =
	Sys.chdir folder_path;
	(* let _ = Sys.command ("pdflatex " ^ file_name ^ " > output.txt") in () *)
	Sys.chdir ".."

let () = init_tex "result.tex" "results"

let test = In_channel.input_line In_channel.stdin 
					 |> Utility.string_of_option 
					 |> Grammar.string_to_syntax_tree_list
					 |> Grammar.syntax_tree_list_in_tex

(* let () = end_tex "results/result.tex" *)
(* let () = compile_tex "result.tex" "results" *)
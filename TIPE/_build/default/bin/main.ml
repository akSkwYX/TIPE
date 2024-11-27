(** 
  [init_tex file_path] initializes a TeX file at the given [file_path].
  This function sets up the necessary environment and writes the initial
  content to the TeX file to prepare it for further writing.

  @param file_path The path to the TeX file to be initialized.
  @return Unit
*)
let init_tex file_path =
	let file = open_out file_path in
	Printf.fprintf file "\\documentclass{article}\n\\usepackage{tikz}\n\\begin{document}\n";
	close_out file

(** 
  [end_tex file_path] appends the LaTeX end document command to the file specified by [file_path].

  @param file_path The path to the file where the end document command will be appended.
  @return Unit
*)
let end_tex file_path =
	let file = open_out_gen [Open_append] 0 file_path in
	Printf.fprintf file "\\end{document}\n";
	close_out file

(** 
  [compile_tex file_name folder_path] compiles a LaTeX file using pdflatex.
  The output PDF and log files are placed in the specified folder.

  @param file_name The name of the LaTeX file to compile.
  @param folder_path The directory where the output files will be stored.
  @return The exit status of the pdflatex command.
*)
let compile_tex file_name folder_path =
	Sys.command ("pdflatex -output-directory=" ^ folder_path ^ " result.tex > " ^ folder_path ^ "output.txt")

let () = init_tex "results/result.tex"

let test = In_channel.input_line In_channel.stdin 
					 |> Utility.string_of_option
					 |> Grammar.string_to_syntax_tree_list
					 |> Grammar.get_results

let () = end_tex "results/result.tex"
let _ = compile_tex "result.tex" "results/"
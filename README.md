## Haxe Compiler with Preprocessors

### Goal
When the Haxe compiler reads source code into memory, allow for plugins to preprocess the files before they are sent for typing or any other form of compilation.

### Justification
* While Haxe's macro system effectively allows for writing preprocessors in-source, its scope is actually quite limited. Instead of performing surgery on the AST, it would often be better to just preprocess the text of the source code. 
* This could be done with explicit preprocessor scripts which are configured in the .hxml, but often we do not actually want to permenantly write to the source code file.
* The Haxe compiler does already have a plugin system but it appears to be just a watered down version of the macro system, as you must load plugins within the macro context itself, and it can only hook onto late compiler callbacks such as "onAfterTyping".

### Implementation

This doesn't appear to be an invasive change at all. Below is some first-pass POC logic being added at the point where files are read into the compiler, inside typing/typeloadParse.ml.

Currently I'm using Toploop/Toplevel to pull .ml scripts from the ./compiler_plugins folder relative to where the compiler was run, and interperatively execute the scripts. However Toploop/Toplevel comes with their own restrictions and caveats so I'm not sure if they would be used in a final implementation. You could also have this be any kind of normal bash script, but then you would need to add more logic for handling the inter-process communication.


```ocaml

let parse_file com rfile p =
	let file_key = com.file_keys#get rfile.ClassPaths.file in
	let contents = match com.file_contents with
			| [] when (Common.defined com Define.DisplayStdin) && DisplayPosition.display_position#is_in_file file_key ->
					let s = Std.input_all stdin in
					close_in stdin;

					let plugin_directory = "./compiler_plugins" in

					let plugin_files = Sys.readdir plugin_directory
							|> Array.to_list
							|> List.filter (fun f -> Filename.check_suffix f ".ml")
							|> List.map (fun f -> Filename.concat plugin_directory f)
					in

					let initialize_toploop () =
							Toploop.initialize_toplevel_env ()
					in

					let run_plugin_script script_file input =
							let channel = open_in script_file in
							let lexbuf = Lexing.from_channel channel in
							Toploop.use_file Format.err_formatter lexbuf;
							close_in channel;

							let () = Toploop.execute_phrase true Format.err_formatter (Parsetree.Ptopdef [Ast_helper.Str.eval (Ast_helper.Exp.constant (Parsetree.Const_string (input, None)))]) in

							let output_ref = ref "" in
							let () = Toploop.execute_phrase true Format.err_formatter (Parsetree.Ptopdef [
									Ast_helper.Str.eval (
											Ast_helper.Exp.let_ Nonrecursive [Ast_helper.Vb.mk (Ast_helper.Pat.var (Location.mknoloc "output_value")) (Ast_helper.Exp.ident (Location.mknoloc (Longident.Lident "output")))]
											(Ast_helper.Exp.sequence (Ast_helper.Exp.setfield (Location.mknoloc (Lident "output_value")))
											(Ast_helper.Exp.ident (Location.mknoloc (Longident.Lident "output"))))
									)
							]) in
							!output_ref
					in

					let s = List.fold_left (fun acc plugin_file ->
							let output = run_plugin_script plugin_file acc in
							output
					) s plugin_files in
					
					com.file_contents <- [file_key, Some s];
					Some s
			| [] -> None
			| files ->
					(try List.assoc file_key files with Not_found -> None)
	in

	match contents with
	| Some s ->
			parse_file_from_string com rfile.file p s
	| _ ->
			match rfile.class_path#file_kind with
			| FFile ->
					let file = rfile.file in
					let ch = try open_in_bin file with _ -> raise_typing_error ("Could not open " ^ file) p in
					Std.finally (fun() -> close_in ch) (parse_file_from_lexbuf com file p) (Sedlexing.Utf8.from_channel ch)


```

In the final design, this would be pulling the preprocessors from the .hxml instead of a local folder.

While the implementation is appearantly straightforward and noninvasive, I do not know how this would affect the LSP or the compilation cache. Testing those aspects would be the bulk of a production implemenation.
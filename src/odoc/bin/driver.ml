let ( let* ) t f = Result.bind t f

let ( let+ ) t f = Result.map f t

(*
In this section odoc is used to generate the documentation of odoc and some of
its dependent packages. We can make a few simplifying assumptions here:

We start with some functions to execute the three phases of odoc.

Compiling a file with odoc requires a few arguments: the file to compile, an
optional parent, a list of include paths, a list of children for mld files, and
an output path. Include paths can be just '.', and we can calculate the output
file from the input because all of the files are going into the same directory.

Linking a file with odoc requires the input file and a list of include paths. As
for compile we will hard-code the include path.

Generating the HTML requires the input odocl file and an output path. We will
hard-code the output path to be html.

In all of these we'll ignore stderr.
*)
(* let odoc = Cmd.v "../../_esy/default/store/i/odoc-57278b99/bin/odoc" *)
let odoc = Bos.Cmd.v "odoc"

let ( % ) = Bos.Cmd.( % )

exception CompileError

let compile file ?parent children =
  print_endline ("Compiling " ^ Fpath.to_string file);
  let output_file =
    let ext = Fpath.get_ext file in
    let basename = Fpath.basename (Fpath.rem_ext file) in
    match ext with
    | ".mld" -> "page-" ^ basename ^ ".odoc"
    | ".cmt" | ".cmti" | ".cmi" -> basename ^ ".odoc"
    | _ -> failwith ("bad extension: " ^ ext)
  in
  let cmd =
    odoc % "compile" % Fpath.to_string file % "-I" % "." % "-o" % output_file
  in
  let cmd =
    List.fold_left (fun cmd child -> cmd % "--child" % child) cmd children
  in
  let cmd = match parent with Some p -> cmd % "--parent" % p | None -> cmd in
  let result = Bos.OS.Cmd.(run_out ~err:err_null cmd |> to_lines) in
  match result with
  | Ok _ ->
      print_endline ("Successfully compiled " ^ Fpath.to_string file);
      output_file
  | Error error ->
      (match error with
      | `Msg message ->
          print_endline message;
          print_endline ("Error compiling " ^ Fpath.to_string file));
      raise CompileError

let link file =
  let cmd = odoc % "link" % Bos.Cmd.p file % "-I" % "." in
  match Bos.OS.Cmd.(run_out ~err:err_null cmd |> to_lines) with
  | Ok result -> result
  | Error _ -> failwith "Error in link"

let html_generate file =
  let cmd = odoc % "html-generate" % Bos.Cmd.p file % "-o" % "html" in
  match Bos.OS.Cmd.(run_out cmd ~err:err_null |> to_lines) with
  | Ok result -> result
  | Error _ -> failwith "Error in html_generate"

let support_files () =
  let cmd = odoc % "support-files" % "-o" % "html" in
  match Bos.OS.Cmd.(run_out cmd |> to_lines) with
  | Ok result -> result
  | Error _ -> failwith "Error in support_files"

(*

We'll now make some library lists. We have not only external dependency
libraries, but Odoc itself is separated into libraries too. These two sets of
libraries will be documented in different sections, so we'll keep them in
separate lists, together with a We start by declaring a few lists representing
these sections as well as a list mapping the section to its parent matching to
the hierarchy declared above.

*)

let dep_libraries =
  [ "cmdliner"; "astring"; "fpath"; "result"; "yojson"; "tyxml"; "stdlib" ]

let odoc_libraries =
  [
    "odoc_xref_test";
    "print";
    "odoc_xref2";
    "odoc_parser";
    "odoc_odoc";
    "odoc_model_desc";
    "odoc_model";
    "odoc_manpage";
    "odoc_loader";
    "odoc_latex";
    "odoc_html";
    "odoc_document";
    "odoc_compat";
  ]

let all_libraries = dep_libraries @ odoc_libraries

let extra_docs = [ "interface"; "driver" ]

let parents =
  let add_parent p l = List.map (fun lib -> (lib, p)) l in
  add_parent "deps" dep_libraries @ add_parent "odoc" odoc_libraries

let with_error s = function Ok result -> result | Error _ -> failwith s

(*
Odoc operates on the compiler outputs. We need to find them for both the files
compiled by dune within this project and those in libraries we compile against.
The following uses ocamlfind to locate the library paths we're looking in for
our dependencies:
*)
let ocamlfind = Bos.Cmd.v "ocamlfind"

let lib_path lib =
  let cmd = Bos.Cmd.(ocamlfind % "query" % lib) in
  Bos.OS.Cmd.(run_out cmd |> to_lines |> Result.map List.hd)

let lib_paths =
  List.fold_left
    (fun acc lib ->
      let* acc = acc in
      let+ l = lib_path lib in
      (lib, l) :: acc)
    (Ok []) dep_libraries
  |> with_error "Error in lib_paths"

(*
We need a function to find odoc inputs given a search path. The files that odoc
operates on are cmti, cmt or cmi files, in order of preference, and the
following function finds all files like that given a search path, returning an
Fpath.Set.t containing Fpath.t values representing the absolute path to the file
without its extension.
*)

let find_units p =
  let+ paths =
    Bos.OS.Dir.fold_contents ~dotfiles:true
      (fun p acc ->
        if List.exists (fun ext -> Fpath.has_ext ext p) [ "cmt"; "cmti"; "cmi" ]
        then p :: acc
        else acc)
      [] (Fpath.v p)
  in
  let l = List.map Fpath.rem_ext paths in
  List.fold_left (fun acc path -> Fpath.Set.add path acc) Fpath.Set.empty l

(*
Since the units returned by this function have their extension stripped, we need
function to find the best file to use given this basename.
*)

let best_file base =
  List.map (fun ext -> Fpath.add_ext ext base) [ "cmti"; "cmt"; "cmi" ]
  |> List.find (fun f ->
         Bos.OS.File.exists f |> with_error "Error in best_file")

(*
Many of the units will be 'hidden' \-- that is, their name will be mangled by
dune in order to namespace them. This is achieved by prefixing the namespace
module and a double underscore, so we can tell by the existence of a double
underscore that a module is intended to be hidden. The following predicate tests
for that condition:
*)

let is_hidden path = Astring.String.is_infix ~affix:"__" (Fpath.to_string path)

(*
To build the documentation, we start with these files. We'll call
odoc compile-deps on the file to find out which other compilation units it
depends upon, with the following function:
*)

type compile_deps = { digest : Digest.t; deps : (string * Digest.t) list }

let compile_deps f =
  let cmd = Bos.Cmd.(odoc % "compile-deps" % Fpath.to_string f) in
  print_endline (Bos.Cmd.to_string cmd);

  let* lines = Bos.OS.Cmd.(run_out cmd |> to_lines) in
  let l = List.filter_map (Astring.String.cut ~sep:" ") lines in
  let basename = Fpath.(basename (f |> rem_ext)) |> String.capitalize_ascii in
  match List.partition (fun (n, _) -> basename = n) l with
  | [ (_, digest) ], deps -> Ok { digest; deps }
  | _ -> Error (`Msg "odd")

(*
Let's now put together a list of all possible modules. We'll keep track of which
library they're in, and whether that library is a part of odoc or a dependency
library.
*)

let odoc_all_unit_paths =
  find_units ".." |> with_error "Error in odoc_all_unit_paths"

let odoc_units =
  List.map
    (fun lib ->
      Fpath.Set.fold
        (fun p acc ->
          if Astring.String.is_infix ~affix:lib (Fpath.to_string p) then
            ("odoc", lib, p) :: acc
          else acc)
        odoc_all_unit_paths [])
    odoc_libraries

let lib_units =
  List.map
    (fun (lib, p) ->
      Fpath.Set.fold
        (fun p acc -> ("deps", lib, p) :: acc)
        (find_units p |> with_error "Error in lib_units")
        [])
    lib_paths

let all_units = odoc_units @ lib_units |> List.flatten

(*
Let's compile all of the parent mld files. We do this in order such that the
parents are compiled before the children, so we start with odoc.mld, then
deps.mld, and so on. The result of this file is a list of the resulting odoc
files.
*)

let mkpage x = "page-" ^ x

let mkmod x = "module-" ^ x

let mkmld x = Fpath.(add_ext "mld" (v x))

let root_compile children = compile (mkmld "odoc") (List.map mkpage children)

let deps_compile children =
  compile (mkmld "deps") ~parent:"odoc" (List.map mkpage children)

let independent_docs_compile docs =
  List.map (fun p -> compile (mkmld p) ~parent:"odoc" []) docs

let compile_mlds () =
  print_endline "Compiling mlds";
  let root = root_compile ("deps" :: odoc_libraries @ extra_docs) in
  let deps = deps_compile dep_libraries in
  let extra_odocs = independent_docs_compile extra_docs in
  let odocs =
    List.map
      (fun library ->
        let parent = List.assoc library parents in
        let children =
          List.filter_map
            (fun (parent, lib, child) ->
              if lib = library then Some (Fpath.basename child |> mkmod)
              else None)
            all_units
        in
        compile (mkmld library) ~parent children)
      all_libraries
  in
  List.map Fpath.v (root :: deps :: odocs @ extra_odocs)

(*

Now we get to the compilation phase. For each unit, we query its dependencies,
then recursively call to compile these dependencies. Once this is done we
compile the unit itself. If the unit has already been compiled we don't do
anything. Note that we aren't checking the hashes of the dependencies which a
build system should do to ensure that the module being compiled is the correct
one. Again we benefit here from the fact that we're creating the docs for one
leaf package, and that there must be no module name clashes in its dependencies.
The result of this function is a list of the resulting odoc files.

*)

let compile_all () =
  print_endline "Compile all...";
  let mld_odocs = compile_mlds () in
  let rec rec_compile lib file =
    let output = Fpath.(base (set_ext "odoc" file)) in
    match Bos.OS.File.exists output with
    | Ok true | Error _ -> []
    | Ok false ->
        let deps =
          match compile_deps file with
          | Ok result -> result
          | Error _ -> failwith "Error in compile_deps invocation"
        in
        let files =
          List.fold_left
            (fun acc (dep_name, digest) ->
              match
                List.find_opt
                  (fun (_, _, f) ->
                    Fpath.basename f |> String.capitalize_ascii = dep_name)
                  all_units
              with
              | None -> acc
              | Some (_, lib, dep_path) ->
                  let file = best_file dep_path in
                  rec_compile lib file @ acc)
            [] deps.deps
        in
        ignore (compile file ~parent:lib []);
        output :: files
  in
  List.fold_left
    (fun acc (parent, lib, dep) -> acc @ rec_compile lib (best_file dep))
    [] all_units
  (* (List.flatten odoc_deps) *)
  @ mld_odocs

(*

Linking is now straightforward. We only need to link non-hidden odoc files, as
any hidden are almost certainly aliased inside the non-hidden ones \(a result
of namespacing usually, and these aliases will be expanded.

*)

let link_all odoc_files =
  let not_hidden f = not (is_hidden f) in
  List.map
    (fun odoc_file ->
      ignore (link odoc_file);
      Fpath.set_ext "odocl" odoc_file)
    (List.filter not_hidden odoc_files)

(*

Now we can simply run odoc html-generate over all of the resulting odocl
files

*)

let generate_all odocl_files =
  List.iter (fun f -> ignore (html_generate f)) odocl_files;
  support_files ()

(*

The following code actually executes all of the above, and we're done!

*)

let compiled = compile_all ()

let linked = link_all compiled

let _ = generate_all linked

let revolt () = print_endline "Revolt!"

let revolt_t = Cmdliner.Term.(const revolt $ const ())

let () =
  Cmdliner.Term.exit
  @@ Cmdliner.Term.eval (revolt_t, Cmdliner.Term.info "revolt")

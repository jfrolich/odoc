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
let _ = print_endline "Building docs..."

(*
   this is the configuration, needs to be picked up from the working directory
   in a JSON file

   documel.json
*)
let dep_libraries =
  [ "cmdliner"; "astring"; "fpath"; "result"; "yojson"; "tyxml" (* "stdlib" *) ]

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

let root_page = "odoc"

let with_error s = function Ok result -> result | Error _ -> raise s

exception CompileError

exception LinkError

exception HtmlGenerationError

exception JSONGenerationError

exception SupportFilesError

exception LibPathsError

exception BestFileError

exception AllUnitPathsError

exception LibUnitsError

exception CompileDeps

exception BadExtension of string

let odoc = Bos.Cmd.v "odoc"

let ( % ) = Bos.Cmd.( % )

let extra_docs = [ "interface"; "driver" ]

let mkpage x = "page-" ^ x

let mkmod x = "module-" ^ x

let compile file ?parent children =
  (* print_endline ("Compiling " ^ Fpath.to_string file); *)
  let output_file =
    let ext = Fpath.get_ext file in
    let basename = Fpath.basename (Fpath.rem_ext file) in
    match ext with
    | ".mld" -> mkpage basename ^ ".odoc"
    | ".cmt" | ".cmti" | ".cmi" -> basename ^ ".odoc"
    | _ -> raise (BadExtension ext)
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
      (* print_endline ("Successfully compiled " ^ Fpath.to_string file); *)
      output_file
  | Error error ->
      (match error with
      | `Msg message ->
          print_endline message;
          print_endline ("Error compiling " ^ Fpath.to_string file));
      raise CompileError

let link file =
  let cmd = odoc % "link" % Bos.Cmd.p file % "-I" % "." in
  Bos.OS.Cmd.(run_out ~err:err_null cmd |> to_lines) |> with_error LinkError

let html_generate file =
  let cmd = odoc % "html-generate" % Bos.Cmd.p file % "-o" % "html" in
  Bos.OS.Cmd.(run_out cmd ~err:err_null |> to_lines)
  |> with_error HtmlGenerationError

let json_generate file =
  let cmd = odoc % "json-generate" % Bos.Cmd.p file % "-o" % "html" in
  Bos.OS.Cmd.(run_out cmd ~err:err_null |> to_lines)
  |> with_error JSONGenerationError

let support_files () =
  let cmd = odoc % "support-files" % "-o" % "html" in
  Bos.OS.Cmd.(run_out cmd |> to_lines) |> with_error SupportFilesError

(*
We'll now make some library lists. We have not only external dependency
libraries, but Odoc itself is separated into libraries too. These two sets of
libraries will be documented in different sections, so we'll keep them in
separate lists, together with a We start by declaring a few lists representing
these sections as well as a list mapping the section to its parent matching to
the hierarchy declared above.
*)

(*
Odoc operates on the compiler outputs. We need to find them for both the files
compiled by dune within this project and those in libraries we compile against.
The following uses ocamlfind to locate the library paths we're looking in for
our dependencies:
*)

let ocamlfind = Bos.Cmd.v "ocamlfind"

(* resolving the path for a library *)
let resolve_lib_path lib =
  let cmd = Bos.Cmd.(ocamlfind % "query" % lib) in
  Bos.OS.Cmd.(run_out cmd |> to_lines |> Result.map List.hd)

(** constructing (library_name, library_path) tuples *)
let resolve_lib_paths library_names =
  List.fold_left
    (fun acc lib ->
      let* acc = acc in
      let+ l = resolve_lib_path lib in
      (lib, l) :: acc)
    (Ok []) library_names
  |> with_error LibPathsError

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

let find_best_file base =
  List.map (fun ext -> Fpath.add_ext ext base) [ "cmti"; "cmt"; "cmi" ]
  |> List.find (fun f -> Bos.OS.File.exists f |> with_error BestFileError)

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
  (* print_endline ("DEPS of " ^ Fpath.to_string f); *)
  let cmd = Bos.Cmd.(odoc % "compile-deps" % Fpath.to_string f) in

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

type path = Fpath.t

let odoc_all_unit_paths =
  find_units "../_build/" |> with_error AllUnitPathsError

type unit = Unit_local of Fpath.t | Unit_namespace of string * Fpath.t

let units : (string, unit) Hashtbl.t = Hashtbl.create 10000

let _ =
  Fpath.Set.iter
    (fun p ->
      if not (is_hidden p) then
        Hashtbl.add units
          (Fpath.basename p |> String.capitalize_ascii)
          (Unit_local p))
    odoc_all_unit_paths

(* let _ =
  List.iter (fun (_, path) -> print_endline (Fpath.to_string path)) odoc_units *)

let _ =
  List.iter
    (fun (lib, p) ->
      Fpath.Set.iter
        (fun p ->
          if not (is_hidden p) then
            Hashtbl.add units
              (Fpath.basename p |> String.capitalize_ascii)
              (Unit_namespace ("deps", p)))
        (find_units p |> with_error LibUnitsError))
    (resolve_lib_paths dep_libraries)

let _ =
  Seq.iter
    (fun u ->
      print_endline
        (match u with
        | Unit_local f | Unit_namespace (_, f) -> Fpath.to_string f))
    (Hashtbl.to_seq_values units)
(*
Let's compile all of the parent mld files. We do this in order such that the
parents are compiled before the children, so we start with odoc.mld, then
deps.mld, and so on. The result of this file is a list of the resulting odoc
files.
*)

let mkmld x = Fpath.(add_ext "mld" (v x))

let compile_mlds () =
  (* print_endline "Compiling mlds"; *)
  let root_children =
    List.map (fun x -> mkpage x) ("deps" :: extra_docs)
    @ (units |> Hashtbl.to_seq_values
      |> Seq.filter_map (function
           | Unit_local p -> Some p
           | Unit_namespace _ -> None)
      |> Seq.map (fun child -> Fpath.basename child)
      |> List.of_seq)
  in
  List.iter print_endline root_children;
  let root = compile (mkmld ("" ^ root_page)) root_children in

  let deps_children =
    units |> Hashtbl.to_seq_values
    |> Seq.filter_map (function
         | Unit_local _ -> None
         | Unit_namespace (_, f) -> Some f)
    |> Seq.map (fun child -> Fpath.basename child |> mkmod)
    |> List.of_seq
  in
  let deps = compile (mkmld ("" ^ "deps")) ~parent:root_page deps_children in

  let extra_odocs =
    List.map (fun p -> compile (mkmld ("" ^ p)) ~parent:root_page []) extra_docs
  in

  (* print_endline "Done with MLDs"; *)
  List.map Fpath.v (root :: deps :: extra_odocs)

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

module StringSet = Set.Make (String)

let already_compiled = Hashtbl.create 10000

let compile_all () =
  (* print_endline "Compile all..."; *)
  let mld_odocs = compile_mlds () in
  let rec rec_compile lib file =
    let output = Fpath.(base (set_ext "odoc" file)) in
    (* we need to do this to avoid circular dependencies *)
    match Hashtbl.find_opt already_compiled (Fpath.to_string output) with
    | Some true -> []
    | Some false | None ->
        Hashtbl.add already_compiled (Fpath.to_string output) true;
        let deps = compile_deps file |> with_error CompileDeps in
        let files =
          List.fold_left
            (fun acc (dep_name, digest) ->
              match Hashtbl.find_opt units dep_name with
              | None -> acc
              | Some (Unit_local dep_path) ->
                  let file = find_best_file dep_path in
                  rec_compile root_page file @ acc
              | Some (Unit_namespace (lib, dep_path)) ->
                  let file = find_best_file dep_path in
                  rec_compile lib file @ acc)
            [] deps.deps
        in
        ignore (compile file ~parent:lib []);
        output :: files
  in
  List.fold_left
    (fun acc u ->
      acc
      @
      match u with
      | Unit_local dep -> rec_compile "odoc" (find_best_file dep)
      | Unit_namespace (lib, dep) -> rec_compile lib (find_best_file dep))
    []
    (units |> Hashtbl.to_seq_values |> List.of_seq)
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
  List.iter
    (fun f ->
      let _ = html_generate f in
      let _ = json_generate f in
      ())
    odocl_files;
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

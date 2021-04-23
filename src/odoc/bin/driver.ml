(*
  TODO:
    - [ ] store last modified date of unit, and compare to odoc file to see if we need to recompile
    - [ ] generate odoc and odocl alongside the cmt/cmti files
    - [x] don't shell out to the executables, but call directly
    - [x] Remove dependency of Bos


*)
let ( let* ) t f = Result.bind t f

let ( let+ ) t f = Result.map f t

module StringSet = Set.Make (String)

let _ = print_endline "Building docs..."

type config = {
  (* where to find the units *)
  unit_root : string;
  (* the dependencies that documl needs to generate docs for *)
  dep_libraries : string list;
  root_page : string;
}

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

exception GetDepsError

open Odoc_odoc

type path = Fpath.t

type unit = Unit_local of Fpath.t | Unit_namespace of string * Fpath.t

let extra_docs = [ "interface"; "driver" ]

let mkpage x = "page-" ^ x

let mkmod x = "module-" ^ x

let compile file ?parent ~env ~directories children =
  (* print_endline ("Compiling " ^ Fpath.to_string file); *)
  let output_file =
    let ext = Fpath.get_ext file in
    let basename = Fpath.basename (Fpath.rem_ext file) in
    match ext with
    | ".mld" -> mkpage basename ^ ".odoc"
    | ".cmt" | ".cmti" | ".cmi" -> basename ^ ".odoc"
    | _ -> raise (BadExtension ext)
  in
  let _ = Fs.Directory.mkdir_p (Fs.File.dirname file) in
  match
    Compile.compile ~env ~directories
      ~parent_cli_spec:
        (match parent with
        | Some parent -> CliParent parent
        | None -> CliNoparent)
      ~hidden:false ~children
      ~output:(output_file |> Fs.File.of_string)
      ~warn_error:false file
  with
  | Ok _ ->
      (* print_endline ("Successfully compiled " ^ Fpath.to_string file); *)
      output_file
  | Error error ->
      (match error with
      | `Msg message ->
          print_endline message;
          print_endline ("Error compiling " ^ Fpath.to_string file));
      raise CompileError

let link ~env file =
  Odoc_link.from_odoc ~env ~warn_error:false file
    Fs.File.(set_ext ".odocl" file)
  |> with_error LinkError

let html_generate ~env file =
  Rendering.render_odoc ~env ~renderer:Html_page.renderer ~warn_error:false
    ~syntax:Odoc_document.Renderer.Reason
    ~output:(Fs.Directory.of_string "html")
    {
      semantic_uris = true;
      closed_details = false;
      indent = true;
      theme_uri = Odoc_html.Tree.Relative None;
      support_uri = Odoc_html.Tree.Relative None;
    }
    file
  |> with_error HtmlGenerationError

let json_generate ~env file =
  let directories = [ "." |> Fs.Directory.of_string ] in
  let env = Env.create ~important_digests:false ~directories ~open_modules:[] in
  Rendering.render_odoc ~env ~renderer:Odoc_json.Json_renderer.renderer
    ~warn_error:false ~syntax:Odoc_document.Renderer.Reason
    ~output:(Fs.Directory.of_string "html")
    { semantic_uris = true; closed_details = false; indent = true }
    file
  |> with_error JSONGenerationError

let support_files () =
  Support_files.write ~without_theme:false (Fs.Directory.of_string "html")

let resolve_lib_path lib =
  let ic = Unix.open_process_in ("ocamlfind query " ^ lib) in
  try input_line ic with End_of_file -> raise GetDepsError

(** constructing (library_name, library_path) tuples *)
let resolve_lib_paths library_names =
  List.fold_left
    (fun acc lib ->
      let acc = acc in
      let l = resolve_lib_path lib in
      (lib, l) :: acc)
    [] library_names

(*
We need a function to find odoc inputs given a search path. The files that odoc
operates on are cmti, cmt or cmi files, in order of preference, and the
following function finds all files like that given a search path, returning an
Fpath.Set.t containing Fpath.t values representing the absolute path to the file
without its extension.
*)
let traverse_files dir =
  let rec loop result = function
    | f :: fs when Sys.is_directory (Fpath.to_string f) ->
        print_endline ("A: " ^ (f |> Fpath.to_string));
        Sys.readdir (Fpath.to_string f)
        |> Array.to_list
        |> List.map (Fpath.add_seg f)
        |> List.append fs |> loop result
    | f :: fs
      when List.exists (fun ext -> Fpath.has_ext ext f) [ "cmt"; "cmti"; "cmi" ]
      ->
        print_endline ("B: " ^ (f |> Fpath.to_string));
        loop (f :: result) fs
    | f :: fs ->
        print_endline ("C: " ^ (f |> Fpath.to_string));
        loop result fs
    | [] ->
        print_endline "NOTHING: ";
        result
  in
  print_endline ("DIR: " ^ (dir |> Fpath.to_string));
  loop [] [ dir ]

(* let rec traverse_files path =
  let files = Sys.readdir (path) in
  let files = Array.to_list files in
  List.fold_left (fun acc file -> if Sys.is_directory file then (List.append traverse_files file) else file :: files) *)

let add_units units ~path ~namespace =
  traverse_files (Fpath.v path)
  |> List.iter (fun path ->
         let path = Fpath.rem_ext path in
         Hashtbl.add units
           (Fpath.basename path |> String.capitalize_ascii)
           (match namespace with
           | Some namespace -> Unit_namespace (namespace, path)
           | None -> Unit_local path))

(*
Since the units returned by this function have their extension stripped, we need
function to find the best file to use given this basename.
*)

let find_best_file base =
  List.map (fun ext -> Fpath.add_ext ext base) [ "cmti"; "cmt"; "cmi" ]
  |> List.find (fun f -> Sys.file_exists (Fpath.to_string f))

(*
Many of the units will be 'hidden' \-- that is, their name will be mangled by
dune in order to namespace them. This is achieved by prefixing the namespace
module and a double underscore, so we can tell by the existence of a double
underscore that a module is intended to be hidden. The following predicate tests
for that condition:
*)

let is_hidden path = Astring.String.is_infix ~affix:"__" (Fpath.to_string path)

type compile_deps = { digest : Digest.t; deps : (string * Digest.t) list }

let compile_deps f =
  let l =
    Depends.for_compile_step f
    |> List.map (fun t ->
           (Depends.Compile.name t, Digest.to_hex @@ Depends.Compile.digest t))
  in
  let basename = Fpath.(basename (f |> rem_ext)) |> String.capitalize_ascii in
  match List.partition (fun (n, _) -> basename = n) l with
  | [ (_, digest) ], deps -> Ok { digest; deps }
  | _ -> Error (`Msg "odd")

let add_local_units ~config units =
  add_units units ~path:config.unit_root ~namespace:None

(* let _ =
   List.iter (fun (_, path) -> print_endline (Fpath.to_string path)) odoc_units *)

let add_dep_units ~config units =
  List.iter
    (fun (_, path) -> add_units units ~path ~namespace:(Some "deps"))
    (resolve_lib_paths config.dep_libraries)

(* Seq.iter (fun u ->
     print_endline
       (match u with
       | Unit_local f | Unit_namespace (_, f) -> Fpath.to_string f))
   (Hashtbl.to_seq_values units) *)

(*
Let's compile all of the parent mld files. We do this in order such that the
parents are compiled before the children, so we start with odoc.mld, then
deps.mld, and so on. The result of this file is a list of the resulting odoc
files.
*)

(* this compiles the mld and markdown files *)
let compile_docs ~config ~units ~env ~directories =
  let mkmld x = Fpath.(add_ext "mld" (v x)) in
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
  let root =
    compile ~env ~directories (mkmld ("doc/" ^ config.root_page)) root_children
  in

  let deps_children =
    units |> Hashtbl.to_seq_values
    |> Seq.filter_map (function
         | Unit_local _ -> None
         | Unit_namespace (_, f) -> Some f)
    |> Seq.map (fun child -> Fpath.basename child |> mkmod)
    |> List.of_seq
  in
  let deps =
    compile ~env ~directories
      (mkmld ("doc/" ^ "deps"))
      ~parent:config.root_page deps_children
  in

  let extra_odocs =
    List.map
      (fun p ->
        compile ~env ~directories
          (mkmld ("doc/" ^ p))
          ~parent:config.root_page [])
      extra_docs
  in

  (* print_endline "Done with MLDs"; *)
  List.map Fpath.v (root :: deps :: extra_odocs)

let compile_all ~config ~env ~directories =
  let units : (string, unit) Hashtbl.t = Hashtbl.create 10000 in
  let already_compiled = Hashtbl.create 10000 in
  let _ = add_local_units ~config units in
  let _ = add_dep_units ~config units in
  (* print_endline "Compile all..."; *)
  let mld_odocs = compile_docs ~env ~directories ~config ~units in
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
                  rec_compile config.root_page file @ acc
              | Some (Unit_namespace (lib, dep_path)) ->
                  let file = find_best_file dep_path in
                  rec_compile lib file @ acc)
            [] deps.deps
        in
        ignore (compile ~env ~directories file ~parent:lib []);
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

let link_all ~env odoc_files =
  let not_hidden f = not (is_hidden f) in
  List.map
    (fun odoc_file ->
      ignore (link ~env odoc_file);
      Fpath.set_ext "odocl" odoc_file)
    (List.filter not_hidden odoc_files)

let generate_all ~env odocl_files =
  List.iter
    (fun f ->
      let _ = html_generate ~env f in
      let _ = json_generate ~env f in
      ())
    odocl_files;
  support_files ()

(*
   this is the configuration, needs to be picked up from the working directory
   in a JSON file

   documel.json
*)
let default_config =
  {
    unit_root = "_build/";
    root_page = "odoc";
    dep_libraries =
      [
        "cmdliner";
        "astring";
        "fpath";
        "result";
        "yojson";
        "tyxml" (* "stdlib" *);
      ];
  }

(* this should be unit_root, but we need to localize the docs first instead of hardcoding the location *)
let directories = [ "." |> Fs.Directory.of_string ]

let env = Env.create ~important_digests:false ~directories ~open_modules:[]

let compiled = compile_all ~env ~directories ~config:default_config

let linked = link_all ~env compiled

let _ = generate_all ~env linked

let revolt () = print_endline "Revolt!"

let revolt_t = Cmdliner.Term.(const revolt $ const ())

let () =
  Cmdliner.Term.exit
  @@ Cmdliner.Term.eval (revolt_t, Cmdliner.Term.info "revolt")

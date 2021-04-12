open Odoc_document
open Odoc_document.Types

type args = { semantic_uris : bool; closed_details : bool; indent : bool }

let rec list_concat_map ?sep ~f = function
  | [] -> []
  | [ x ] -> f x
  | x :: xs -> (
      let hd = f x in
      let tl = list_concat_map ?sep ~f xs in
      match sep with None -> hd @ tl | Some sep -> hd @ sep :: tl)

module Page = struct
  let rec include_ ?theme_uri indent { Subpage.content; _ } =
    [ page ?theme_uri indent content ]

  and subpages ?theme_uri indent i =
    list_concat_map ~f:(include_ ?theme_uri indent)
    @@ Doctree.Subpages.compute i

  and page ?theme_uri ?support_uri indent p =
    let filename =
      Fpath.set_ext "json" (Odoc_html.Link.Path.as_filename p.Page.url)
    in
    let content formatter =
      Yojson.Basic.pretty_print formatter (Types.Page.serialize p)
    in
    let children = subpages ?theme_uri indent p in
    { Renderer.filename; content; children }
end

let render args page =
  Odoc_html.Link.semantic_uris := args.semantic_uris;
  Odoc_html.Tree.open_details := not args.closed_details;

  Page.page args.indent page

let files_of_url url =
  [ Fpath.set_ext "json" (Odoc_html.Link.Path.as_filename url) ]

let renderer = { Renderer.name = "json"; render; files_of_url }

let doc blocks formatter =
  Yojson.Basic.pretty_print formatter
    (Odoc_document.Types.Block.serialize blocks)

/*
 * Copyright (c) 2016 Thomas Refis <trefis@janestreet.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

open React.StaticReact;

type uri =
  | Absolute(string)
  | Relative(string);

let page_creator =
    (~theme_uri=Relative("./"), ~url, name, header, toc, content) => {
  let is_page = Link.Path.is_page(url);
  let path = Link.Path.for_printing(url);
  let rec add_dotdot = (~n, acc) =>
    if (n <= 0) {
      acc;
    } else {
      add_dotdot(~n=n - 1, "../" ++ acc);
    };

  let resolve_relative_uri = uri => {
    /* Remove the first "dot segment". */
    let uri =
      if (String.length(uri) >= 2 && String.sub(uri, 0, 2) == "./") {
        String.sub(uri, 2, String.length(uri) - 2);
      } else {
        uri;
      };

    /* How deep is this page? */
    let n =
      List.length(path)
      - (
        /* This is just horrible. */
        if (is_page) {1} else {0}
      );

    add_dotdot(uri, ~n);
  };

  let head_element: element = {
    let title_string =
      Printf.sprintf("%s (%s)", name, String.concat(".", path));

    let theme_uri =
      switch (theme_uri) {
      | Absolute(uri) => uri
      | Relative(uri) => resolve_relative_uri(uri)
      };

    let support_files_uri = resolve_relative_uri("./");

    let odoc_css_uri = theme_uri ++ "odoc.css";
    let highlight_js_uri = support_files_uri ++ "highlight.pack.js";

    <head>
      <title> {string(title_string)} </title>
      <link rel="stylesheet" href=odoc_css_uri />
      <meta charset="utf-8" />
      <meta name="generator" content="odoc %%VERSION%%" />
      <meta name="viewport" content="width=device-width,initial-scale=1.0" />
      <script src=highlight_js_uri> {string("")} </script>
      <script
        dangerouslySetInnerHTML={__html: "hljs.initHighlightingOnLoad();"}
      />
    </head>;
  };

  let breadcrumbs = {
    let dot =
      if (Link.semantic_uris^) {
        "";
      } else {
        "index.html";
      };
    let dotdot = add_dotdot(~n=1, dot);
    let up_href =
      if (is_page && name != "index") {
        dot;
      } else {
        dotdot;
      };
    let has_parent = List.length(path) > 1;
    if (has_parent) {
      let l =
        [<a href=up_href> {string("Up")} </a>, string(" – ")]
        /* Create breadcrumbs */
        @ {
          let breadcrumb_spec =
            if (is_page) {
              (n, x) => (n, dot, x);
            } else {
              (n, x) => (n, add_dotdot(~n, dot), x);
            };

          let rev_path =
            if (is_page && name == "index") {
              List.tl(List.rev(path));
            } else {
              List.rev(path);
            };

          rev_path
          |> List.mapi(breadcrumb_spec)
          |> List.rev
          |> Utils.list_concat_map(
               ~sep=?Some([string(" "), entity("#x00BB"), string(" ")]),
               ~f=((n, addr, lbl)) =>
               if (n > 0) {
                 [[<a href=addr> {string(lbl)} </a>]];
               } else {
                 [[string(lbl)]];
               }
             )
          |> List.flatten;
        };

      [<nav> ...l </nav>];
    } else {
      [];
    };
  };

  let myHeader = React.StaticReact.header;
  let myBody =
    breadcrumbs
    @ [<myHeader> ...header </myHeader>]
    @ toc
    // @ [Html.div(~a=[Html.a_class(["content"])], content)];
    @ [<div class_="content"> ...{[string(""), ...content]} </div>];
  React.ReactDomStatic.[
    unsafe("<!DOCTYPE html>"),
    <html xmlns="http://www.w3.org/1999/xhtml">
      head_element
      <body> ...myBody </body>
    </html>,
  ];
  // Html.html(head, Html.body(body));
};

let make = (~theme_uri=?, ~url, ~header, ~toc, title, content, children) => {
  let filename = Link.Path.as_filename(url);
  let html = page_creator(~theme_uri?, ~url, title, header, toc, content);

  let content = React.ReactDomStatic.to_content(html);
  {Odoc_document.Renderer.filename, content, children};
};

let open_details = ref(true);

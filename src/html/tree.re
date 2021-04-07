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
  | Relative(option(Odoc_document.Url.Path.t));

let page_creator =
    (
      ~theme_uri=Relative(None),
      ~support_uri=Relative(None),
      ~url,
      name,
      header,
      toc,
      content,
    ) => {
  let is_leaf_page = Link.Path.is_leaf_page(url);
  let path = Link.Path.for_printing(url);
  let rec add_dotdot = (~n, acc) =>
    if (n <= 0) {
      acc;
    } else {
      add_dotdot(~n=n - 1, "../" ++ acc);
    };

  let head_element: element = {
    let title_string =
      Printf.sprintf("%s (%s)", name, String.concat(".", path));

    let file_uri = (base, file) =>
      switch (base) {
      | Absolute(uri) => uri ++ "/" ++ file
      | Relative(uri) =>
        let page: Odoc_document.Url.Path.t = {
          kind: "file",
          parent: uri,
          name: file,
        };

        Link.href(
          ~resolve=Current(url),
          Odoc_document.Url.Anchor.{page, anchor: "", kind: "file"},
        );
      };

    let odoc_css_uri = file_uri(theme_uri, "odoc.css");
    let highlight_js_uri = file_uri(support_uri, "highlight.pack.js");

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
      if (is_leaf_page && name != "index") {
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
            if (is_leaf_page) {
              (n, x) => (n, dot, x);
            } else {
              (n, x) => (n, add_dotdot(~n, dot), x);
            };

          let rev_path =
            if (is_leaf_page && name == "index") {
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

      [<nav class_="odoc-nav"> ...l </nav>];
    } else {
      [];
    };
  };

  let myHeader = React.StaticReact.header;
  let myBody =
    breadcrumbs
    @ [<myHeader class_="odoc-preamble"> ...header </myHeader>]
    @ toc
    // @ [Html.div(~a=[Html.a_class(["content"])], content)];
    @ [<div class_="odoc-content"> ...{[string(""), ...content]} </div>];
  React.ReactDomStatic.[
    unsafe("<!DOCTYPE html>"),
    <html xmlns="http://www.w3.org/1999/xhtml">
      head_element
      <body class_="odoc"> ...myBody </body>
    </html>,
  ];
};

let make =
    (
      ~theme_uri=?,
      ~support_uri=?,
      ~indent,
      ~url,
      ~header,
      ~toc,
      title,
      content,
      children,
    ) => {
  let filename = Link.Path.as_filename(url);
  let html =
    page_creator(
      ~theme_uri?,
      ~support_uri?,
      ~url,
      title,
      header,
      toc,
      content,
    );

  let content = React.ReactDomStatic.to_content(~indent, html);
  {Odoc_document.Renderer.filename, content, children};
};

let open_details = ref(true);

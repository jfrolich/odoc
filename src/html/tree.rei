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

open Odoc_document;
open React_renderer.StaticReact;

/** Supported languages for printing code parts. */;

type uri =
  | Absolute(string)
  | /** The type for absolute and relative URIs. The relative URIs are resolved
 using the HTML output directory as a target. */
    Relative(
      option(Odoc_document.Url.Path.t),
    );

/** {1 Page creator} */;

/** [make ?theme_uri (body, children)] calls "the page creator" to turn [body]
 into an [[ `Html ] elt]. If [theme_uri] is provided, it will be used to
 locate the theme files, otherwise the HTML output directory is used. */

let make:
  (
    ~theme_uri: uri=?,
    ~support_uri: uri=?,
    ~indent: bool,
    ~url: Url.Path.t,
    ~header: list(React.element),
    ~toc: list(React.element),
    string,
    list(React.element),
    list(Renderer.page)
  ) =>
  Renderer.page;

/* TODO: move to a centralized [State] module or something. Along with
   Relative_link.semantic_uris. */
/** Whether [<details>] tags should be opened by default or not.
 Default is [true]. */

let open_details: ref(bool);

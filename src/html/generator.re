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

open Odoc_document.Types;

module Doctree = Odoc_document.Doctree;
open React.StaticReact;

// let rec classNames = (~classStr: option(string)=?, classes) => {
//   switch((classStr, classes)) {
//   | (className, []) =>className
//   | (None, [className, ...classes]) => classNames(~classStr=?className, classes)
//   | (Some(classStr), [None, ...classes]) => classNames(~classStr, classes)
//   | (Some(classStr), [Some(className), ...classes]) => classNames(~classStr=(classStr ++ " " ++ className), classes)
//   }
// }

let rec classNames = (~classStr: option(string)=?, classes: list(string)) => {
  switch (classStr, classes) {
  | (className, []) => className
  | (None, [className, ...classes]) =>
    classNames(~classStr=className, classes)
  | (Some(classStr), [className, ...classes]) =>
    classNames(~classStr=classStr ++ " " ++ className, classes)
  };
};

module AnchorLink = {
  let createElement = (~id, ~classes=[], ~children as _, ()) => {
    <a href={"#" ++ id} class_=?{classNames(["anchor", ...classes])}>
      {string("")}
    </a>;
  };
};

module RawMarkup = {
  let createElement = (~t: Raw_markup.t, ~children as _=?, ()) => {
    switch (t) {
    | (`Html, content) => [
        React.ReactDomStatic.unsafe(content),
        // <div dangerouslySetInnerHTML={__html: content} />,
      ]
    | _ => [] /* Original comment: this is wrong */
    };
  };
};

module rec InlineElement: {
  let createElement:
    (
      ~emph_level: int=?,
      ~resolve: Link.resolve=?,
      ~content: Inline.t,
      ~children: list(element)=?,
      unit
    ) =>
    list(element);
} = {
  let createElement =
      (~emph_level=0, ~resolve=?, ~content, ~children as _=?, ()) => {
    content
    |> List.map((t: Inline.one) => {
         switch (t.desc, resolve) {
         | (Entity(s), _) =>
           let inner = entity(s);
           if (t.attr == []) {
             [inner];
           } else {
             [<span class_=?{classNames(t.attr)}> inner </span>];
           };
         | (Text(s), _) =>
           let inner = string(s);
           if (t.attr == []) {
             [inner];
           } else {
             [<span class_=?{classNames(t.attr)}> inner </span>];
           };
         | (Linebreak, _) => [<br />]
         | (Styled(style, c), resolve) =>
           <StyledElement style emph_level>
             ...<InlineElement
                  emph_level={
                    switch (style) {
                    | `Emphasis => emph_level + 1
                    | _ => emph_level
                    }
                  }
                  content=c
                  ?resolve
                />
           </StyledElement>
         | (Link(href, c), resolve) => [
             <a href class_=?{classNames(t.attr)}>
               ...<InlineElement emph_level content=c ?resolve />
             </a>,
           ]
         | (InternalLink(Resolved((_, _))), None) =>
           <InlineElement emph_level content />
         | (InternalLink(Resolved((uri, content))), Some(resolve)) => [
             <a href={Link.href(~resolve, uri)} class_=?{classNames(t.attr)}>
               ...<InlineElement emph_level content resolve />
             </a>,
           ]
         | (InternalLink(Unresolved(content)), resolve) => [
             <span class_=?{classNames(["xref-unresolved", ...t.attr])}>
               ...<InlineElement emph_level ?resolve content />
             </span>,
           ]
         | (Source(c), resolve) =>
           let container = element =>
             <InlineElement ?resolve emph_level content=element />;
           <SourceElement source=c container />;

         | (Raw_markup(r), _) => <RawMarkup t=r />
         }
       })
    |> List.concat;
  };
}
and SourceElement: {
  type container = Inline.t => list(element);
  let createElement:
    (
      ~container: container,
      ~source: list(Source.token),
      ~class_: list(string)=?,
      ~children: list(element)=?,
      unit
    ) =>
    list(element);
} = {
  type container = Inline.t => list(element);
  let createElement = (~container, ~source, ~class_=[], ~children as _=?, ()) => {
    let rec token = (x: Source.token) =>
      switch (x) {
      | Elt(i) => container(i)
      | Tag(None, l) => [<span> ...{tokens(l)} </span>]
      | Tag(Some(s), l) => [
          <span class_=?{classNames([s])}> ...{tokens(l)} </span>,
        ]
      }
    and tokens = tokens => tokens |> List.map(token) |> List.concat;
    switch (tokens(source)) {
    | [] => []
    | tokens => [<code class_=?{classNames(class_)}> ...tokens </code>]
    };
  };
}

and StyledElement: {
  let createElement:
    (~style: style, ~emph_level: int, ~children: list(element), unit) =>
    list(element);
} = {
  let createElement = (~style, ~emph_level, ~children, ()) => {
    switch ((style: style)) {
    | `Emphasis => [
        <em class_=?{classNames(emph_level mod 2 == 0 ? [] : ["odd"])}>
          ...children
        </em>,
      ]
    | `Bold => [<b> ...children </b>]

    | `Italic => [<i> ...children </i>]
    | `Superscript => [<sup> ...children </sup>]
    | `Subscript => [<sub> ...children </sub>]
    };
  };
};

// module Anchor = {
//   let createElement = (~anchor, ()) => {
//     switch (anchor) {
//     | None => []
//     | Some({Odoc_document.Url.Anchor.anchor, _}) => [
//         <a id=anchor href={"#" ++ anchor} class_=["anchor", "anchored"] />,
//       ]
//     };
//   };
// };

// let class_ = (l: Class.t) =>
//   if (l == []) {
//     [];
//   } else {
//     [Html.a_class(l)];
//   }
// and raw_markup = (t: Raw_markup.t) => {
//   let (target, content) = t;
//   if (`Html == target) {
//     [Html.Unsafe.data(content)];
//   } else {
//     [];
//       /* This is wrong */
//   };
// };

// module Details = {
//   let createElement = (~is_open, ~summary as passedSummary, ~children, ()) =>
//     if (is_open) {
//       <details> ...{[<summary> passedSummary </summary>, ...children]} </details>;
//     } else {
//       <details open_=()> ...{[<summary> passedSummary </summary>, ...children]} </details>;
//     };
// };

module Heading = {
  let createElement =
      (~resolve, ~label, ~level, ~title: Inline.t, ~children as _=?, ()) => {
    let id = label;
    let content =
      switch (id) {
      | Some(id) => [
          <a href={"#" ++ id} class_="anchor"> {string("")} </a>,
          ...<InlineElement resolve content=title />,
        ]
      | None => <InlineElement resolve content=title />
      };
    switch (level) {
    | 0 => <h1 ?id> ...content </h1>
    | 1 => <h2 ?id> ...content </h2>
    | 2 => <h3 ?id> ...content </h3>
    | 3 => <h4 ?id> ...content </h4>
    | 4 => <h5 ?id> ...content </h5>
    | _ => <h6 ?id> ...content </h6>
    };
  };
};

let rec block = (~resolve, l: Block.t): list(element) => {
  l
  |> List.map((t: Block.one) => {
       switch (t.desc) {
       | Inline(i) =>
         if (t.attr == []) {
           <InlineElement resolve content=i />;
         } else {
           [
             <span class_=?{classNames(t.attr)}>
               ...<InlineElement resolve content=i />
             </span>,
           ];
         }
       | Paragraph(i) => [
           <p class_=?{classNames(t.attr)}>
             ...<InlineElement resolve content=i />
           </p>,
         ]
       | List(Ordered, l) => [
           <ol class_=?{classNames(t.attr)}>
             ...{l |> List.map(x => <li> ...{block(~resolve, x)} </li>)}
           </ol>,
         ]

       | List(Unordered, l) => [
           <ul class_=?{classNames(t.attr)}>
             ...{
                  [
                    string(""),
                    ...l |> List.map(x => <li> ...{block(~resolve, x)} </li>),
                  ]
                }
           </ul>,
         ]
       | Description(l) => [
           <dl class_=?{classNames(t.attr)}>
             ...{
                  l
                  |> List.map(((i, b)) => {
                       [
                         <dt> ...<InlineElement resolve content=i /> </dt>,
                         <dd> ...{block(~resolve, b)} </dd>,
                       ]
                     })
                  |> List.concat
                }
           </dl>,
         ]
       | Raw_markup(r) => <RawMarkup t=r />
       | Verbatim(s) => [
           <pre class_=?{classNames(t.attr)}> {string(s)} </pre>,
         ]
       | Source(c) =>
         let container = element => <InlineElement resolve content=element />;
         [
           <pre class_=?{classNames(t.attr)}>
             ...<SourceElement source=c container />
           </pre>,
         ];
       }
     })
  |> List.concat;
};

let rec is_only_text = l => {
  let is_text: Item.t => _ =
    fun
    | Heading(_)
    | Text(_) => true
    | Declaration(_) => false
    | Include({content: {content, _}, _}) => is_only_text(content);

  List.for_all(is_text, l);
};

// let class_of_kind = kind =>
//   switch (kind) {
//   | Some(spec) => class_(["spec", spec])
//   | None => []
//   };

module Details = {
  let createElement =
      (~children: list(element), ~summary as summary_, ~open_, ()) => {
    <details open_>
      ...{
           [
             <summary> <span class_="def"> ...summary_ </span> </summary>,
             ...children,
           ]
         }
    </details>;
  };
};

let rec documentedSrc = (~resolve, t: DocumentedSrc.t): list(element) => {
  open DocumentedSrc;
  let take_code = l =>
    Doctree.Take.until(
      l,
      ~classify=
        fun
        | Code(code) => Accum(code)
        | Alternative(Expansion({summary, _})) => Accum(summary)
        | _ => Stop_and_keep,
    );

  let take_descr = l =>
    Doctree.Take.until(
      l,
      ~classify=
        fun
        | Documented({attrs, anchor, code, doc}) =>
          Accum([{DocumentedSrc.attrs, anchor, code: `D(code), doc}])
        | Nested({attrs, anchor, code, doc}) =>
          Accum([{DocumentedSrc.attrs, anchor, code: `N(code), doc}])
        | _ => Stop_and_keep,
    );

  let rec to_html = (t): list(element) =>
    switch (t) {
    | [] => []
    | [Code(_) | Alternative(_), ..._] =>
      let (code, _, rest) = take_code(t);
      let container = element => <InlineElement resolve content=element />;
      <SourceElement container source=code /> @ to_html(rest);
    | [Subpage(subp), ..._] => subpage(~resolve, subp)
    | [Documented(_) | Nested(_), ..._] =>
      let (l, _, rest) = take_descr(t);
      let one = ({attrs, anchor, code, doc}) => {
        let content =
          switch (code) {
          | `D(code) => <InlineElement resolve content=code />
          | `N(n) => to_html(n)
          };

        let doc =
          switch (block(~resolve, doc)) {
          | [] => []
          | block => [<td class_="doc"> ...block </td>]
          };

        switch (anchor) {
        | None =>
          <tr>
            ...{[<td class_=?{classNames(attrs)}> ...content </td>, ...doc]}
          </tr>
        | Some({Odoc_document.Url.Anchor.anchor, _}) =>
          <tr id=anchor class_="anchored">
            ...{
                 [
                   <td class_=?{classNames(attrs)}>
                     ...{[<AnchorLink id=anchor />, ...content]}
                   </td>,
                   ...doc,
                 ]
               }
          </tr>
        };
      };

      [<table> ...{List.map(one, l)} </table>, ...to_html(rest)];
    };

  to_html(t);
}
and subpage = (~resolve, subp: Subpage.t): list(element) =>
  items(~resolve, subp.content.items)
and items = (~resolve, l): list(element) =>
  [@tailcall]
  {
    let rec walk_items = (~only_text, acc, t: list(Item.t)): list(element) => {
      let continue_with = (rest, elts) =>
        walk_items(~only_text, List.rev_append(elts, acc), rest);

      switch (t) {
      | [] => List.rev(acc)
      | [Text(_), ..._] as t =>
        let (text, _, rest) =
          Doctree.Take.until(
            t,
            ~classify=
              fun
              | Item.Text(text) => Accum(text)
              | _ => Stop_and_keep,
          );

        let content = block(~resolve, text);
        let elts =
          if (only_text) {
            content;
          } else {
            [<aside> ...{[string(""), ...content]} </aside>];
          };

        elts |> continue_with(rest);
      | [Heading(h), ...rest] =>
        [<Heading resolve label={h.label} level={h.level} title={h.title} />]
        |> continue_with(rest)
      | [
          Include({
            kind,
            anchor,
            doc,
            content: {summary: summary_, status, content},
          }),
          ...rest,
        ] =>
        let included_html = items(content);
        let docs = block(~resolve, doc);
        let container = element => <InlineElement resolve content=element />;
        let summary_ = <SourceElement source=summary_ container />;

        let content = {
          switch (status) {
          | `Inline => included_html
          | other => [
              <Details
                open_={
                  switch (other) {
                  | `Closed => false
                  | `Open => true
                  | `Inline
                  | `Default => Tree.open_details^
                  }
                }
                summary=summary_>
                ...included_html
              </Details>,
            ]
          };
        };

        /* TODO : Why double div ??? */
        let content = [
          <div class_="doc"> ...{List.append(docs, content)} </div>,
        ];
        let classes =
          switch (kind) {
          | Some(spec) => ["spec", spec]
          | None => []
          };
        {
          switch (anchor) {
          | None => [
              <div>
                <div class_=?{classNames(classes)}> ...content </div>
              </div>,
            ]
          | Some({Odoc_document.Url.Anchor.anchor, _}) => [
              <div>
                <div
                  id=anchor class_=?{classNames(classes)} class2="anchored">
                  ...{[<AnchorLink id=anchor classes=[] />, ...content]}
                </div>
              </div>,
            ]
          };
        }
        |> continue_with(rest);

      | [Declaration({Item.kind, anchor, content, doc}), ...rest] =>
        let classes =
          switch (kind) {
          | Some(spec) => ["spec", spec]
          | None => []
          };
        let anchor_link =
          switch (anchor) {
          | None => []
          | Some({Odoc_document.Url.Anchor.anchor, _}) => [
              <AnchorLink id=anchor classes=[] />,
            ]
          };
        let id =
          switch (anchor) {
          | None => None
          | Some({Odoc_document.Url.Anchor.anchor, _}) => Some(anchor)
          };
        let content = anchor_link @ documentedSrc(~resolve, content);
        {
          switch (doc) {
          | [] => [
              <div
                class_=?{classNames(classes)}
                ?id
                class2=?{Option.map(_ => "anchored", id)}>
                ...content
              </div>,
            ]
          | docs => [
              <div>
                <div
                  class_=?{classNames(classes)}
                  ?id
                  class2=?{Option.map(_ => "anchored", id)}>
                  ...content
                </div>
                <div> ...{block(~resolve, docs)} </div>
              </div>,
            ]
          };
        }
        |> continue_with(rest);
      };
    }
    and items = l => walk_items(~only_text=is_only_text(l), [], l);
    items(l);
  };

module Toc = {
  open Odoc_document.Doctree;

  let render_toc = (toc: Toc.t) => {
    let rec section = ({Toc.anchor, text, children}) => {
      let link =
        <a href={"#" ++ anchor}> ...<InlineElement content=text /> </a>;

      switch (children) {
      | [] => [link]
      | _ => [link, sections(children)]
      };
    }
    and sections = the_sections =>
      <ul>
        ...{
             the_sections
             |> List.map(the_section => <li> ...{section(the_section)} </li>)
           }
      </ul>;

    switch (toc) {
    | [] => []
    | _ => [<nav class_="toc"> {sections(toc)} </nav>]
    };
  };

  let on_sub: Subpage.status => bool =
    fun
    | `Closed
    | `Open
    | `Default => false
    | `Inline => true;

  let from_items = i => render_toc @@ Toc.compute(~on_sub, i);
};

module Page = {
  let on_sub =
    fun
    | `Page(_) => None
    | `Include(x) =>
      switch (x.Include.status) {
      | `Closed
      | `Open
      | `Default => None
      | `Inline => Some(0)
      };

  let rec include_ = (~theme_uri=?, {Subpage.content, _}) => [
    page(~theme_uri?, content),
  ]
  and subpages = (~theme_uri=?, i) =>
    Utils.list_concat_map(~f=include_(~theme_uri?)) @@
    Doctree.Subpages.compute(i)
  and page = (~theme_uri=?, {Page.title, header, items: i, url} as p) => {
    let resolve = Link.Current(url);
    let i = Doctree.Shift.compute(~on_sub, i);
    let toc = Toc.from_items(i);
    let subpages = subpages(~theme_uri?, p);
    let header = items(~resolve, header);
    let content = items(~resolve, i);
    let page =
      Tree.make(~theme_uri?, ~header, ~toc, ~url, title, content, subpages);

    page;
  };
};

let render = (~theme_uri=?, page) => Page.page(~theme_uri?, page);

let doc = (~xref_base_uri, b) => {
  let resolve = Link.Base(xref_base_uri);
  block(~resolve, b);
};

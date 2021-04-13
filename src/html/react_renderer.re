module type REACT_RENDERER = {
  type element;
  type sep =
    | Space
    | Comma;

  type attrib_value =
    | AFloat(float)
    | AInt(int)
    | AStr(string)
    | AStrL(sep, list(string));

  let tag:
    (
      ~attrs: list(option((string, attrib_value))),
      ~children: list(element)=?,
      string
    ) =>
    element;
  let string: string => element;
  let entity: string => element;
  let unsafe: string => element;
  let list: list(element) => element;
};

module type REACT_ELEMENTS = {
  type unsafe_html = {__html: string};
  let div:
    (
      ~id: string=?,
      ~class_: string=?,
      ~class2: string=?,
      ~children: list(React.element)=?,
      ~dangerouslySetInnerHTML: unsafe_html=?,
      unit
    ) =>
    React.element;
};

module ReactDomStatic: {
  include REACT_RENDERER;
  let to_content: (~indent: bool, list(element), Format.formatter) => unit;
  let render_to_buffer: (~indent: bool, list(element)) => Buffer.t;
} = {
  // the source of this was originally copy-pasted from
  // https://github.com/ocsigen/tyxml/blob/lib/xml_print.ml

  type node = (bool, Format.formatter) => unit;
  type element =
    | El(node)
    | El_list(list(element));

  type sep =
    | Space
    | Comma;

  type attrib_value =
    | AFloat(float)
    | AInt(int)
    | AStr(string)
    | AStrL(sep, list(string));

  let is_control = c => {
    let cc = Char.code(c);
    cc <= 8 || cc == 11 || cc == 12 || 14 <= cc && cc <= 31 || cc == 127;
  };
  let emptytags = [
    "area",
    "base",
    "br",
    "col",
    "command",
    "embed",
    "hr",
    "img",
    "input",
    "keygen",
    "link",
    "meta",
    "param",
    "source",
    "wbr",
  ];

  module S = Set.Make(String);
  let is_emptytag =
    switch (emptytags) {
    | [] => (_ => false)
    | l =>
      let set = List.fold_left((s, x) => S.add(x, s), S.empty, l);
      (x => S.mem(x, set));
    };

  let add_unsafe_char = b =>
    fun
    | '<' => Buffer.add_string(b, "&lt;")
    | '>' => Buffer.add_string(b, "&gt;")
    | '"' => Buffer.add_string(b, "&quot;")
    | '&' => Buffer.add_string(b, "&amp;")
    | c when is_control(c) => {
        Buffer.add_string(b, "&#");
        Buffer.add_string(b, string_of_int(Char.code(c)));
        Buffer.add_string(b, ";");
      }
    | c => Buffer.add_char(b, c);

  let encode_unsafe_char = s => {
    let b = Buffer.create(String.length(s));
    String.iter(add_unsafe_char(b), s);
    Buffer.contents(b);
  };

  /* copied form js_of_ocaml: compiler/javascript.ml */
  let pp_number = (fmt, v) =>
    if (v == infinity) {
      Format.pp_print_string(fmt, "Infinity");
    } else if (v == neg_infinity) {
      Format.pp_print_string(fmt, "-Infinity");
    } else if (v != v) {
      Format.pp_print_string(fmt, "NaN");
    } else {
      let vint = int_of_float(v);
      /* compiler 1000 into 1e3 */
      if (float_of_int(vint) == v) {
        let rec div = (n, i) =>
          if (n != 0 && n mod 10 == 0) {
            div(n / 10, succ(i));
          } else if (i > 2) {
            Format.fprintf(fmt, "%de%d", n, i);
          } else {
            Format.pp_print_int(fmt, vint);
          };
        div(vint, 0);
      } else {
        let s1 = Printf.sprintf("%.12g", v);
        if (v == float_of_string(s1)) {
          Format.pp_print_string(fmt, s1);
        } else {
          let s2 = Printf.sprintf("%.15g", v);
          if (v == float_of_string(s2)) {
            Format.pp_print_string(fmt, s2);
          } else {
            Format.fprintf(fmt, "%.18g", v);
          };
        };
      };
    };
  let pp_noop = (_, _) => ();

  let open_box = (indent, fmt) =>
    if (indent) {
      Format.pp_open_box(fmt, 0);
    } else {
      ();
    };
  let close_box = (indent, fmt) =>
    if (indent) {
      Format.pp_close_box(fmt, ());
    } else {
      ();
    };
  let sp = (indent, fmt) =>
    if (indent) {
      Format.pp_print_space(fmt, ());
    } else {
      Format.pp_print_string(fmt, " ");
    };
  let cut = (indent, fmt) =>
    if (indent) {
      Format.pp_print_cut(fmt, ());
    } else {
      ();
    };

  let pp_encode = (encode, indent, fmt, s) => {
    let s = encode(s);
    if (indent) {
      Format.fprintf(fmt, "@[%a@]", Format.pp_print_text, s);
    } else {
      Format.pp_print_string(fmt, s);
    };
  };

  let pp_sep = indent =>
    fun
    | Space => ((fmt, ()) => sp(indent, fmt))
    | Comma => ((fmt, ()) => Format.fprintf(fmt, ",%t", sp(indent)));

  let pp_attrib_value = (encode, indent, fmt, a) =>
    switch (a) {
    | AFloat(f) => Format.fprintf(fmt, "\"%a\"", pp_number, f)
    | AInt(i) => Format.fprintf(fmt, "\"%d\"", i)
    | AStr(s) => Format.fprintf(fmt, "\"%s\"", encode(s))
    | AStrL(sep, slist) =>
      Format.fprintf(
        fmt,
        "\"%a\"",
        Format.pp_print_list(
          ~pp_sep=pp_sep(indent, sep),
          pp_encode(encode, indent),
        ),
        slist,
      )
    };

  let pp_attrib = (encode, indent, fmt, (attr_key, attr_value)) =>
    Format.fprintf(
      fmt,
      "%t%s=%a",
      sp(indent),
      attr_key,
      pp_attrib_value(encode, indent),
      attr_value,
    );

  let pp_attribs = (encode, indent) =>
    Format.pp_print_list(~pp_sep=pp_noop, pp_attrib(encode, indent));

  let pp_tag_and_attribs = (encode, indent, fmt, (tag, attrs)) => {
    open_box(indent, fmt);
    Format.fprintf(
      fmt,
      "%s%a%t",
      tag,
      pp_attribs(encode, indent),
      attrs |> List.filter_map(a => a),
      cut(indent),
    );
    close_box(indent, fmt);
  };

  let pp_closedtag = (encode, indent, fmt, tag, attrs) =>
    if (is_emptytag(tag)) {
      Format.fprintf(
        fmt,
        "<%a/>",
        pp_tag_and_attribs(encode, indent),
        (tag, attrs),
      );
    } else {
      open_box(indent, fmt);
      Format.fprintf(
        fmt,
        "<%a>%t</%s>",
        pp_tag_and_attribs(encode, indent),
        (tag, attrs),
        cut(indent),
        tag,
      );
      close_box(indent, fmt);
    };
  let pp_elts = indent => {
    Format.pp_print_list(
      ~pp_sep=(fmt, ()) => cut(indent, fmt),
      (fmt, el) => el(indent, fmt),
    );
  };
  let flatten_elements = t => {
    let rec internal = (cont: list(element), acc, t) =>
      switch (t) {
      | El(el) => internal(cont, [el, ...acc], El_list([]))
      | El_list([]) =>
        switch (cont) {
        | [] => acc
        | [hd, ...tl] => internal(tl, acc, hd)
        }
      | El_list([El(el), ...r]) => internal(cont, [el, ...acc], El_list(r))
      | El_list([l, ...r]) => internal([El_list(r), ...cont], acc, l)
      };
    internal([], [], t) |> List.rev;
  };
  let tag = (~attrs, ~children=[], tag) =>
    El(
      (indent, fmt) => {
        switch (children) {
        | [] => pp_closedtag(encode_unsafe_char, indent, fmt, tag, attrs)
        | children =>
          open_box(indent, fmt);
          Format.fprintf(
            fmt,
            "<%t%a>%t%a%t%t</%s>",
            open_box(indent),
            pp_tag_and_attribs(encode_unsafe_char, indent),
            (tag, attrs),
            cut(indent),
            pp_elts(indent),
            flatten_elements(El_list(children)),
            close_box(indent),
            cut(indent),
            tag,
          );
          close_box(indent, fmt);
        }
      },
    );
  let unsafe = str => El((_indent, fmt) => Format.fprintf(fmt, "%s", str));
  let entity = str => El((_, fmt) => Format.fprintf(fmt, "&%s;", str));

  let string = str =>
    El(
      // it's a leave node... don't need to capture this
      (_, fmt) => Format.fprintf(fmt, "%s", encode_unsafe_char(str)),
    );

  let to_content = (~indent, elements, formatter) => {
    pp_elts(indent, formatter, flatten_elements(El_list(elements)));
  };

  let render_to_buffer = (~indent, elements) => {
    let buffer = Buffer.create(1000);
    let formatter = Format.formatter_of_buffer(buffer);
    pp_elts(indent, formatter, flatten_elements(El_list(elements)));
    buffer;
  };

  let list = elem_list => El_list(elem_list);
};

module Make = (Renderer: REACT_RENDERER) => {
  module React = Renderer;
  type unsafe_html = {__html: string};
  let str_attr = (name, attr) =>
    attr |> Option.map(attr => (name, Renderer.AStr(attr)));
  let div =
      (
        ~id=?,
        ~class_=?,
        ~class2=?,
        ~children=?,
        ~dangerouslySetInnerHTML=?,
        (),
      ) => {
    Renderer.tag(
      ~attrs=[
        str_attr("class", class_),
        str_attr("id", id),
        str_attr("class", class2),
      ],
      ~children=?
        switch (dangerouslySetInnerHTML) {
        | Some({__html: html}) => Some([Renderer.unsafe(html)])
        | None => children
        },
      "div",
    );
  };
  let span = (~id=?, ~class_=?, ~children=?, ()) => {
    Renderer.tag(
      ~attrs=[str_attr("class", class_), str_attr("id", id)],
      ~children?,
      "span",
    );
  };
  let code = (~id=?, ~class_=?, ~children=?, ()) => {
    Renderer.tag(
      ~attrs=[str_attr("class", class_), str_attr("id", id)],
      ~children?,
      "code",
    );
  };
  let a = (~href, ~class_=?, ~id=?, ~children=?, ()) => {
    Renderer.tag(
      ~attrs=[
        Some(("href", AStr(href))),
        str_attr("class", class_),
        str_attr("id", id),
      ],
      ~children?,
      "a",
    );
  };

  let p = (~class_=?, ~id=?, ~children=?, ()) => {
    Renderer.tag(
      ~attrs=[str_attr("class", class_), str_attr("id", id)],
      ~children?,
      "p",
    );
  };
  let nav = (~class_=?, ~id=?, ~children=?, ()) => {
    Renderer.tag(
      ~attrs=[str_attr("class", class_), str_attr("id", id)],
      ~children?,
      "nav",
    );
  };
  let details = (~open_, ~class_=?, ~id=?, ~children=?, ()) => {
    Renderer.tag(
      ~attrs=[
        open_ ? Some(("open", AStr("open"))) : None,
        str_attr("class", class_),
        str_attr("id", id),
      ],
      ~children?,
      "details",
    );
  };
  let summary = (~class_=?, ~id=?, ~children=?, ()) => {
    Renderer.tag(
      ~attrs=[str_attr("class", class_), str_attr("id", id)],
      ~children?,
      "summary",
    );
  };
  let ol = (~class_=?, ~id=?, ~children=?, ()) => {
    Renderer.tag(
      ~attrs=[str_attr("class", class_), str_attr("id", id)],
      ~children?,
      "ol",
    );
  };
  let ul = (~class_=?, ~id=?, ~children=?, ()) => {
    Renderer.tag(
      ~attrs=[str_attr("class", class_), str_attr("id", id)],
      ~children?,
      "ul",
    );
  };
  let tr = (~class_=?, ~id=?, ~children=?, ()) => {
    Renderer.tag(
      ~attrs=[str_attr("id", id), str_attr("class", class_)],
      ~children?,
      "tr",
    );
  };
  let td = (~class_=?, ~id=?, ~children=?, ()) => {
    Renderer.tag(
      ~attrs=[str_attr("class", class_), str_attr("id", id)],
      ~children?,
      "td",
    );
  };
  let table = (~class_=?, ~id=?, ~children=?, ()) => {
    Renderer.tag(
      ~attrs=[str_attr("class", class_), str_attr("id", id)],
      ~children?,
      "table",
    );
  };
  let tbody = (~class_=?, ~id=?, ~children=?, ()) => {
    Renderer.tag(
      ~attrs=[str_attr("class", class_), str_attr("id", id)],
      ~children?,
      "tbody",
    );
  };
  let aside = (~class_=?, ~id=?, ~children=?, ()) => {
    Renderer.tag(
      ~attrs=[str_attr("class", class_), str_attr("id", id)],
      ~children?,
      "aside",
    );
  };
  let li = (~class_=?, ~id=?, ~children=?, ()) => {
    Renderer.tag(
      ~attrs=[str_attr("class", class_), str_attr("id", id)],
      ~children?,
      "li",
    );
  };
  let dl = (~class_=?, ~id=?, ~children=?, ()) => {
    Renderer.tag(
      ~attrs=[str_attr("class", class_), str_attr("id", id)],
      ~children?,
      "dl",
    );
  };
  let dd = (~class_=?, ~id=?, ~children=?, ()) => {
    Renderer.tag(
      ~attrs=[str_attr("class", class_), str_attr("id", id)],
      ~children?,
      "dd",
    );
  };
  let dt = (~class_=?, ~id=?, ~children=?, ()) => {
    Renderer.tag(
      ~attrs=[str_attr("class", class_), str_attr("id", id)],
      ~children?,
      "dt",
    );
  };
  let pre = (~class_=?, ~id=?, ~children=?, ()) => {
    Renderer.tag(
      ~attrs=[str_attr("class", class_), str_attr("id", id)],
      ~children?,
      "pre",
    );
  };

  let html = (~xmlns=?, ~class_=?, ~id=?, ~children=?, ()) => {
    Renderer.tag(
      ~attrs=[
        str_attr("xmlns", xmlns),
        str_attr("class", class_),
        str_attr("id", id),
      ],
      ~children?,
      "html",
    );
  };
  let head = (~class_=?, ~id=?, ~children=?, ()) => {
    Renderer.tag(
      ~attrs=[str_attr("class", class_), str_attr("id", id)],
      ~children?,
      "head",
    );
  };
  let body = (~class_=?, ~id=?, ~children=?, ()) => {
    Renderer.tag(
      ~attrs=[str_attr("class", class_), str_attr("id", id)],
      ~children?,
      "body",
    );
  };
  let header = (~class_=?, ~id=?, ~children=?, ()) => {
    Renderer.tag(
      ~attrs=[str_attr("class", class_), str_attr("id", id)],
      ~children?,
      "header",
    );
  };
  let title = (~class_=?, ~id=?, ~children=?, ()) => {
    Renderer.tag(
      ~attrs=[str_attr("class", class_), str_attr("id", id)],
      ~children?,
      "title",
    );
  };
  let script = (~src=?, ~id=?, ~children=?, ~dangerouslySetInnerHTML=?, ()) => {
    Renderer.tag(
      ~attrs=[str_attr("src", src), str_attr("id", id)],
      ~children=?
        switch (dangerouslySetInnerHTML) {
        | Some({__html: html}) => Some([Renderer.unsafe(html)])
        | None => children
        },
      "script",
    );
  };
  let link = (~rel=?, ~href=?, ~class_=?, ~id=?, ~children=?, ()) => {
    Renderer.tag(
      ~attrs=[
        str_attr("rel", rel),
        str_attr("href", href),
        str_attr("class", class_),
        str_attr("id", id),
      ],
      ~children?,
      "link",
    );
  };
  let meta =
      (~charset=?, ~name=?, ~content=?, ~class_=?, ~id=?, ~children=?, ()) => {
    Renderer.tag(
      ~attrs=[
        str_attr("charset", charset),
        str_attr("name", name),
        str_attr("content", content),
        str_attr("class", class_),
        str_attr("id", id),
      ],
      ~children?,
      "meta",
    );
  };

  let makeMarkup = (tagName, ~class_=?, ~id=?, ~children=?, ()) => {
    Renderer.tag(
      ~attrs=[str_attr("class", class_), str_attr("id", id)],
      ~children?,
      tagName,
    );
  };
  let em = makeMarkup("em");
  let b = makeMarkup("b");
  let i = makeMarkup("i");
  let sup = makeMarkup("sup");
  let sub = makeMarkup("sub");

  let makeHeading = (tagName, ~class_=?, ~id=?, ~children=?, ()) => {
    Renderer.tag(
      ~attrs=[str_attr("class", class_), str_attr("id", id)],
      ~children?,
      tagName,
    );
  };

  let h1 = makeHeading("h1");
  let h2 = makeHeading("h2");
  let h3 = makeHeading("h3");
  let h4 = makeHeading("h4");
  let h5 = makeHeading("h5");
  let h6 = makeHeading("h6");

  let br = (~children as _=?, ()) => {
    Renderer.tag(~attrs=[], "br");
  };
};

module StaticReact = Make(ReactDomStatic);

let bla = {
  StaticReact.(<div />);
};

type json = Yojson.Basic.t

let mk_type type_ list = `Assoc (("type", `String type_) :: list)

let serialize_status = function
  | `Inline -> `String "Inline"
  | `Open -> `String "Open"
  | `Closed -> `String "Closed"
  | `Default -> `String "Default"

module Style = struct
  type t = [ `Bold | `Italic | `Emphasis | `Superscript | `Subscript ]

  let serialize : t -> json = function
    | `Bold -> `String "Bold"
    | `Italic -> `String "Italic"
    | `Emphasis -> `String "Emphasis"
    | `Superscript -> `String "Superscript"
    | `Subscript -> `String "Subscript"
end

module Class = struct
  type t = string list

  let serialize : t -> json = fun t -> `List (List.map (fun el -> `String el) t)
end

module UrlType = struct
  module Path = struct
    type t = { kind : string; parent : t option; name : string }

    let rec serialize : t -> json =
     fun t ->
      `Assoc
        [
          ("kind", `String t.kind);
          ("parent", match t.parent with None -> `Null | Some t -> serialize t);
          ("name", `String t.name);
        ]
  end

  type t = {
    page : Path.t;
    anchor : string;
        (** Anchor in {!field-page} where the element is attached *)
    kind : string;
        (** What kind of element the path points to.
        e.g. "module", "module-type", "exception", ... *)
  }

  let serialize : t -> json =
   fun t ->
    `Assoc
      [
        ("page", Path.serialize t.page);
        ("anchor", `String t.anchor);
        ("kind", `String t.kind);
      ]
end

module Raw_markup = struct
  type target = string

  type t = target * string

  let serialize : t -> json = fun (a, b) -> `List [ `String a; `String b ]
end

module rec InternalLink : sig
  type resolved = UrlType.t * Inline.t

  type unresolved = Inline.t

  type t = Resolved of resolved | Unresolved of Inline.t

  val serialize : t -> Yojson.Basic.t
end = struct
  type resolved = UrlType.t * Inline.t

  type unresolved = Inline.t

  type t = Resolved of resolved | Unresolved of Inline.t

  let serialize = function
    | Resolved (url, inline) ->
        `Assoc
          [
            ("url", UrlType.serialize url); ("inline", Inline.serialize inline);
          ]
    | Unresolved inline -> `Assoc [ ("inline", Inline.serialize inline) ]
end

and Source : sig
  type tag = string option

  type t = token list

  and token = Elt of Inline.t | Tag of tag * t

  val serialize : t -> json
end = struct
  type tag = string option

  type t = token list

  and token = Elt of Inline.t | Tag of tag * t

  let rec serialize t =
    `List
      (List.map
         (function
           | Elt inline -> Inline.serialize inline
           | Tag (tag, source) ->
               `Assoc
                 [
                   ( "tag",
                     match tag with Some tag -> `String tag | None -> `Null );
                   ("source", serialize source);
                 ])
         t)
end

and Inline : sig
  type entity = string

  type href = string

  type t = one list

  and one = { attr : Class.t; desc : desc }

  and desc =
    | Text of string
    | Entity of entity
    | Linebreak
    | Styled of Style.t * t
    | Link of href * t
    | InternalLink of InternalLink.t
    | Source of Source.t
    | Raw_markup of Raw_markup.t

  val serialize : t -> json
end = struct
  type entity = string

  type href = string

  type t = one list

  and one = { attr : Class.t; desc : desc }

  and desc =
    | Text of string
    | Entity of entity
    | Linebreak
    | Styled of Style.t * t
    | Link of href * t
    | InternalLink of InternalLink.t
    | Source of Source.t
    | Raw_markup of Raw_markup.t

  let rec serialize t =
    `List
      (List.map
         (fun { attr; desc } ->
           `Assoc
             [
               ("attr", Class.serialize attr);
               ( "desc",
                 match desc with
                 | Text text ->
                     `Assoc
                       [ ("type", `String "Text"); ("value", `String text) ]
                 | Entity entity ->
                     `Assoc
                       [ ("type", `String "Entity"); ("value", `String entity) ]
                 | Linebreak -> `Assoc [ ("type", `String "Linebreak") ]
                 | Styled (style, inline) ->
                     `Assoc
                       [
                         ("type", `String "Styled");
                         ("style", Style.serialize style);
                         ("inline", serialize inline);
                       ]
                 | Link (href, inline) ->
                     `Assoc
                       [
                         ("type", `String "Link");
                         ("href", `String href);
                         ("inline", serialize inline);
                       ]
                 | InternalLink internal_link ->
                     `Assoc
                       [
                         ("type", `String "InternalLink");
                         ("value", InternalLink.serialize internal_link);
                       ]
                 | Source source ->
                     `Assoc
                       [
                         ("type", `String "Source");
                         ("value", Source.serialize source);
                       ]
                 | Raw_markup raw_markup ->
                     `Assoc
                       [
                         ("type", `String "Raw_markup");
                         ("value", Raw_markup.serialize raw_markup);
                       ] );
             ])
         t)
end

and Description : sig
  type one = { attr : Class.t; key : Inline.t; definition : Block.t }

  type t = one list

  val serialize : t -> json
end = struct
  type one = { attr : Class.t; key : Inline.t; definition : Block.t }

  type t = one list

  let serialize t =
    `List
      (List.map
         (fun { attr; key; definition } ->
           `Assoc
             [
               ("attr", Class.serialize attr);
               ("key", Inline.serialize key);
               ("definition", Block.serialize definition);
             ])
         t)
end

and Heading : sig
  type t = { label : string option; level : int; title : Inline.t }

  val serialize : t -> json
end = struct
  type t = { label : string option; level : int; title : Inline.t }

  let serialize { label; level; title } =
    `Assoc
      [
        ("label", match label with Some label -> `String label | None -> `Null);
        ("level", `Int level);
        ("title", Inline.serialize title);
      ]
end

and Block : sig
  type t = one list

  and one = { attr : Class.t; desc : desc }

  and desc =
    | Inline of Inline.t
    | Paragraph of Inline.t
    | List of list_type * t list
    | Description of Description.t
    | Source of Source.t
    | Verbatim of string
    | Raw_markup of Raw_markup.t

  and list_type = Ordered | Unordered

  val serialize : t -> json
end = struct
  type t = one list

  and one = { attr : Class.t; desc : desc }

  and desc =
    | Inline of Inline.t
    | Paragraph of Inline.t
    | List of list_type * t list
    | Description of Description.t
    | Source of Source.t
    | Verbatim of string
    | Raw_markup of Raw_markup.t

  and list_type = Ordered | Unordered

  let serialize_list_type = function
    | Ordered -> `String "Ordered"
    | Unordered -> `String "Unordered"

  let rec serialize_desc : desc -> json = function
    | Inline v -> mk_type "Inline" [ ("value", Inline.serialize v) ]
    | Paragraph v -> mk_type "Paragraph" [ ("value", Inline.serialize v) ]
    | List (list_type, list) ->
        mk_type "List"
          [
            ("listType", serialize_list_type list_type);
            ("list", `List (List.map serialize list));
          ]
    | Description description ->
        mk_type "Description" [ ("value", Description.serialize description) ]
    | Source v -> mk_type "Source" [ ("value", Source.serialize v) ]
    | Verbatim v -> mk_type "Verbatim" [ ("value", `String v) ]
    | Raw_markup v -> mk_type "Raw_markup" [ ("value", Raw_markup.serialize v) ]

  and serialize_one { attr; desc } =
    `Assoc [ ("attr", Class.serialize attr); ("desc", serialize_desc desc) ]

  and serialize t = `List (List.map serialize_one t)
end

and DocumentedSrc : sig
  type 'a documented = {
    attrs : Class.t;
    anchor : UrlType.t option;
    code : 'a;
    doc : Block.t;
    markers : string * string;
  }

  type t = one list

  and one =
    | Code of Source.t
    | Documented of Inline.t documented
    | Nested of t documented
    | Subpage of Subpage.t
    | Alternative of Alternative.t

  val serialize : t -> json
end = struct
  type 'a documented = {
    attrs : Class.t;
    anchor : UrlType.t option;
    code : 'a;
    doc : Block.t;
    markers : string * string;
  }

  type t = one list

  and one =
    | Code of Source.t
    | Documented of Inline.t documented
    | Nested of t documented
    | Subpage of Subpage.t
    | Alternative of Alternative.t

  let serialize_documented : 'a documented -> ('a -> json) -> json =
   fun { attrs; anchor; code; doc; markers = marker_a, marker_b } serialize_code ->
    `Assoc
      [
        ("attrs", Class.serialize attrs);
        ( "anchor",
          match anchor with
          | Some anchor -> UrlType.serialize anchor
          | None -> `Null );
        ("code", serialize_code code);
        ("doc", Block.serialize doc);
        ("markers", `List [ `String marker_a; `String marker_b ]);
      ]

  let rec serialize_one : one -> json = function
    | Code v -> mk_type "Code" [ ("value", Source.serialize v) ]
    | Documented v ->
        mk_type "Documented"
          [ ("value", serialize_documented v Inline.serialize) ]
    | Nested v ->
        mk_type "Nested" [ ("value", serialize_documented v serialize) ]
    | Subpage v -> mk_type "Subpage" [ ("value", Subpage.serialize v) ]
    | Alternative v ->
        mk_type "Alternative" [ ("value", Alternative.serialize v) ]

  and serialize t = `List (List.map serialize_one t)
end

and Alternative : sig
  type expansion = {
    status : [ `Inline | `Open | `Closed | `Default ];
    summary : Source.t;
    expansion : DocumentedSrc.t;
    url : UrlType.Path.t;
  }

  type t = Expansion of expansion

  val serialize : t -> json
end = struct
  type expansion = {
    status : [ `Inline | `Open | `Closed | `Default ];
    summary : Source.t;
    expansion : DocumentedSrc.t;
    url : UrlType.Path.t;
  }

  type t = Expansion of expansion

  let serialize = function
    | Expansion { status; summary; expansion; url } ->
        mk_type "Expansion"
          [
            ("status", serialize_status status);
            ("summary", Source.serialize summary);
            ("expansion", DocumentedSrc.serialize expansion);
            ("url", UrlType.Path.serialize url);
          ]
end

and Subpage : sig
  type status = [ `Inline | `Open | `Closed | `Default ]

  type t = { status : status; content : Page.t }

  val serialize : t -> json
end = struct
  type status = [ `Inline | `Open | `Closed | `Default ]

  type t = { status : status; content : Page.t }

  let serialize { status; content } =
    `Assoc
      [
        ("status", serialize_status status); ("content", Page.serialize content);
      ]
end

and Include : sig
  type status = [ `Inline | `Open | `Closed | `Default ]

  type t = { status : status; content : Item.t list; summary : Source.t }

  val serialize : t -> json
end = struct
  type status = [ `Inline | `Open | `Closed | `Default ]

  type t = { status : status; content : Item.t list; summary : Source.t }

  let serialize { status; content; summary } =
    `Assoc
      [
        ("status", serialize_status status);
        ("content", `List (List.map Item.serialize content));
        ("summary", Source.serialize summary);
      ]
end

and Item : sig
  type 'a item = {
    attr : Class.t;
    anchor : UrlType.t option;
    content : 'a;
    doc : Block.t;
  }

  type declaration = DocumentedSrc.t item

  type text = Block.t

  type t =
    | Text of text
    | Heading of Heading.t
    | Declaration of DocumentedSrc.t item
    | Include of Include.t item

  val serialize : t -> json
end = struct
  type 'a item = {
    attr : Class.t;
    anchor : UrlType.t option;
    content : 'a;
    doc : Block.t;
  }

  let serialize_item : 'a item -> ('a -> json) -> json =
   fun { attr; anchor; content; doc } serialize_content ->
    `Assoc
      [
        ("attr", Class.serialize attr);
        ( "anchor",
          match anchor with
          | Some anchor -> UrlType.serialize anchor
          | None -> `Null );
        ("content", serialize_content content);
        ("doc", Block.serialize doc);
      ]

  type declaration = DocumentedSrc.t item

  type text = Block.t

  type t =
    | Text of text
    | Heading of Heading.t
    | Declaration of DocumentedSrc.t item
    | Include of Include.t item

  let serialize = function
    | Text v -> mk_type "Text" [ ("value", Block.serialize v) ]
    | Heading v -> mk_type "Heading" [ ("value", Heading.serialize v) ]
    | Declaration v ->
        mk_type "Declaration"
          [ ("value", serialize_item v DocumentedSrc.serialize) ]
    | Include v ->
        mk_type "Declaration" [ ("value", serialize_item v Include.serialize) ]
end

and Page : sig
  type t = {
    title : string;
    header : Item.t list;
    items : Item.t list;
    url : UrlType.Path.t;
  }

  val serialize : t -> json
end = struct
  type t = {
    title : string;
    header : Item.t list;
    items : Item.t list;
    url : UrlType.Path.t;
  }

  let serialize { title; header; items; url } =
    `Assoc
      [
        ("title", `String title);
        ("header", `List (List.map Item.serialize header));
        ("items", `List (List.map Item.serialize items));
        ("url", UrlType.Path.serialize url);
      ]
end

let inline ?(attr = []) desc = Inline.{ attr; desc }

let block ?(attr = []) desc = Block.{ attr; desc }

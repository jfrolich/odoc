open Odoc_document;

let render:
  (~theme_uri: Tree.uri=?, ~indent: bool, Types.Page.t) => Renderer.page;

let doc:
  (~xref_base_uri: string, Types.Block.t, ~indent: bool, Format.formatter) =>
  unit;

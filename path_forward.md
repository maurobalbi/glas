Features for 1.0:

  ✓ Finish parser: attribute syntax, pattern guards, record spread 
  ✓ Finish inference for Result, Tuple, Case multiple subjects
  ✓ finish type_from_ast
  ✓ type alias
  ✓ fix parsing for panic, <>
  - Optimization: if function is annotated, skip inferring!
  - fix make_ast for types with module def, e.g. wisp.Response
  - Fix pattern completion (e.g. only show constructors)
  ✓  Fix pattern inference for module types (e.g. local.Wobbler)
  ✓ Fix Record Spread
  - Fix function label / Record label lowering + inference (Unsaturated Constructors are inferred as fn (...) -> Constr)
  - Finish hover for names (e.g. Adt, Variant, docs, etc)
  - Add signature help
  - Add Inlay - Hints ?
  - Fix call completion in pipes
  - Types completion (additional trigger char ':')
  - Reimplement target based compiliation
  - Find references, highlight related
  - Add rename assists!
  - completion expr.use, maybe more?
  - improve documentation

After
  - Add some more diagnostics
    - Double declaration
    - wrong labelling
    - Unexpected spreads
    - Error reporting: 
      https://www.reddit.com/r/ProgrammingLanguages/comments/i2hfti/strategies_for_displaying_type_errors_with_global/
    - experiment with type-inference diagnostics
# Changelog

## 0.7.0

- **Breaking API change:** the offset field of `Pos` and the result of `posCoff`
  now use `Maybe Int`. Wrap known offsets in `Just` and use `Nothing` for unknown
  offsets. `startPos` supplies `Just 0`, `linePos` supplies `Nothing`, and
  `advancePos` preserves unknown offsets. The `Read`, `Show`, and generic `Data`
  representations change accordingly, including positions nested in other types.
- Make `Pos` equality consistent with ordering by comparing only filename,
  line, and column. `Loc` comparisons consequently ignore endpoint offsets too.
- Merge offsets conservatively at matching endpoint coordinates. Retain an
  offset only when both are known and equal. Unknown or conflicting offsets
  produce `Nothing`. Span combination is associative, commutative, and
  idempotent, including offset information.
- Bound stack usage when aggregating finite lists through `mconcat`, `sconcat`,
  and the default `locOfList`, including aggregation through `locOf` on lists.
- Preserve both filenames when displaying cross-file spans, for example
  `a.hs:1:1-b.hs:1:1`. Same-file display formats are unchanged.
- Add GHC 9.12 and 9.14 to CI. Require `base >= 4.9`, matching the supported
  GHC 8.0+ range, and remove obsolete compatibility code.
- Restore GitHub Actions CI and retire Travis CI. Add baseline, property, and
  regression tests, with test execution from source distributions in CI.
- Move library sources into `src/`, enable library warnings, and configure
  Stylish Haskell and VS Code formatting.
- Document position conventions, offset migration, combination laws, and
  wrapper behavior in the `Data.Loc` Haddock documentation.

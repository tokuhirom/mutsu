# Regex captures: shared MatchTarget subject, stored text axis removed where spans live (ADR-0016 P3a)

The regex engine's captures no longer store their matched text alongside the
span where a span already exists. A new `MatchTarget { text: Arc<String>,
chars: Arc<[char]> }` is built once per engine entry point and shared by the
whole capture tree: the entry publishes it on the returned accumulator
(`RegexCaptures::target`), every lazy `MatchNode` carries it (so `.orig` is an
`Arc` bump and `.Str` derives from the recorded span, with an ASCII byte-slice
fast path), and a thread-local engine scope covers mid-match Match synthesis
(`<?{ … }>` `.made` dispatch, reduce-time `$*` action runs).

On top of that subject, three stored-text redundancies are deleted outright:
`CapNode.matched` and `RegexCaptures.matched` (all ~25 `chars[a..b].collect()`
whole-match writes removed; readers use `span_str`), the `String` element of
`QuantifiedCaptureEntry` (now `(from, to, subcap)`), and the text in
`positional_slots`. The P5 leaf position search — recover a leaf's offsets by
scanning the subject for its text, wrong for repeated text — is retired; every
leaf that reaches the Match builder carries a recorded span.

Landing this surfaced that `positional_offsets` was not maintained by every
positional producer: the quantified-capture fold fell back to fabricating
`0..len` spans (the exact bug class ADR-0016 Cause 2 describes), which stored
text used to paper over. Every positional push/merge site now keeps the
offsets axis aligned, pinned by `t/subst-closure-quantified-capture.t`.

Two compatibility fixes came out of deriving text from spans:

- `:m` (ignoremark) and `:i` fold-expansion matches now remap the entire
  capture tree — including sub-captures — back to original-subject space.
  Previously sub-captures kept derived-space offsets and derived-space text:
  `"cafés" ~~ m:m/ caf (e) s /` captured `"e"`; raku (and now mutsu) captures
  `"é"` with the correct original-space span.
- The pcre2 `:P5` path reported byte offsets in `.from`/`.to` and the
  positional axes; they are now char offsets like every other path.

The accumulator's `named`/`positional` text vectors still exist — removing
them is structurally the P4 axis collapse (`HashMap<Symbol, Vec<Arc<CapNode>>>`
/ `Vec<PosSlot>`), where the remaining per-capture text `collect`s disappear.
Local interleaved A/B puts this intermediate state at ≈ +2–3 % on
`bench-yaml-parse`; the bench-CI history (int-arith-normalized) is the
authoritative number, and the cost is expected back with P4.

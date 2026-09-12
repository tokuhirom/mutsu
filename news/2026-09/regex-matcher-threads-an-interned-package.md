# The regex matcher threads its package as an interned Symbol

The regex/grammar matcher carried its dispatch package as a `pkg: &str` through
every function of its call chain, and the memoized subrule resolver at the
bottom of that chain re-interned it on every probe: **102,685 interns on a
60-row YAML parse, 1.0% of the whole program**, and the largest single caller of
`Symbol::intern`. Three rounds of [#7576](https://github.com/tokuhirom/mutsu/issues/7576)
named the fix — thread a `Symbol` instead of re-deriving one — and deferred it
each time as too wide to bolt on: "dozens of signatures across the regex files".

It is a type change, so the compiler finds every site. 66 signatures in
`src/runtime/regex/` take `pkg: Symbol` now. The twelve entry points that used to
open with `let pkg = self.current_package()` read the atomic mirror
(`current_package_sym()`) instead — a relaxed load rather than a `String` clone
out of an `RwLock` — and the resolver's cache key is the symbol it was handed.

What the change pulls along with it, each **removing** work rather than making it
cheaper:

- `ParsedTokenCandidate` and the raw resolution tuple carry the dispatch package
  as a `Symbol`. A candidate is resolved once and then used at every match
  position, where the `String` was cloned per use and re-interned to be
  compared; `resolve_token_patterns_*` also stops calling `def.package.resolve()`,
  which allocated a `String` per candidate out of a symbol the registry already
  held.
- `parse_candidate_in_pkg` compares two `u32`s to decide whether to switch
  packages, instead of cloning the current package name to compare it.
- The call graph's `RuleNode` is `(Symbol, Symbol)`. The reachability walk
  hashes, compares and stores these by the thousand, and each node allocated two
  `String`s to be compared once; the node is `Copy` now, so the walk's `clone()`s
  go too.
- `STREAMABLE`, probed before every `<subrule>` call is resolved, is keyed by
  `Symbol` rather than by package text.

Measured under callgrind on the 60-row document: **1,531,462,938 -> 1,494,195,378
Ir (-2.43%)**, with `Symbol::intern` going **385,083 -> 210,464 calls (-45%)**,
`LocalKey::with` 8.69% -> 7.07% of the program, and `memcmp` 1.63% -> 1.40%.
Cumulative for the round: **1,559,607,721 -> 1,494,195,378 (-4.19%)**.

## The latent bug it surfaced

Eight regex scratch interpreters built themselves with a struct literal that
overrode `current_package` — the `RwLock<String>` — but left
`current_package_sym`, the atomic mirror `current_package_sym()` reads, at the
scratch default. A scratch therefore answered for the wrong package. Nothing on
the old `&str` chain consulted the mirror, which is why it sat there unnoticed;
`parse_candidate_in_pkg` consults it now, and the first release build of this
change failed to parse the YAML benchmark at all.

Both fields are set at every one of those sites now, as `runtime_thread.rs`
already did. Note what did *not* catch it: `t/grammar` (78 files) and `t/regex`
(258 files) were both green on the broken build — the failure needed a
cross-package grammar subrule, which only the bundled `YAMLish` battery
exercises.

**Method note.** A duplicated piece of state with one authoritative copy and one
mirror is only as sound as its least careful construction site, and a
construction site that nobody reads through is untested by definition. The
mirror was introduced as an optimization for a path that always went through the
setter; the bug was waiting for the first reader that did not.

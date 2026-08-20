# P5tie needs a real container-tie protocol (`Stash.BIND-KEY`)

## Symptom

`P5tie`'s test suite (un-triaged `test_die` row in
[todo/tickets/dist-test-suite-failures-batch.md](../tickets/dist-test-suite-failures-batch.md))
dies in all three test files with the same root cause, confirmed against a
clean raku baseline (`raku -I lib t/<file>` passes all subtests for all
three files):

```
No such method 'BIND-KEY' for invocant of type 'Stash'
  in sub tie at lib/P5tie.rakumod line 38    # scalar.rakutest
  in sub tie at lib/P5tie.rakumod line 179   # array.rakutest
  in sub tie at lib/P5tie.rakumod line 311   # hash.rakutest
```

(A second, unrelated parse-time bug that used to make `array.rakutest` die
before any test ran — `X::Syntax::NoSelf` from a bare `die` inside a plain
`sub` nested in a class body — was root-caused and fixed separately; see
`news/2026-08/p5tie-array-rakutest-noself-parse-bug.md`. All three files now
fail at the same `BIND-KEY` gap below, confirming they always shared this one
root cause.)

`P5tie` implements Perl 5's `tie()` by binding a variable's storage to a
user-supplied class instance (`TIESCALAR`/`TIEARRAY`/`TIEHASH` + `FETCH`/
`STORE`/... trap methods) via Raku's low-level container-binding protocol:

```raku
CALLER::CALLER::.BIND-KEY($name, Proxy.new(FETCH => ..., STORE => ...));
```

`CALLER::CALLER::` is a `PseudoStash` — a reflection object over a lexical
pad — and `.BIND-KEY` on it rebinds the named lexical's *container* itself
(not just its value) to the given object, exactly like `%h<key> := $x` does
for a Hash slot, but at the lexical-pad level. mutsu already implements
`Hash`/`Array`-element `BIND-KEY` (`vm_call_method_mut_ops.rs`,
`vm_var_assign_index_named.rs`; used for `%h<key> := $val`), but has no
`BIND-KEY` on the `Stash`/`PseudoStash` reflection value returned by `.WHO`
or `CALLER::...::`.

## Why this needs a design pass

Implementing `tie()` properly means implementing the real container-binding
primitive on `Stash`/`PseudoStash` generally — not just enough to make this
one dist's trap methods fire. This is genuine MOP/container-model work, not
a quick patch. Per `CLAUDE.md`'s BATTERIES.md rung-3 ban, `tie` semantics
should be real interpreter machinery (rung 2), not a native P5tie-specific
stopgap.

## Part A priority triage (2026-08-20)

Before investing in this, the batch-sweep corpus (a fresh ~400-dist random
sample of the fez ecosystem, re-fetched via `scripts/dist-compat-sweep.py`'s
`fetch_tarball`/index logic since the original sweep's `~/.cache` was gone;
see "Corpus method" below) was grepped for `Stash`/`PseudoStash`-level
`BIND-KEY` usage (i.e. `.WHO.BIND-KEY`, `CALLER::...::.BIND-KEY`,
`OUR::.BIND-KEY`, not the already-supported `%hash.BIND-KEY`/element form).

**Result: not single-dist.** Besides `P5tie` itself, the `annotations` dist
(`ra-annotations`) calls the same primitive directly:

```raku
# annotations/lib/annotations.rakumod
my package EXPORT {
    package MANDATORY {
        OUR.WHO.BIND-KEY: .key, .value
            for annotations::core::;
        ...
```

`OUR.WHO` is a package-level `Stash`, and `.BIND-KEY` here installs each
`annotations::core::` symbol into the importing package's stash at `use`
time — a different call site than P5tie's `CALLER::CALLER::.BIND-KEY`, but
the same missing primitive (`Stash.BIND-KEY`, not `Hash.BIND-KEY`).

Every *other* `BIND-KEY` hit in the corpus (`P5pack`, `Memoize`,
`WWW::GCloud::Utils`, `LibXML::Class`, `FINALIZER`, `Data::Record`,
`App::Lorea`, `PDF`) turned out to be the **already-supported** `Hash`
element form (`%some_hash.BIND-KEY($key, $val)`), not the `Stash` form — so
they are not affected by this gap.

The `Tie::StdArray`/`Tie::StdHash`/`Tie::Array`/`Tie::Hash` dists (found via
a metadata grep for "tie") are passive `TIEARRAY`/`TIEHASH`-trap-method
provider classes meant to be used *with* a real `tie()` — they do not call
`tie()`/`BIND-KEY` themselves, so they add no new evidence either way; they
would become usable once this primitive (or P5tie's `tie()` built on it)
exists.

**Verdict: real but niche (2/~400 sampled dists, ~0.5%).** Worth recording
precisely and revisiting in a future container/MOP-focused session (ideally
alongside other `Stash`/reflection gaps found by the same sweep), but not
large/urgent enough to justify starting the design work in this session
given the size of the primitive (see "Why this needs a design pass" above).
Deferred; not attempted this session.

### Corpus method (for reproducing this triage)

```
mkdir -p ~/.zef/store/fez
curl -s https://360.zef.pm/ -o ~/.zef/store/fez/fez.json   # the fez index itself
# then sample N dist names from the index (sorted(name) + random.sample(seed)),
# fetch each tarball from https://360.zef.pm/<path>, extract only
# *.rakumod/*.pm6/*.pm/*.raku source files, and grep.
```
The original sweep's `~/.cache/mutsu-dist-sweep/` cache referenced in earlier
tickets was gone (fresh worktree/environment), so this session re-fetched a
fresh ~400-dist sample (not the literal same 60-dist sample from the
`dist-test-suite-failures-batch` run, since the live fez index has since
changed and the same seed no longer reproduces the same sample) — broader
coverage than the original batch, at the cost of not being a literal replay.

## Next steps (design, not started)

Find/design whatever the real container-binding primitive should be (a
`ContainerRef`-level operation, given mutsu already has a `ContainerRef` cell
abstraction for aliasing — see ADR-0013 §7) and implement `Stash.BIND-KEY`
(and any sibling ops P5tie's `tie`/`untie`/`tied` need) in terms of it. The
natural implementation surface is wherever mutsu currently resolves `.WHO`
and the `CALLER::`/`OUR::`/pseudo-stash package syntax into a `Stash`-like
value (see `src/runtime/accessors_stash.rs`) plus the existing
`Hash`/`Array` element `BIND-KEY` dispatch (`vm_call_method_mut_ops.rs`,
`vm_var_assign_index_named.rs`) as a reference for how the write-through
semantics should look, generalized to a lexical-pad/package-stash slot
instead of a hash/array slot.

## Repro

```
cd <extracted P5tie dist>
raku -I lib t/scalar.rakutest   # passes, 21 subtests via TAP
target/debug/mutsu -I lib t/scalar.rakutest   # dies: No such method 'BIND-KEY'
target/debug/mutsu -I lib t/array.rakutest    # dies: No such method 'BIND-KEY' (parse bug fixed, same gap now)
target/debug/mutsu -I lib t/hash.rakutest     # dies: No such method 'BIND-KEY'
```

Dist tarball: fetch fresh via
`curl -s https://360.zef.pm/P/5T/P5TIE/a009971a160803378f013d5850c0621efee4510c.tar.gz`
(the path recorded in the fez index as of 2026-08-20; re-run
`scripts/dist-compat-sweep.py` or look up the current path if it has moved —
not vendored into this repo). `annotations`' path as of the same date:
`https://360.zef.pm/A/NN/ANNOTATIONS/<hash>.tar.gz` (look up via the fez
index, e.g. `ra-annotations` in the `name` field).

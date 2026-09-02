# `XML` is bundled — the upstream suite reaches 15/15

The XML parse+generate battery slot is filled. `XML` v0.3.6
(`auth<zef:raku-community-modules>`, Artistic-2.0) is vendored verbatim into
`modules/XML/`, so this works against the shipped binary with no `-I` and no
`mzef install`:

```raku
use XML;
my $doc = from-xml('<catalog id="c1"><book lang="en">Raku</book></catalog>');
say $doc.root.elements(:TAG<book>)[0]<lang>;                      # en
say ~make-xml('rss', :version<2.0>, \('channel', \('title', 'mutsu')));
# <rss version="2.0"><channel><title>mutsu</title></channel></rss>
```

It won the field over `LibXML` (a `libxml2` NativeCall binding) on ecosystem
standing — 45 dependents against 7, the highest of any battery survey so far —
zero runtime dependencies, and no hard system-library dependency. The survey,
the metrics table and the `libxml2` maintenance note are in
[docs/batteries/xml.md](../../docs/batteries/xml.md).

## Every step was a general interpreter fix

The dist went from **1/15 to 15/15** without a single XML-specific
accommodation, which is what BATTERIES.md §1's rung-2 policy asks for:

| Point in time | mutsu |
| --- | --- |
| Original survey (2026-08-22) | 1/15 |
| After the two originally-filed blockers | 2/15 |
| After the group-backreference fix (2026-08-26) | 5/15 |
| After ADR-0061's lexical `$self` (2026-08-27) | 9/15 |
| After the junction-slurpy / `IO::Path(Str)` coercion fixes (2026-08-31) | 13/15 |
| After the Capture-slip and list-stringification fixes (2026-09-02) | **15/15** |

The last two came out of re-measuring the suite for this bundling:

- [A Capture inside a slipped container is one argument](slip-array-element-capture-not-respread.md)
  — `make-xml`'s `craft($name, |@contents, |%attribs)` relay lost the nesting
  before `craft-new`'s `$what ~~ Capture` arm could recurse.
- [Stringifying a list calls each element's own `Str`](list-str-calls-element-str.md)
  — `t/namespaces.rakutest` compares `@items[3].contents`, a list of
  `XML::Text` nodes; mutsu compared `XML::Text()`.

A third was found by smoke-testing the bundled copy rather than by the suite:
[a nested Capture literal was flattened while being built](nested-capture-literal-flattened.md),
so `make-xml('rss', \('channel', \('title', 'x')))` collapsed the subtree into
text on the parent. The upstream suite never covered a two-level literal, which
is a reminder that the release gate is a floor, not a ceiling — `t/xml-battery.t`
exercises the shapes a user actually writes.

## Wiring

No code was needed: `resolve_bundled_lib_paths()` registers every
`modules/<Dist>/lib` that exists, so creating the directory is the whole
registration step. `batteries.lock` pins the upstream tag
(`0349d282e257be61075f55abfde4c42a01bc8f10`) for the release-time gate, and all
15 upstream files are on `batteries-whitelist.txt`, so a regression fails the
release rather than silently degrading the bundle. `t/xml-battery.t` is the
in-repo smoke test (parse, round-trip, build, mutate), so a *resolution*
regression fails `make test` too.

One unrelated file joined the whitelist in the same regeneration —
`Log::Async`'s `07-done.rakutest`, which now passes (verified stable across two
independent gate runs). It stringifies accumulated log output through a mixed-in
`$*OUT.say`, so the list-stringification fix above is the plausible cause.

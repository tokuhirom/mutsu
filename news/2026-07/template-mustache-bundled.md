# `Template::Mustache` is bundled — and one hyper bug was the whole reason it did not work

The template slot is filled. `Template::Mustache` v1.2.6 is vendored verbatim at
`modules/Template-Mustache/` and resolves with **zero config**:

```raku
use Template::Mustache;
say Template::Mustache.render('Hello {{name}}!', { name => 'World' });   # Hello World!
```

## The bug: a hyper method call swallowed a `Slip`

When the slot was surveyed, `Template::Mustache` passed **1 of its 13** upstream
test files under mutsu — while passing under raku. The entire gap was one
interpreter bug.

A hyper is built on `deepmap`, so a method that returns a `Slip` contributes its
*elements* to the result, exactly as it would from `map`. mutsu kept the `Slip`
as a single element:

```raku
class C {
    has $.n;
    method made() { slip('a' ~ $!n, { k => $!n }) }
}
my @objs = C.new(n => 1), C.new(n => 2);
say @objs>>.made.raku;
# raku:  ["a1", {:k(1)}, "a2", {:k(2)}]
# mutsu: [slip("a1", {:k(1)}), slip("a2", {:k(2)})]
```

`Template::Mustache`'s `hunk` action does exactly this (`make @x.Slip`), and its
`TOP` action then reads `$<hunk>».made.flat`. With the `Slip` left nested,
`.flat` descended into it and **decomposed the `Hash` inside into its Pairs**, so
every parse-tree node lost its `type` key and rendering died with
`Impossible format type:`. That is why the first visible symptom —
`Use of Nil in string context` — was so misleading: it is a *warning*, fatal in
neither implementation, and pointed nowhere near the cause.

The fix flattens a `Slip` result at the three points where a hyper collects
per-element results (`src/vm/vm_hyper_method_ops.rs`). Pin:
`t/hyper-method-slip-result.t`, every case checked against raku.

`Template::Mustache` went **1/13 → 11/13**, including the whole official mustache
spec suite (`91-specs`, 10/10). The remaining two files are tracked in
`todo/tickets/mustache-remaining-two-files.md`; per the gate's per-file baseline
philosophy the 11 are pinned now.

## The survey behind the choice

Ten candidates were enumerated from the local Zef indices (~2500 dists) and each
one's own upstream suite was run under **both raku and mutsu**. The full table,
with licenses, release dates, dependency counts and reverse-dependency counts,
is [docs/batteries/templates.md](../../docs/batteries/templates.md).

Two findings shaped it:

- **The whole field was healthy under raku and broken under mutsu.** The battery
  decision was therefore not "which engine" but "which mutsu bugs" — see
  `todo/deep/template-engines-blocked-on-mutsu.md`, which carries the other
  engines' blockers, three of them now reduced to minimal repros.
- **The existing readiness note was stale.** `PLAN.md` had carried
  "Template::Mustache, 91/92 specs" as prose; the real number was 1/13.

Because that will happen again, the *procedure* is now written down as
[docs/batteries/selection-method.md](../../docs/batteries/selection-method.md)
and referenced from BATTERIES.md §2: enumerate from `~/.zef/store/{rea,fez}`,
collect license / deps / release date / **dependents**, and always measure the
`raku` baseline first — without it, `Template::Mojo`'s abandoned 2017 release
would have been filed as a mutsu bug instead of its current one, which really is.

`Template::Mustache` won on every axis the criteria rank: Artistic-2.0, zero
runtime dependencies, the most-depended-on engine in the ecosystem (11
dependents, incl. `Bailador`, `Documentable`, `Pod::To::HTML`), maintained under
`raku-community-modules`, and logic-free — the safer default for a blog, where
the program supplies the logic.

The gate grew from 80 to **91** files, all passing.

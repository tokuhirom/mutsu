# Three general interpreter fixes found by re-measuring the Config::TOML battery

[#7539](https://github.com/tokuhirom/mutsu/issues/7539) tracks bundling
`Config::TOML` v0.1.3 and its dependency `Crane` v0.1.2 as a battery, parked
behind core interpreter blockers. Both upstream suites were re-fetched and
re-run against a fresh build; the pass counts had **regressed** since the
ticket's last measurement, and chasing that regression turned up three
unrelated general bugs.

| Suite | raku | mutsu (ticket, 2026-09-07) | mutsu before this pass | mutsu after |
| --- | --- | --- | --- | --- |
| `Config::TOML` v0.1.3 | 19/19 files | 11/19 | 10/19 | **14/19** |
| `Crane` v0.1.2 | 15/15 files | 4/15 | 2/15 | **4/15** |

Counts are from a release build (`target/release/mutsu`) with a 120s per-file
budget; a debug build is one file short, `special-cases/03-txn` timing out
rather than failing.

None of the three fixes is TOML-specific; each is a rule mutsu was getting
wrong for any program of the same shape.

## 1. A package nested under a setting-provided name was invisible when reached transitively

Rakudo merges a `use`d compunit's declarations stash by stash. A
`class X::Crane::GetRootContainerKey` therefore lands in the setting's own `X`
stash — one process-global object every compunit shares — rather than in the
declaring compunit's `GLOBALish`, which is the only thing the importing
compunit merges. So `use Crane;` alone makes every `X::Crane::*` exception
nameable, and Crane's own test files name them directly.

mutsu's #7797 qualified-name gate reconstructs rakudo's lexical rule on top of
its flat package stores, but had no notion of the setting's own stashes: it
required the running compunit to have `use`d something under the name's own
top-level segment. A script that `use`d only `Crane` was refused
`X::Crane::GetRootContainerKey` outright, aborting four files mid-run.

Measured against rakudo (v2026.07), which draws the line at exactly this
spelling:

| declared in a transitively-`use`d module | rakudo |
| --- | --- |
| `class Zzz::Foo::Alpha` | "Could not find symbol" |
| `unit module Baz; our sub greet` | "Could not find symbol" |
| `class X::Zork::Alpha` | resolves |
| `class IO::Zork` / `class Pod::Zork` | resolves |

The gate now admits a name whose top-level segment is one the setting itself
provides: the pure-namespace `CORE::` packages (`X`, `CX`, `EXPORTHOW`,
`Exceptions`, `Metamodel`, `PROCESS`, `Pod`, `RakuAST`, `Rakudo`, plus
`CompUnit`/`Encoding`/`Systemic`), and every setting *type* that also acts as a
namespace, which the existing builtin-type list already answers. A fresh
top-level namespace is still gated exactly as before.

Pinned by `t/modules/setting-nested-package-visibility.t`.

## 2. A from-the-end subscript in container context read instead of binding

Every rvalue index path resolves a `WhateverCode` subscript against the
subscripted array's length before using it. The two container-producing paths —
`:=` bind and an `is rw` routine's `return-rw <subscript>` tail — did not: the
index stayed an unconvertible `WhateverCode`, `index_to_usize` declined it, and
the autovivifying op fell through to a plain *read*.

```raku
sub g(\c) is rw { return-rw c[*-1] }
my @a = 1, 2, 3;
g(@a) = 9;      # raku: [1, 2, 9]   mutsu: "Cannot modify an immutable Int (3)"
```

`c[*-0]` on an empty array — the append idiom `Crane::In` is built on, and
what `Config::TOML` performs for every `[[table]]` entry — silently wrote
nowhere, so an array-of-tables document came back with an empty array.

Both autovivifying index ops now resolve a `WhateverCode` index against the
array first. Deliberately narrow: only a `WhateverCode`, never a bare
`Whatever` (which expands to a full index list and is a slice with its own bind
semantics), and only against an `Array`.

Pinned by `t/vm/binding/rw-return-from-the-end-index.t`.

## 3. A `:=` rebind of a subscript adopted an ancestor frame's container

The compiler tags a `$x := <subscript>` bind source with a synthetic per-site
name `__mutsu_bind_index_ref_N`. The tag denotes nothing of its own — the value
under it is the oracle, a rule the store path already states for the
immutability decision — and `N` is a constant-pool index, so two unrelated
routines routinely mint the same name.

The bind's container-promotion did not know that. It asked whether any ancestor
call frame's saved env held the source name, found a *caller's* identically
numbered temp through the chained call env, and adopted that stale cell as the
bind's container. A rebind then kept naming the container the key was looked up
in instead of the deferred entry for a missing key:

```raku
sub walk($container, @path) {
    return unless @path;
    my $root := $container;
    $root := $root{@path[0]};
    say @path[0], ' -> ', $root.WHAT.^name;
    walk($root, @path[1..*]);
}
my %t = a => { b => 1 };
walk(%t, ['a', 'zz', 'yy']);
# raku:  a -> Hash   zz -> Any    yy -> Any
# mutsu: a -> Hash   zz -> Hash   yy -> Hash
```

A recursive path-walking routine therefore descended into *itself* one step per
path element. `Config::TOML`'s `pwd` answered `["a", 0, "b", 0, "c"]` where
rakudo answers `["a", 0, "b", "c"]`, and the parser built a nested Array where
the TOML said Hash.

The trigger is worth naming, because it makes the bug look signature-sensitive:
an `@`/`%` **parameter** disqualifies the callee from the slot-only light call
paths, and only the env-chaining paths can see a caller's temp. The identical
routine with a signature of plain scalars was always correct.

The synthetic index tag is now excluded from the "does an ancestor frame own
this source variable?" test, the same way it is already excluded from the
named-source immutability rule.

Pinned by `t/vm/binding/bind-index-temp-not-an-outer-frame-source.t`.

## What is left on the ticket

`Config::TOML` is at 14/19 and `Crane` at 4/15, so the vendoring steps stay
parked. The residue splits into:

- `special-cases/03-txn` now passes, but takes **58s on a release build where
  `raku` takes 4.9s** — a 12x gap on one 350-line TOML document, and a
  performance finding rather than a wrong answer. Filed separately as a
  `todo:perf` issue.
- `grammar-actions/01`, `02`, `04` and the two dumper files are the residues
  already recorded on the ticket (string-equivalence, Rat/FatRat precision, and
  the untouched TOML dumper).
- `Crane`'s remaining files are its own positional/error-handling and ordering
  gaps plus the object-hash blocker, unchanged by this pass.

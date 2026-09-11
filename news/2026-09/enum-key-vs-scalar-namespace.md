# Enum keys get their own namespace instead of sharing the scalar key space

In Raku an enum's keys live in the package symbol namespace: `our Str enum U «:s<time>»`
declares a *term* `s`, not a `$`-sigiled scalar. The two are different symbols, so `my $s`
and the enum key `s` coexist and neither can see or clobber the other.

mutsu stores a scalar `$s` sigil-stripped, under the `env` key `"s"`, and the enum
registration installed each key under its own bare name — into that same key. The two
namespaces were therefore one namespace, and the collision went both ways
([#7914](https://github.com/tokuhirom/mutsu/issues/7914)):

```raku
# lib/EDefs.rakumod:  unit module EDefs;
#                     our Str enum U is export(:U) « :ms<time> :s<time> :px<length> »;
# lib/EUse.rakumod:   unit class EUse; use EDefs :U; method pick($x) { U::s }
use EUse;
my $s = 'hello';
say "before: [$s]";        # raku & mutsu: before: [hello]
my $r = EUse.pick(1);
say "after: [$s] r=$r";    # raku: after: [hello] r=time
                           # mutsu: "Use of uninitialized value ..."; after: [] r=time
```

and, in the other direction, a bare `s` term read back whatever the same-named *scalar*
last held:

```raku
our Str enum U « :s<time> »;
my $s = 'x'; $s = 'y';
say s;                     # raku: s   (the enum value)   mutsu: y
```

The first shape is how it was found: `CSS::Grammar::Defs` declares
`« … :ms<time> :s<time> … »`, so a reduction script calling into `CSS::Grammar` while
holding a lexical `$s` silently parsed the wrong input for three iterations before
anyone noticed.

## What was actually clobbering the lexical

Two separate sites wrote the enum value under the plain key. The registration loop
(`registration_sub.rs`) did `env.insert(key, enum_val)`, and — the one that produced the
reported `Any` — `import_module`'s exported-variable loop copied `EDefs::s` to `s` *and*
pushed `"s"` onto `pending_rw_writeback_sources`. That list is the env→locals coherence
channel: a name on it gets `env[name]` written through to the caller's local slot at the
next frame reconcile. So the import of an enum key made the importing frame pull `env["s"]`
— by then the decl-seed `Any` placeholder for its own `my $s` — over the slot holding
`'hello'`. The first read still saw `'hello'` because the reconcile ran *after* it; every
later read saw `Any`.

## The fix

A namespace, not a per-site patch. `src/runtime/enum_bare_names.rs` states the rule once:
an enum key's bare spelling is stored under a reserved `__mutsu_enum_bare_` key prefix that
no user symbol can spell, and only term/bareword resolution consults it
(`Interpreter::enum_bare_value`). Variable lookup, which reaches `env` under the sigil-less
name, can no longer see an enum key; an assignment to `$s` can no longer overwrite one.

The storage stays in `env` deliberately. Enum-key visibility is *lexical*, and `env` is what
implements that — block scopes, package-block rollback (a bare key introduced inside
`package Foo { … }` is dropped on exit, only `::`-qualified keys survive, and the prefix
contains no `::` precisely so an enum key keeps that behaviour), thread clones and the `our`
store all key off it. A side table would have had to reimplement every one of those.

Everything that legitimately reaches an enum key by its bare name was moved onto the new
probe: bareword resolution, `::('name')` indirect lookup, the `Mod::EXPORT` stash, the
`GLOBAL::`/`MY::` stash listing (which republishes the key under its bare spelling), the
poisoned-alias check, the literal enum-value parameter constraint that makes
`multi infix:<->(e1, e2)` dispatch (`roast/S03-operators/custom.t`), the `unit` compunit's
package-scope rollback (#7787 — an enum value must not leak to the importer), the
module-scope lexical snapshot, and the two in-place writes to an enum key's own binding:
`Apple does R` and the `X::Assignment::RO` that `Alpha = 3` must raise. `OpCode::DoesVar`
grew a `bareword` flag for the first of those, because `$Apple does R` and `Apple does R`
used to be indistinguishable at the writeback — they shared the key — and now must be
told apart.

The probe itself is gated on a latched "any enum key has been installed" flag, so a program
that declares no enum pays an atomic load rather than a `format!` per bareword read.

`import_module` also stops recording an enum key as a pending caller-slot writeback: an
enum key names no local slot, and that record was the half of the bug that turned `$s`
into `Any`.

## Still shared: sigil-less constants and type names

The same key space is still shared by sigil-less `constant`s and type names with
same-named scalars. This change does not move those — but it does state the rule they
would follow, in one place, rather than leaving it implicit per declarator.

Pinned by `t/types/enum-subset/enum-key-vs-same-named-scalar.t` (both directions, inline
and through a two-module import chain), with the fixtures
`t/lib/EnumKeyScalarDefs.rakumod` and `t/lib/EnumKeyScalarUser.rakumod`.

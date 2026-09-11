# Making String::Utils run: the nqp text surface, `UNIT::`, and predictive Seqs

`String::Utils` is one of the most-depended-on distributions in the zef ecosystem and its
`ecosystem/` record read `blocked_load`: mutsu could not even `use` it. It is written almost
entirely in `nqp::` ops, exports through a hand-written `sub EXPORT`, and drives its `ngram`
sequence from a `PredictiveIterator` — three surfaces that turned out to be missing or wrong in
mutsu, none of them specific to this distribution.

Following the [`ecosystem-dist-fix`](../../.agents/skills/ecosystem-dist-fix/SKILL.md) loop, each
failure was reduced to a few lines that live in this repo, measured against rakudo, and then either
fixed or filed. The distribution went from **0 of 3 test files running** to **2 of 3 green and the
third at 116 of 124 assertions**.

## The `nqp::` text surface

`runtime/nqp_ops_text.rs` and `runtime/nqp_ops_str.rs` are new links in the pure-value dispatch
chain, adding the character-class, Unicode-property, codepoint-array and string/hash primitives:
`iscclass` / `findcclass` / `findnotcclass`, `unipropcode` / `getuniprop_int` / `getuniprop_str`,
`strtocodes` / `strfromcodes`, `substr` / `concat` / `index` / `rindex` / `eqat` / `flip` / `x` /
`uc` / `lc`, `bindkey` / `deletekey` / `existskey` / `clone`, `mod_i`, `hllbool`, `box_s`,
`null_s` / `isnull*`, and the `list_*` / `push_*` / `atpos_s` / `bindpos_s` family. The
`CCLASS_*` and `NORMALIZE_*` names joined the compiler's `nqp::const::` table, `nqp::create`
learned to allocate a native array's storage rather than hand back an empty type object, and
`getattr` / the typed `bindattr_i` / `_n` / `_s` variants were added.

**Every number in there is MoarVM's, read off rakudo rather than invented.** nqp code branches on
the raw integers — `String::Utils`'s `nomark` compares a `getuniprop_int` result against a bare `6`
and means "Mn" by it — so a self-consistent numbering of our own would have run such code silently
wrong instead of loudly unsupported. The General_Category value codes were derived by walking
codepoints 0..0x2FFFF under rakudo; the `CCLASS_*` membership rules by probing `nqp::iscclass` per
class. That measurement is worth keeping: `CCLASS_ALPHABETIC` is `gc L*`, **not** the Unicode
`Alphabetic` property (rakudo answers 0 for U+2160 ROMAN NUMERAL ONE), `CCLASS_UPPERCASE` is `Lu`
rather than `Uppercase`, `CCLASS_PRINTING` is simply "not `Cc`", and `CCLASS_HEXADECIMAL` is ASCII
only. `t/vm/nqp-text-unicode-ops.t` pins all of it and passes under **both** interpreters, so the
oracle travels with the test.

One semantic detail cost a wrong answer before it was measured: `nqp::strtocodes` **replaces** its
target array rather than appending to it. `root` allocates one buffer and re-fills it once per
word, so an appending version compared every word after the first against the wrong offsets and
answered `"abc"` where `root <abcd abce abde>` should give `"ab"`.

## `UNIT::` is a lexical pad, and it now lists the unit's own routines

`UNIT::` resolved as a *package* called "UNIT" and answered an empty stash. It is a lexical
pseudo-package — the compilation unit's outermost pad — so it now shares the `MY::` path.

That alone was not enough. The lexical pseudo-stash enumerated only routines registered under a
bare-name package, i.e. `our`/package subs; a `my sub` (and a plain file-scope `sub`, which is also
lexical) is secluded out of the shared registry into the per-compunit table, and nothing listed
those. The effect was that a module's own subs appeared in `UNIT::` at its file scope — where they
are locals of the running frame — but **not** from inside a routine of that module, which is
precisely where a hand-written `sub EXPORT` reads them. The standard
`UNIT::.grep: { .key.starts-with('&') }` idiom therefore exported nothing at all.

`Interpreter::visible_unit_private_routines` is the enumerating twin of the existing by-name
`unit_private_routine`, with the same visibility rule. Asking the env instead would not have done:
registration deliberately *removes* the `&name` binding when it seclusion-moves a routine, and only
a call through the name puts one back — which made the stash **order-dependent**, listing a routine
on the second read and not the first. `t/modules/import-export/unit-stash-lexical-routines.t` pins
the determinism as well as the contents.

Scoping the registry walk to the pad's own compunit turned out to be part of the fix rather than a
refinement. The registry is shared across units, so a module's `sub EXPORT` (which runs with the
module as `current_unit`) saw the *importing script's* file-scope subs in its `UNIT::`, exported
them straight back, and the script's next `sub` declaration was rejected as a redeclaration of
itself. A routine declared in another file now belongs in a lexical stash only if this scope can
actually see it by name.

## A `PredictiveIterator` can drive a Seq

`Seq.new($iterator)` where the iterator does `PredictiveIterator` built an **empty** Seq and kept
the iterator only in an out-of-band table, so that `.tail` and `.Numeric` could take the
`count-only` shortcut without draining. The cost was that the Seq had no contents: `.list` was `()`
where the identical class doing plain `Iterator` produced its elements. It is now an ordinary
deferred Seq that *also* carries the shortcut, and the guard that used to leave the placeholder
untouched applies only while the body really is one.

## What was filed instead of fixed

Four findings were too large for this change and are recorded as issues, each with its reduction
and both interpreters' output:

- [#7880](https://github.com/tokuhirom/mutsu/issues/7880) — `my $x := BEGIN { ... }` binds `Nil`
  and never runs the body, while `=` works. The compiled bytecode looks right and the opcode
  provably never executes, so something between compiling and running the mainline substitutes a
  different instruction sequence for the bind form.
- [#7881](https://github.com/tokuhirom/mutsu/issues/7881) — a routine exported by a *runtime*
  `sub EXPORT` is invisible to the parser's static export scan, so `imported-listop <words>` cannot
  be parsed. This is the one blocker that keeps `t/01-basic.rakutest` from running at all; with its
  eight affected lines rewritten to the paren form the file reaches 116/124.
- [#7882](https://github.com/tokuhirom/mutsu/issues/7882) — `my @a is List` declares an `Array`.
- [#7883](https://github.com/tokuhirom/mutsu/issues/7883) — the regex cursor protocol
  (`Match.^lookup("!cursor_init")`) is not available to user code, which is how `replace` is
  written.

The distribution's record is re-measured and its remaining red is now explained by issue number
rather than by silence — which is the outcome the skill asks for when a distribution does not go
all the way green in one pass.

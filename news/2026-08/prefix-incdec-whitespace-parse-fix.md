# Prefix `++`/`--` now accept whitespace before their operand

`Archive::SimpleZip` (REA `Archive::SimpleZip` v0.8.0, a zip-write battery
candidate surveyed in `docs/batteries/compression.md`) failed to parse under
mutsu:

```
Failed to parse module 'Archive::SimpleZip': Confused. parse error at line 20,
column 1: ... near: "++ $count} ;\n\n        return $count;\n    }\n\n
multi method mkdir(Str:D() $name" ...
```

The offending line was

```raku
$s.map: { samewith($^a, |c) ; ++ $count} ;
```

## Root cause

A hand-written minimal repro (a `|c` slurpy method, a `.map:` block using
`$^a` + `samewith` + a trailing `; ++ $var`) had previously failed to
reproduce the failure in isolated testing, leading the original finding to
suspect an interaction with an earlier declaration in the file (a `unit
module`/dynamic-`EXPORT` effect, by analogy with
`compress-bzip2-ternary-parse-after-dynamic-export.md`).

Bisecting the real, unmodified `lib/Archive/SimpleZip.rakumod` (fetched from
`pmqs/Archive-SimpleZip` on GitHub, tag `v0.8.0`) with `--dump-ast` quickly
narrowed the failure down past `.map:`, `samewith`, `|c`, and `$^a` — none of
which were load-bearing. The actual trigger was far simpler than any of that
machinery: **a single space between the prefix `++`/`--` operator and its
operand**, e.g. `++ $count` (the real file has one; a hand-written repro
naturally reaches for the more idiomatic `++$count` with no space, which is
why the earlier minimal-repro attempt missed it entirely). Minimal repro:

```raku
my $count = 0;
++ $count;
```

Raku permits (though does not require) whitespace between a prefix
increment/decrement operator and its operand — the same as unary `+`, `-`,
`!`, `?`, `~`, `+^`, `?^`, and `~^`, all of which mutsu's parser already
handled via `PrefixUnaryOp::consumes_ws()`. Prefix `++`/`--`, however, are
parsed by a separate code path — `autoincrement_expr()` in
`src/parser/expr/precedence_meta_ops/arith.rs`, since they bind tighter than
`**` but looser than postfix, a precedence band the generic
`consumes_ws()`-driven prefix parser doesn't cover — and that path never
called `ws()` on the input after consuming the `++`/`--` token, so any
following whitespace was left for the operand parser, which rejected it.

## Fix

`autoincrement_expr()` now consumes whitespace immediately after matching
`++`/`--`, before checking for the hyper-prefix marker (`++<<`, `--«`, ...) or
recursing into the operand. This is a strict superset of the previous
behavior — every existing no-whitespace call site (`++$i`, `--$i`, `++$i **
2`, chained `++ ++ $y` non-associativity, hyper-prefix forms) is unaffected,
since `ws()` on an already-tight input is a no-op.

Once this was fixed, the module made it all the way through parsing. It only
stops on the (expected, out-of-scope for this fix) `Compress::Zlib` being
unavailable — `Archive::SimpleZip`'s zlib dependency isn't vendored in mutsu.
A second symptom seen while bisecting via `--dump-ast` — the true-branch of
`$hdr.compression-method = $empty ?? Zip-CM-Store !! $method` being rejected
as "gobbled" the `!!` — turned out to be a `--dump-ast`-only artifact (that
mode skips the module-scan preseed pass that would otherwise register
`Zip-CM-Store` as a known enum value imported from
`Archive::SimpleZip::Headers`); it does not occur during normal `use`/module
loading, and does not affect the underlying real bug.

## Files changed

- `src/parser/expr/precedence_meta_ops/arith.rs` — `autoincrement_expr()` now
  calls `ws()` after matching the `++`/`--` token.
- `t/prefix-incdec-whitespace.t` — new regression test covering the
  whitespace-before-operand case, precedence with `**`/`*`, the `.map:` block
  idiom from the real repro, and that chained `++ ++ $y` (with whitespace) is
  still correctly rejected as non-associative.

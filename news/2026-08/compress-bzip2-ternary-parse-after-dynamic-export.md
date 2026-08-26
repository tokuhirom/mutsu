# An imported `constant` is a complete term in a ternary branch

`use Compress::Bzip2;` failed to compile under mutsu:

```
Failed to parse module 'Compress::Bzip2': Your !! was gobbled by the
expression in the middle; please parenthesize
```

The offending line is a NativeCall argument in `Compress/Bzip2.pm6`:

```raku
$!bzret = BZ2_bzCompress($!stream, ($!stream.avail-in) ?? BZ_RUN !! BZ_FLUSH);
```

where `BZ_RUN`/`BZ_FLUSH` are constants re-exported by
`Compress::Bzip2::Raw` through a dynamic `sub EXPORT` built from `MY::`
introspection.

## The ticket's hypothesis was close but not right

The ticket suspected the dynamic `sub EXPORT` machinery — that names exported
that way never became parse-time terms in the importing file. Both halves of
that turned out to be wrong, and the real trigger was much smaller. Bisecting
the real dist (fetched from the zef ecosystem index) reduced it to four lines
against a hand-written two-constant module with the same dynamic-`EXPORT`
shape:

```raku
use DynExportedConst;      # my constant PEG_RUN / PEG_FLUSH + sub EXPORT { %all-symbols }
my $x = 1;
my $b = $x ?? PEG_RUN !! PEG_FLUSH;
```

A `gdb` breakpoint on `register_imported_value_term` proved the scan *did*
harvest every constant, `BZ_RUN` included. (The reason several hand-reduced
repros in the ticket "did not reproduce" is that most of them put the ternary in
a listop argument, e.g. `say $x ?? A !! B`, where the parser runs in
`ExprMode::ListopArg` and the guard below is skipped entirely.)

## Root cause

The `?? then !!` bareword guard exists because a bare identifier in then-position
is usually a listop head that swallowed the `!!`
(`X::Syntax::ConditionalOperator::SecondPartGobbled`). Both copies of it —
`src/parser/expr/precedence/ternary.rs` and
`src/parser/expr/precedence/list_infix_top.rs` — asked five "is this a complete
nullary term?" registries, including `is_user_declared_value_term` (a *locally*
declared sigilless `my \foo` / `constant foo`) and `is_user_declared_enum_value`.

A constant harvested from a `use`d module lands in a **different** registry:
`imported_value_terms`, queried by `is_imported_value_term`. Neither ternary
guard asked it. The `when`-matcher guard in `stmt/control/given_when.rs` already
did, which is why an imported constant worked as a `when` matcher but not in a
ternary branch. A statically `is export`-ed constant appeared to work only by
accident: it is additionally registered as an imported *function* name, so the
bareword parses as a `Call` and never reaches the `Expr::BareWord` guard at all.

Both guards now ask `is_imported_value_term` as well.

`use Compress::Bzip2;` compiles now and its own test file gets through its first
four assertions before hitting unrelated NativeCall trouble; the dist is still
not a battery candidate, but the parse blocker recorded in
`docs/batteries/compression.md` is gone.

Pinned by `t/parser-expression-gaps.t` with the `t/lib/DynExportedConst.rakumod`
fixture, which reproduces the dynamic-`sub EXPORT`-from-`MY::` shape.

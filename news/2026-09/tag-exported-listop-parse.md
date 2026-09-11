# A tag-exported routine is a routine at parse time too

`imported-listop <a b c>` is one of Raku's most ordinary shapes, and it is
only a call if the parser already knows the identifier names a routine. When
it does not, `<` is taken as infix less-than and the quote-word list is a hard
parse error — while the parenthesised `imported-listop(<a b c>)` form keeps
working, which is the tell that the problem is the parser's knowledge rather
than the code.

mutsu answers that question with a per-module scan: every `use`d module is
pre-parsed for the names it exports, and those names land in
`Scope::imported_functions`. [ADR-0087](../../docs/adr/0087-runtime-export-hook-parse-time-approximation.md)
extended the scan to modules that compute their exports through a run-time
`sub EXPORT` hook. This entry closes the remaining hole in the same scan.

## The hole

The scan kept only the subs whose `is export` trait carried the `DEFAULT` or
`MANDATORY` tag. A sub exported under a custom tag — `sub joined(*@w) is
export(:extra)` — was dropped from the set entirely, so:

```raku
use ImportedListopAngle :extra;
say (joined <a b c>);
```

died during the parse:

```
===SORRY!=== Error while compiling -e
Confused. expected statement: expected expression after infix operator or ...
------>use ImportedListopAngle :extra; say (joined <a b c>);
                                       ^
```

against `a-b-c` from `raku`, even though `:extra` is exactly the tag that
imports `joined`.

The filter was reasonable-looking and wrong for a structural reason:
`register_module_exports` is handed the *module name* and nothing else. It
never sees the importer's tag list, so it cannot distinguish "plain `use M`,
where a `:extra` sub is genuinely not imported" from "`use M :extra`, where it
is". Filtering on the tag therefore did not implement import semantics; it
just guessed, and guessed wrong for every tagged import.

## The fix

Both collection sites — the AST walk (`collect_exported_subs`) and the regex
fallback (`extract_exported_names_fallback`) — now collect every `is export`
sub whatever tag it carries.

That is a deliberate superset, and it is sound for the same reason ADR-0087's
approximation is: the set is parse-time knowledge only. Its every consumer is
a parser decision about whether an identifier is a routine, while run-time
name resolution is a separate path that still resolves against the real
import set. So a name the importer's tag list withholds is still not callable
— it fails as an undeclared symbol, exactly as it did before — and the cost of
the superset is a worse diagnostic for such a name, never a change in the
meaning of a program that runs. `tag-export-parse-time-only.t` pins that
directly: in a unit that asks only for `:extra`, the tagged name parses and
runs, and `EVAL 'root("abcd")'` on the module's `DEFAULT` name still throws
`X::Undeclared::Symbols`, the same exception `raku` throws.

Tag-exported *operators* were already unaffected: `sub infix:<dbl> is
export(:ops)` reaches the parser's operator matchers through the separate
user-operator scan, so `use TagOps :ops; 3 dbl 4` worked throughout.

## Pins

Two new files under `t/modules/import-export/`, both of which pass unmodified
under `raku` as well as mutsu:

- `imported-listop-angle-arg.t` — the call shapes, for an untagged export and
  a tagged one: `<a b c>`, `<<a b c>>`, `qw{}`, a postfix on the result, and
  the negative direction (`1 < 2` and `1 < 2 < 3` must stay comparisons). The
  untagged half was the shape [#7939](https://github.com/tokuhirom/mutsu/issues/7939)
  reported and had no regression test of its own; it had been fixed for the
  `sub EXPORT` path by ADR-0087 shortly before that issue was filed, and this
  pins it for the ordinary `is export` path.
- `tag-export-parse-time-only.t` — the soundness half described above.

# `Template6` goes 0/12 → 10/12 on four general interpreter fixes (and Jinja2 finally loads)

The template-engine survey (`todo/deep/template-engines-blocked-on-mutsu.md`)
was re-measured on 2026-09-06 — every row in it had gone stale — and
`Template6` 0.16.0, the runner-up for the template battery slot, was reduced
from its long-standing "unreduced, `Use of Nil in string context`" state. That
warning was, as the deep file's own lesson predicted, a pointer and not the
diagnosis: reducing `Parser.compile` by deletion turned up **four separate,
entirely general interpreter bugs**, none of them about the warning. Fixing them
takes the dist from **0 of 12** upstream test files to **10 of 12**. A fifth bug,
found while re-measuring the *other* rows of the same survey, ships alongside
them: it is what finally makes `Template::Jinja2` loadable (0/23 → 3/23).

The survey itself was the other half of the work. Four of its eight rows had
moved since the last measurement without anyone noticing, so
`todo/deep/template-engines-blocked-on-mutsu.md` and
`docs/batteries/templates.md` now carry fresh counts and a measurement log.

## 1. A `split(/regex/, :v)` separator Match had no named captures

`Template6::Parser.compile` splits a template with

```raku
$template.split(/ $<prefix-linebreak>=(\n?) '[%' $<comment-signature>=('#'?)
                  \s* $<tokens>=(.*?) \s* '%]' /, :v)
```

and then reads `~$segment<tokens>` off each separator `Match`. Every one of
those came back `Nil`: the separator Match was built by
`make_match_object_with_captures` with a hard-coded empty named map, and only
text-flattened positional captures (whose `.from`/`.to` were fabricated `0..len`
spans over the separator text rather than offsets into the split subject). The
list-of-splitters form (`.split([/a/, ','], :v)`) dropped even the positional
ones.

Both runtime split paths now obtain the engine's real `RegexCaptures`
(`regex_match_with_captures_from`, the same entry point `.match` uses) and build
the separator Match with `make_match_object_full_visible` — the identical
construction `~~ /…/` performs. `SplitMatch` carries the finished Match instead
of a `Vec<String>` of capture texts, and `separator_value` is now just "the
engine's Match, or the matched text for a string splitter". Pin:
`t/split-regex-separator-captures.t`.

## 2. `.subst(..., :nth(2..*))` aborted the process with a Rust panic

`Parser.compile` ends with
`$script.subst(/ 'my %localdata;' /, '', :nd(2..*)).EVAL`. `subst_nth_indices`
expanded a Range adverb eagerly, so an infinite upper bound became
`(2..=i64::MAX).collect()` and the process died with `capacity overflow` from
`raw_vec`. Range arguments now join `:nth(*)` / `:nth(*-1)` on the *deferred*
path, resolved against the actual match count by `resolve_nth_value_indices` —
which already clamps correctly, and which gained the two Range flavours it was
missing (`1^..3`, `1^..^4`, previously "Cannot convert '2 3' to integer for
:nth"). The literal-string `.subst` branch also learned to consult the deferred
list at all; it used to ignore it and substitute nothing. Pin:
`t/subst-nth-range.t`.

## 3. An attribute default's closure was stamped with the CONSTRUCTING class

`has %!directive-handlers = … default => (-> @, **@values { parse-set(…) }, …)`
could not call `parse-set`, a file-scoped sub in the same
`unit class Template6::Parser` — but only when `Template6::Context::BUILD`
constructed the parser, never when the script constructed it directly, which is
what made it look like a scoping mystery.

An attribute default is lowered to bytecode when the class is declared but
*executed* inside whatever frame calls `.new`. `lexical_closure_package()`
walked the routine stack first and found the constructing caller's method, so it
stamped the caller's class onto every closure the default built; the closure
then ran under the wrong package and `bare_name_packages()` never contained the
declaring one. `eval_attr_default_expr` already sets `constructing_class` — the
one and only producer of that field, and an exact "we are inside an attribute
default of class X" signal — so `lexical_closure_package()` now consults it
first. Pin: `t/attr-default-closure-package.t` (plus two fixture modules under
`t/lib/`, since the bug only appears across a module boundary).

## 4. Assigning to `$_` inside a nested block was discarded on block exit

`Context.get-template-block` caches a compiled template with

```raku
given $template {
    if $_ !~~ Callable { $_ = $.parser.compile($_) }
    %.blocks{$d}{$d.^name} = $_;
}
```

and mutsu cached the raw template *text*, so the second `process` of the same
template died with `No such method 'CALL-ME' for invocant of type 'Str'`.

`OpCode::BlockScope`'s env restore skipped `_` unconditionally, on the grounds
that "the lexical topic is block-scoped". That is true of a block that *binds*
its own topic (a `for`/`given`/`when` body, a pointy `-> $_`) and false of a
plain nested block, whose `$_` simply IS the enclosing topic. The write still
reached the topic's source variable through its container, so `$template` and
`$_` disagreed from the next statement onwards — which is why the symptom was so
strange. The skip is now conditional on the block actually containing a
topic-binding opcode. The same fix repairs `sub f($_ is rw)`, where a write
inside a nested block reached neither the sub's own later reads nor the caller.
Pin: `t/topic-assign-in-nested-block.t`; `t/for-topic-restore.t` still passes.

## 5. A `use`d module's `is export`ed types never reached the parse-time type index

Found while re-measuring the rest of the survey, and shipped in the same change
because it is the same kind of bug. `when SomeType { … }` needs the parser to
know `SomeType` is a type — an undeclared bareword there really does gobble the
block in Raku, and mutsu diagnoses that. The parse-time type index is built by
scanning each `use`d module's source, but a trait on a declarator
(`class Cond is export { }`) makes the parser wrap the declaration in a bare
`Stmt::Block`, and `collect_module_type_names_under` never walked into one. So
**every `is export`ed class, role, grammar and enum in a `use`d module was
invisible** to the importer's parser, and `when Cond { }` failed to compile with
"Function 'Cond' needs parens to avoid gobbling block". Imported enum *values*
had the same hole.

Bare blocks are now walked (with the same package prefix, since a bare block
introduces no package level), and imported type names are registered in the
outermost scope like imported enum values and constants already were, so they
stay visible for the rest of the importing file. This is what kept
`Template::Jinja2` from loading at all: its `Renderer.rakumod` dispatches on the
AST node types it imports, `when If { … } when For { … }`. With it fixed the
dist loads and runs 3 of its 23 files (up from 1). Pin:
`t/when-imported-exported-type.t` plus `t/lib/ExportedTypeIndex.rakumod`.

## Where `Template6` stands

10 of 12 files pass. The two that remain are filed separately, both reduced:

- `todo/tickets/array-arg-mutation-lost-on-the-second-call-through-a-slurpy-relay.md`
  — an `@`-parameter mutation reaches the caller on the first call through a
  slurpy relay and is silently lost on every later one (18-line repro, not the
  JIT). This is `t/02-for.rakutest`.
- `todo/tickets/template6-include-local-data-not-reaching-the-included-stash.md`
  — `[% INCLUDE "x" name = "World" %]` renders `name` instead of `World`. This
  is `t/05-includes.rakutest`.

A third finding, unrelated to `Template6` but hit while writing the pins, is
`todo/tickets/trailing-comma-in-attribute-default-drops-the-declaration.md`.

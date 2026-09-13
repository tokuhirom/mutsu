# A method literal is a Routine, not a Block

`method (...) { ... }` in expression position is a `Method` in raku, and a
`Method` is a `Routine`: `return` returns from it, its parameters carry the
implicit `Any` nominal type every routine signature has, and it reports its own
type. mutsu compiled it down the *block* path instead, so it answered `Block`,
typed its parameters `Mu`, and made `return` inside one die with "Attempt to
return outside of any Routine". The `submethod` spelling did not parse at all,
and neither did `anon submethod`.

This came out of the ecosystem parity cluster
[#7988](https://github.com/tokuhirom/mutsu/issues/7988), where
`MetamodelX::Dataclass` could not load its own module because of a single line:

```raku
my &call-me = anon submethod call-me(Mu $obj: *%args) {
    $obj.new( |%args )
};
```

## The shape of the bug

The same shape the previous runs on that cluster kept finding: **a capability
mutsu already had, restated in a second place with less of it.**

`parse_anon_method_with_params` already parses the whole method-literal
grammar — an explicit invocant with its type and `where` constraint, a
user-named invocant bound as a lexical alias beside `self`, traits, a return
type. The `anon` arm of the term parser did not use it. It had its own copy
that knew only `method` (never `submethod`), and routed every form it did
recognise through the anonymous *sub* parser, which has no notion of an
invocant. So `anon method ($x) { self }` had no receiver, `anon method { }`
was a plain `Sub`, and `anon submethod` reached no branch at all.

Deleting that copy and delegating is the whole parser fix: `anon` means only
"install no symbol", which a routine literal already does, so every `anon`
spelling *is* the ordinary literal. The `method`/`submethod` term arm and the
`my method` / `my submethod` term form now share one declarator-keyword reader
for the same reason — the two spellings differ only in the type the closure
reports.

## Recording the declarator instead of guessing at it

`Expr::AnonSubParams` carried a single `is_sub: bool`, which was doing two
jobs: telling the RakuAST converter `RakuAST::Sub` from
`RakuAST::PointyBlock`, and selecting the Block-vs-Routine compile path. A
method literal is neither of the two things that flag can spell, so it was
filed under "block" and got a block's semantics.

It is now `declarator: RoutineDeclarator` — `Block`, `Sub`, `Method` or
`Submethod`. `is_routine()` selects the compile path (all three routine
spellings take it), and `callable_type()` gives the `__mutsu_callable_type`
marker the closure-building opcode installs in the captured environment — the
same marker a class-body method declaration already sets, which is what makes
`.WHAT` answer `Method` / `Submethod`.

## Effect

`MetamodelX::Dataclass` goes from `blocked_load` to loading, and its
`t/01-basic.t` from not running at all to 7 of 7 — a `guts`-axis distribution
whose remaining red file (`t/00-meta.t`) fails on an unrelated gap in `META6`
(`is json-skip-null` as an attribute trait).

Pinned by `t/oo/method/method-literal-is-a-routine.t`, verified green against
rakudo.

## Residue

A named `anon method NAME { ... }` keeps its name in raku (`.name` answers
`NAME`); a method literal has nowhere to carry one, so the name is read and
dropped, as it was before. An unnamed method literal's `.name` is `<anon>` in
raku and `""` here. And the *named declarator statement* path is untouched:
`my method foo { }` / `my submethod foo { }` still register a `Sub`, because
that lowering drops `is_submethod` on its way to `RegisterDecl`. Filed
separately as [#8313](https://github.com/tokuhirom/mutsu/issues/8313).

# `state TYPE $x` hoists, and `our TYPE $x` no longer compiles

`news/2026-09/typed-declaration-hoist-missing-in-most-block-forms.md` wired the
compiler's `hoist_typed_var_decls` pre-pass into every block-compilation entry
point, implementing Raku's "declarations are in effect at block start" rule.
That pre-pass skipped `state` and `our`, leaving two divergences.

## 1. `state TYPE $x` is in effect from block start

```raku
my $x = 0;
{
    my $err = "NO-DIE";
    try { EVAL '$x = "abc"'; CATCH { default { $err = .^name } } };
    say $err;              # raku: X::TypeCheck::Assignment   mutsu: NO-DIE
    state Int $x;
}
```

Raku's block-start visibility does not distinguish `state` from `my`, so the
hoist now covers it. The reason it was excluded — that seeding a Nil-valued
typed scalar with its type object could reset a `state` container that has
already been initialised — turned out not to apply: `SetVarTypeHoisted` is
already **value-neutral** by construction (see its opcode doc). It registers the
constraint and seeds only a name that has *no* binding at all, precisely so it
cannot write to an enclosing scope's variable — and a surviving `state`
container is bound, so it is left alone. The persistence rows in the new pin
(a counter, an accumulator `state Int @a`, a `state` in a loop body) hold.

## 2. `our TYPE $x` is refused at parse time

```raku
our Int $x;
# raku : ===SORRY!=== Cannot put a type constraint on an 'our'-scoped variable
# mutsu: compiled, then failed a runtime type check on the implicit Any
```

A package variable is reachable by its qualified name from anywhere, so there
is nowhere to enforce a lexical constraint — rakudo rejects the combination at
compile time and mutsu now does the same, with rakudo's message and its
`X::Comp::AdHoc` type.

The refusal lives in the **compiler**, not the parser, because rakudo still
*parses* the declaration: `Q[our Int $x].AST` builds a
`RakuAST::VarDeclaration::Simple` carrying both `scope => "our"` and its
`type`, which `t/rakuast-vardecl-scoped.t` already pins. A parser-level refusal
made that `.AST` throw. `our Int sub f() { … }` never reaches the check (the
parser takes the typed-routine path, where the constraint is the return type)
and `our Int constant K = 3` is excluded by its `__constant` trait — both are
legal.

Two spellings are still accepted, and are filed rather than forced through:
`our Int ($a, $b)` lowers to `VarDecl`s with `is_our: false`, and
`our Int $.x` is a `HasDecl` compiled through the class-body planner rather
than `compile_stmt`. See
`todo/tickets/our-typed-destructuring-and-attribute-declarations-are-accepted.md`.

`our TYPE $x` no longer compiling is also why the hoist's match still excludes
`is_our`.

## Scope

Pinned by `t/state-and-our-typed-declarations.t` (18 assertions measured against
rakudo 2026.07; the whole file passes under `raku` unchanged): both block-start
rows, five `state`-persistence rows, the constraint still being enforced, an
untyped `state`, four refusal rows, the surviving `.AST` parse, and the two
`our TYPE` forms that must keep compiling.

Found next door and filed rather than folded in:
`todo/tickets/an-undefined-typed-state-scalar-reads-as-nil.md` — `state Int $u`
with no initialiser reads as `Nil` where rakudo says `Int`, because
`StateVarInit` installs the persisted value over the type-object seed. It
reproduces identically before and after this change.

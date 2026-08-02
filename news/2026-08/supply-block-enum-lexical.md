# A `my enum` declared in a block is lexical to it

`my enum E <A B>` binds four names — the enum type and every variant — and in
Raku all four are lexicals of the declaring block, exactly like `my $x`. mutsu
treated none of them that way: the compiler recorded only `Stmt::VarDecl` names
as a block's own declarations, so an enum's names were invisible to every scope
mechanism built on that record.

Two symptoms followed, and the first was the top blocker for Cro's HTTP parsers.

## The variant lost to an outer symbol inside a `whenever` callback

`Cro::HTTP::ResponseParser` and `Cro::HTTP::RequestParser` are both shaped like

```raku
supply {
    my enum Expecting <StatusLine Header Body>;
    whenever $in -> $packet { ... $expecting = Header ... }
}
```

next to a `Cro::HTTP::Header` class, whose short name `Header` is bound in the
env. A `whenever` callback is dispatched later, from the *emitting* thread, whose
ambient env is the main script's — so names the enclosing `supply { }` body owns
have to be installed with overwrite when the callback runs. That list
(`exec_whenever_scope_op`'s `owned_lexicals`) is derived from the block's own
`my` declarations, so the enum variants were not on it and `Header` resolved to
the class. Both parsers died with `X::Undeclared::Symbols: Header`.

The declaration now records its type and variant names in
`CompiledCode::my_declared_sym`, which puts them on that list. Reduced to plain
mutsu:

```raku
class Hdr2 { }
my $src = Supplier.new;
my $out = supply {
    my enum E <X Hdr2 Y>;
    whenever $src.Supply { emit Hdr2.WHAT.^name }   # was Hdr2, now E
};
```

`t/http-response-parser.rakutest` went from 129 passing subtests to 143 of 158,
and `t/http-request-parser.rakutest` — which used to abort at test 108 — now
plans 295 and passes 264.

## The variant outlived its block

The mirror image: the binding leaked *out*, so a same-named outer symbol stayed
clobbered for the rest of the program.

```raku
class Zed { }
{ my enum E <Zed Q>; }
say Zed.^name;      # raku: Zed   mutsu was: E
```

Three exit paths each dropped it for a different reason, and all three are fixed:
`BlockScope` restoration consults the runtime `block_declared_vars` set, which
`RegisterEnum` now adds a `my enum`'s names to; the two compiled-call merges
(`vm_call_light`, `vm_call_named_inner`) skip only names with a callee *local
slot*, which an enum name never gets, so they consult a new
`CompiledCode::my_declared_enum_sym` as well.

That new set exists because these names are lexical yet slotless: every bareword
read of one looks like a free variable to `compute_free_vars`, and free-var
status is exactly what exempts a name from the closure-exit writeback filters.
They are subtracted from `free_var_syms` instead.

Pinned by `t/supply-block-enum-lexical.t`. One boundary remains — a name a
`whenever` callback installs authoritatively is not removed when the callback
returns, so it is still bound after the `react` block — recorded in
`todo/tickets/whenever-owned-lexical-outlives-the-react-block.md`.

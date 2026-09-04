# An `our multi` in a package body can see its own `our proto`

```
$ raku  -e 'module M { our proto sub foo($) {*}; our multi sub foo(Int $x) { "M" } }; say M::foo(1);'
M
$ mutsu -e '<same>'   # before
Cannot declare individual multi candidates in 'our' scope
```

The same shape worked at mainline and in a `class` body, and a `module` body
worked with `my`-scoped candidates; only `our` candidates in a `module`/`package`
body were rejected.

## Cause

An `our multi` is legal only when an `our proto` declares the multi, and
`register_sub_decl_with_metadata` enforces that by looking the proto up in
`proto_subs`. A package body's candidates are installed by a third path that is
neither the hoist pass nor the in-sequence registration:
`preregister_inline_package_subs`, the CHECK-time prepass that publishes an
inline package's interface during compilation. It runs with `current_package`
set to the package while the package BODY — and therefore its
`RegisterProtoSub` — has not run, so `proto_subs` was empty for that key and the
check fired.

## Fix, and the protocol it needed

The prepass now collects `Stmt::ProtoDecl` alongside the candidates and
registers every proto BEFORE any candidate. That is the same "declaration-only
interface" the prepass exists to publish, but it is not enough on its own: the
body's own in-sequence `register_proto_decl` then finds its key already in
`proto_subs` and answers `X::Redeclaration`.

So the prepass leaves a
`__mutsu_inline_package_proto_preregistered::{package}::{name}` marker, and
`register_proto_decl` treats a key carrying one as its own pre-registration
rather than a redeclaration. This is the proto twin of the
`__mutsu_inline_package_sub_preregistered` protocol the candidates already use,
and it was the piece the ticket identified as the actual work. The marker is
**consumed** by the registration that honours it, so a genuine duplicate
`our proto` — in the same body or in a re-opened one — is still refused.

A `proto method` is skipped in the prepass for the same reason the
`RegisterProtoSub` opcode skips it: a method proto belongs to the class method
table, never to the package proto-sub table.

## Also: the refusal now uses rakudo's wording

When the declaration really is illegal, mutsu said "Cannot declare individual
multi candidates in 'our' scope". rakudo says "Cannot use 'our' with individual
multi candidates. Please declare an our-scoped proto instead" — same exception
type (`X::Declaration::Scope::Multi`, which is what
`roast/S06-multi/type-based.t` checks), and it tells the user what to do. mutsu
now gives that message verbatim.

## Coverage

`t/our-multi-in-a-package-body.t` — 10 assertions, all dual-oracled against
rakudo: the `module` and `package` bodies, multi-candidate dispatch inside one,
the three shapes that already worked as controls, and the three refusals (no
proto at all, a duplicate `our proto` in one body, and a re-opened body).
`make test` (3647 files) and a full local `make roast` are green.

## One shape moved without fully converging

`module M { proto sub f(...) {*}; our multi sub f(Int $x) {…} }` — a bare
(`my`-scoped) proto with `our` candidates — used to be rejected outright and is
now accepted, which matches rakudo: rakudo accepts the declaration too. The two
still differ at the CALL: rakudo refuses `M::f(1)` with "Could not find symbol
'&f' in 'M'", because the proto's scope decides the multi's visibility and a
bare proto keeps the whole multi lexical. mutsu resolves it. Filed as
`todo/tickets/my-scoped-proto-does-not-keep-its-our-candidates-lexical.md`.

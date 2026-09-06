# Every consumer of a returned container now consults the same producer

ADR-0059 fixed the rule — *an `is rw` routine returns a container* — and
ADR-0067 built the inbound transport for it over slices 1 through 5 and the E6
producer. That campaign closed with two rows deliberately left open, filed as
`todo/tickets/rw-method-result-is-not-a-container-for-bind-or-invocant.md` and
`todo/tickets/attribute-accessor-container-lost-in-argument-position.md`. Both
are now closed, along with a pre-existing dispatch hole they exposed.

The shape, all four rows of which raku answers `9`:

```raku
class C { has $.v is rw; method acc is rw { $!v } }
my $c = C.new(v => 42);
$c.acc = 9;                                    # worked
my $x := $c.acc; $x = 9;                       # died: Cannot assign to an immutable value
sub g($y is rw) { $y = 9 }; g($c.acc);         # died: expects a writable container
sub f(\x) is raw { x }; f($c.v) = 9;           # answered 42 — silent, exit 0
```

## Both tickets were right about the symptom and wrong about the work

The first ticket proposed a new producer that would *run* the `is rw` method for
a plain read and hand its container back when the read sits in a
container-wanting context, gated on `method_is_rw_capable`, with the `:=` row as
the cheapest entry point. Measuring the method's **other** tail shapes first
showed that nothing of the sort was needed. `method m(\x) is rw { x }` already
hands back a location (slice 1's `CaptureVarCell`), and so does
`method at($i) is rw { @!l[$i] }` (the subscript's own container-mode compile) —
and `:=`, an `is rw` argument and an lvalue invocant all already accept it.
Exactly one tail shape was left out: the bare `$!v`. The gap was in the
*callee's* compile, not at any call site.

It was left out for a structural reason. A method frame does not read its
attribute out of the instance; dispatch seeds a local slot named `!v` with a
copy, and the tail compiles to a plain `GetLocal` of that slot. Boxing the slot
— what `CaptureVarCell` does — would have minted a cell disconnected from the
instance, so writes through it would have evaporated silently. The new
`OpCode::AttrContainerRef` therefore reaches past the slot to `self`'s own
attribute cell and promotes *that*, using character-for-character the
`promote_attr_to_container` call the public-accessor path already makes. Sharing
the promotion rather than minting a second cell is what makes `my $x := $c.v`
and `my $x := $c.acc` name one container; `$c.acc =:= $c.acc` is now `True`, as
in raku.

The second ticket's stated blocker was that argument position, unlike the
invocant case, has "no decontainerize chokepoint downstream to absorb a
`ContainerRef` that nobody consumes". Measured, the binder already absorbs one —
it has an explicit arm saying a bare `ContainerRef` *is* a writable lvalue even
without a source variable name — and a read-only or `is copy` parameter
decontainerizes on its way in. So the fix is the argument twin of ADR-0067's E6
producer: `OpCode::MarkRwArgRefContext`, emitted before an argument-less method
call in argument position and runtime-gated on whether any registered candidate
of the named callee declares a container-binding parameter at that index. The
gate reads `ParamDef::binds_caller_container`, the same predicate the binder
uses, so the two cannot drift apart. The parser-rewritten lvalue spellings
(`f($c.v) = 9`, `++f($c.v)`) are relayed rather than special-cased: their real
callee is a string *argument* of `__mutsu_assign_named_sub_lvalue`, so one
compiler field carries its name down to the list-literal element loop that holds
the real arguments.

## The hole that was there all along

The one consumer that genuinely did not absorb a container was multi-dispatch
candidate matching — and it was already broken for every other producer, with no
accessor involved:

```raku
multi mm(Int $y is rw) { $y = 5 }
multi mm(Str $y)       { 'str' }
sub relay(\x) is raw { x }
my $b = 7; mm(relay($b));   # raku: 5    mutsu: Cannot resolve caller mm(Int:D)
```

The diagnostic named the very type it had just refused to match, because the
*message* deref'd the container and the matcher did not. `args_match_param_types`
now type-checks a `ContainerRef` argument by its contents, and its `is rw`
dispatch gate accepts a bare `ContainerRef` as the lvalue the binder already
says it is. Without that, this work would have traded a working `g($c.v)` for a
broken `mm($c.v)`.

## What the battery gate found that nothing else did

Making an `$!attr`-tailed `is rw` method genuinely return a container is what
raku does, and it promptly broke four whitelisted `URI` files that `make test`
and a 326-file targeted roast sweep were both green on. Reducing them exposed
two distinct places where a `ContainerRef` was not transparent — and **both
reproduce on `main` through the existing producers**, with no part of this work
involved:

```raku
# (1) a promoted attribute slot read from inside a method body
class U { has A $.a is rw; method m { with $!a { ... } } }   # entered `with` on the CELL

# (2) a container reaching a user-method-aware renderer
class T { method Str { 's' } }
sub f(\x) is raw { x }
say ~f(T.new);                                               # 'T()', not 's'
```

For (1), `promote_attr_to_container` replaces the slot with a cell, and the
method body's cell-direct `$!x` read handed that cell back undereferenced —
indistinguishable from a defined value, so `with $!a { ... }` took the wrong
branch and the topic was the cell rather than the object. The write side had the
mirror bug: it *replaced* the slot, which would disconnect every alias handed out
of it at the first internal `$!x = v`. Both are fixed at the primitive — the read
derefs, the write goes through a new `InstanceAttrs::store_through_container`.

For (2), `Value::to_string_value` already looked through a container, but the
four *user-method-aware* renderers did not: `~`, `say`/`note`, string
interpolation and the `Test` assertions each choose between a pure stringifier
and a `.Str`/`.gist` dispatch by matching the value's shape, so an `Instance`
inside a container rendered as the bare `TypeName()` placeholder and the user's
`method Str` never ran. Each is now container-transparent, alongside the
`VarRef` unwrapping `unwrap_test_arg_value` already did for exactly this reason.

The generalisable lesson is the one ADR-0067's E6 producer recorded, in a wider
form: **every site that dispatches on a value's shape is a place a container can
be mistaken for the thing it holds**, and the battery gate is the only one of
the three suites that found them.

## Cost, and what still refuses

`AttrContainerRef` exists only inside an `is rw`/`is raw` method body whose tail
is a bare `$!attr`, and `MarkRwArgRefContext` is emitted only for an
argument-less method-call argument — so every file under `benchmarks/` compiles
to bytecode containing neither op. No A/B number is claimed and none is needed:
on this box a byte-identical control drifted +6.3% during the E6 measurement, so
the honest statement is that the benchmark programs are byte-identical.

Five refusals are unchanged and asserted as controls: a non-rw attribute
accessor as an `is rw` argument, a non-rw-capable method result, an rw-capable
method that returns a value rather than a location, a literal, and assigning to
a non-rw method result. An `@`/`%`-sigiled rw tail stays untouched — an
aggregate attribute value is already a shared container with its own accessor
path, and wrapping it in a scalar cell would disagree with that storage.

Two argument spellings are still not covered, both loudly: a *method*-call
argument (`$s.take($c.v)`) and a call through a code variable
(`my $r = &g; $r($c.v)`). Neither has a callee name the gate can key on — the
invocant's class is not knowable at compile time, and a code variable has no
name at all — and the cheap over-approximation for the method half would hand a
container to every accessor-shaped method argument in any program that declares
one such method anywhere. That is a wider change than this slice's callee-keyed
gate and wants its own measurement, so it is recorded as
`todo/tickets/rw-argument-producer-needs-a-nameless-callee-gate.md`.

Assigning `Nil` through an `is rw` method leaves a typed attribute holding `Nil`
where raku restores the declared type object; the generated accessor gets this
right and the rw-method store does not. Pre-existing, and recorded as
`todo/tickets/rw-method-lvalue-store-skips-typed-attribute-nil-reset.md`.

One further residual was measured and is **not** caused by this work:
`sub f(\x) is raw { x }; f(42) = 9` reports success and drops the write where
raku dies, with no accessor anywhere in the program. That is the assignment path
failing to refuse a routine that handed back a value, and it is recorded as
`todo/tickets/raw-sub-lvalue-assign-does-not-refuse-a-value-result.md`.

Pinned by `t/rw-result-container-consumers.t` (36 tests, byte-identical output
under `mutsu` and `raku`). ADR-0067 carries the full measured tables, including
the correction of a slice-3a row that attributed mutsu's own diagnostic to raku.

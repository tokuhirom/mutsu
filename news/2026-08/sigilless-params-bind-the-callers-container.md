# A sigilless (`\p`) parameter binds the caller's container, and the `:=` ancestor-frame splice narrows to one frame

A sigilless parameter is implicitly **raw** in Raku: it binds the argument's
container, not a value copy. mutsu left `\p` out of every container-aliasing
gate, so it got only by-name bookkeeping that reconciles the caller through a
one-shot *value* writeback at return. Any binding that outlived the call never
reached the caller. Fixing that also removed the last thing standing between
`propagate_bind_to_ancestor_frames` and the frame-identity rule it should have
had all along, closing the remaining half of the Tier S recursion clobber.

## What was wrong

```raku
my $counter = 0;
class C { has $!p; method set(\pulled) { $!p := pulled }; method bump { $!p++ } }
my $o = C.new; $o.set($counter); $o.bump; say $counter;
# raku: 1    mutsu (before): 0
```

Three separate gates each spelled "binds the caller's container" as a bare
`traits` scan for `rw`/`raw`, which a bare `\p` does not carry — the parser
records it as `ParamDef::sigilless` with empty traits:

* `vm_method_dispatch.rs`'s `has_rw_params` — so a method with a `\p` parameter
  took the fast path that populates locals directly and skips the binder
  entirely;
* `binding_signature.rs`'s `rw_shared_cell_key` branch — so even on the slow
  path, `\p` never got the shared `ContainerRef` cell that `is rw` gets;
* the compiler's `:=` RHS routing — `my $x := p` parses as a bare `BareWord`
  RHS and so never reached the `compile_call_arg` route that tags the source
  with `WrapVarRef`. With no source identity at all, `SetLocal`'s bind path saw
  a plain value with `bind_source == None` and marked the target *immutable*:
  `sub f(\p) { my $x := p; $x = 1 }` died with "Cannot assign to an immutable
  value".

What `\p` did have was the by-name `__mutsu_sigilless_alias::p` alias plus the
exit-time rw writeback — enough for a write made *inside* the call, and nothing
more. A bind stored in an attribute, captured by a returned closure, or relayed
into a further raw parameter all silently lost the caller.

`value/signature.rs`'s signature *introspection* had reported a sigilless
parameter as `raw` all along; the binder simply did not agree with it.

## The fix

`ParamDef::binds_caller_container()` is now the single oracle: an explicit
`is raw` / `is rw`, or a plain sigilless parameter. `|c` captures and `+a` /
`*@a` slurpies also carry `sigilless` but bind a freshly built aggregate, so
they are excluded, as are named parameters, invocants and sub-signatures. The
method-dispatch gate and the binder both consult it, and the compiler routes a
`:=` whose RHS is a known sigilless variable through `compile_call_arg` so the
bind carries a real `WrapVarRef` source.

One more discrimination was needed at the binder itself.
`Compiler::positional_arg_source_name` records a bare `Expr::BareWord` argument
source verbatim, so a **class or type name** reaches the binder looking exactly
like a sigilless variable. Installing the shared cell under that name shadowed
the class with a `ContainerRef` for the rest of the program:
`sub is-coerced(Any $v, Mu \target, ...)` called as `is-coerced $v1, C1, ...`
made every later `C1(Any)` coercion fail with "no acceptable coercion method
found" (roast `S12-coercion/coercion-methods.t`). The `WrapVarRef` site already
tells them apart — a real slot for `g($z)` / `g(p)`, the `u32::MAX` "known NOT a
local of this frame" sentinel for `g(C1)` — so only that explicit sentinel
vetoes the cell, for implicitly-raw parameters only. Explicit `is rw` / `is raw`
binding is untouched.

One genuine pre-existing bug surfaced on the way, in
`assign_method_lvalue_with_values`: at the instance-invocant lvalue-return site
(`$obj.m(args) = v`), the pending argument-source names still described the
enclosing `__mutsu_assign_method_lvalue` call, whose first "argument" is the
invocant, while `method_args` held only the method's own arguments — off by
one. Any parameter that re-reads its argument by source name therefore bound
the *invocant* into the method's first parameter, so
`method at(\k) is rw { return-rw %!store{k} }` keyed the hash by the object
instead of by `'k'`. The type-object half of the same lvalue return
(`try_rw_method_container_lvalue`) had already cleared the sources for exactly
this reason and documented it; the instance half had not. It only became
observable once `\k` stopped taking the method fast path.

## What this unblocked

`propagate_bind_to_ancestor_frames` splices a `:=` bind's shared cell into the
ancestor frame that declares the source. `news/2026-08/bind-propagate-ancestor-frames-frame-ownership-gate.md`
gated that by frame identity but had to keep two concessions, both recorded in
`todo/tickets/bind-alias-chain-through-raw-params-blocks-innermost-frame-splice.md`:
the loop still wrote into **every** matching frame, and the ownership gate had
to exempt parameter slots. Both existed only because a raw-parameter alias
*chain* (`method new(\p) { self.bless!SET-SELF: p }` ->
`method !SET-SELF(\p) { $!x := p }`, roast `S32-list/tail.t`'s
`PredictiveIterator`) reached the caller solely by the blanket write happening
to find a frame that declared the same name — which is why those roast files
passed on `main` only when the test author happened to name the outer variable
after the parameter.

With the chain carried by the shared cell itself, both concessions are gone:
the splice stops at the innermost matching frame, and the gate is back to the
compiler's own slot resolution with no parameter exemption. That fixes the last
outstanding shape of the recursion clobber — a `:=` performed from a **closure
nested inside a recursive routine**, where the closure's compiled code has no
slot for the captured lexical so the splice must happen, but only into the
recursion level that created the closure:

```raku
my @levels;
sub rec(Int $n) {
    my $v = $n;
    if $n > 0 { rec($n - 1) } else { my $c = { my $x := $v; $x = 999 }; $c() }
    @levels.push($v);
}
rec(3);
say @levels;   # raku: [999 1 2 3]   mutsu (before): [999 999 999 999]
```

`roast/S32-list/tail.t` and `skip.t` now pass for the right reason: the
renamed-outer-variable variant of their `PredictiveIterator` shape (subtest 8
of the new test file) passes too, which the old mechanism could not do.

## Coverage

`t/raw-param-binds-caller-container.t` (18 subtests, all verified against
`raku`) deliberately gives the caller's variable a name **different** from the
parameter's wherever the shape allows, since a same-name test cannot tell a
real fix from the old name coincidence: raw and `is rw` binds written in the
same call; binds stored in an attribute and written on a later call; a bind
captured by a returned closure; two- and three-hop raw relays; the
`bless`-relayed attribute bind that mirrors `PredictiveIterator`; a raw
parameter bound to a literal staying readonly; array and hash aliasing; a raw
parameter leaving a same-named caller lexical alone; `|c` / `+a` sigilless
parameters keeping their existing by-value/collecting behaviour; and a type
name passed to a raw parameter still type-checking and still being coercible
afterwards.

`t/bind-alias-recursive-frame-index.t` grows to 11 subtests with the
closure-in-recursion case restored. `t/is-rw-lvalue-container-return.t`,
`t/bind-source-tracks-through-call-chain.t`, `t/bind-alias-reverse-write.t`,
`t/list-alias-shadowed-name.t` and `t/lexical-self-vs-invocant.t` stay green,
as do full local `make test` and `make roast`.

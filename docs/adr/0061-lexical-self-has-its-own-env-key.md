# ADR-0061: A user lexical `$self` has its own env key — `self` names only the invocant

- Status: Accepted (implemented)
- Date: 2026-08-27
- Supersedes: —
- Related: [ADR-0018](0018-slot-addressed-lexical-capture-and-env-sync.md) (slot-addressed
  lexical capture), [ADR-0055](0055-closure-free-vars-resolve-to-their-own-binding.md)
  (a closure's free variable resolves to its own binding)

## Context

In Raku, `self` is a **term**, not a `$`-sigiled variable. `my $self` is therefore an
ordinary lexical that never interacts with a method's invocant, and the two can coexist
in the same scope with completely independent values.

mutsu stores scalars **sigil-less**: `my $x` becomes the env key `"x"`. So `my $self`
became the env key `"self"` — precisely the key a method's invocant binds under. The two
were one storage location, and whichever was written last won.

The consequences were not cosmetic:

```raku
class Outer { method tag { 'OUTER' } }
class Inner { method tag { 'INNER' } }

my $self = Outer.new;
my $m = method () { $self.tag };
say $m(Inner.new);          # raku: OUTER    mutsu: INNER   (silent data corruption)
```

and, in the shape that actually matters in the wild:

```raku
class B {
    has @.nodes;
    method AT-POS($offset) is rw {
        my $self = self;
        Proxy.new(
            FETCH => method () { $self.nodes[$offset] },
            STORE => method ($val) { $self.nodes[$offset] = $val }
        )
    }
}
say B.new(nodes => ['x','y'])[1];
# raku:  y
# mutsu: thread 'mutsu-main' has overflowed its stack / fatal runtime error
```

Here the `FETCH` method literal binds *its* invocant — the `Proxy` itself — to `"self"`,
replacing the captured lexical; `$self.nodes` then dispatches against the Proxy, whose
every method access derefs through `FETCH` again, forever. Renaming the lexical
(`my $outer = self`) made both cases correct, which is the whole tell.

`my $self = self;` + `Proxy` is the standard Raku way to write an `is rw`
`AT-POS`/`AT-KEY`, so **any** distribution with a custom container hit this. Concretely
it was the dominant remaining blocker for the `XML` battery
([docs/batteries/xml.md](../batteries/xml.md)): `XML::Element` implements both `AT-POS`
and `AT-KEY` with that idiom, so `$doc.root[0]` — the most ordinary thing you can do with
a parsed document — aborted the process.

The compiler already documented the collision from the other direction: the "`self` is
immutable, reject assignment" check in `src/compiler/stmt.rs` had to be gated on
`lexically_in_method` precisely because "scalars are stored sigil-less and would otherwise
collide" with a plain `my $self`.

The AST *did* distinguish the two all along — `$self` parses to `Expr::Var("self")` and
the bare term to `Expr::BareWord("self")`. Only the storage key was shared.

## Decision

**The env key `"self"` names a method's invocant and nothing else. A user lexical spelled
`$self` gets the reserved key `"$self"`** (`crate::env::LEX_SELF`).

No ordinary scalar carries its sigil in mutsu's env, so `"$self"` cannot collide with any
user variable; and it is a *plain lowercase-identifier* key under the sigil, so it flows
through the ordinary lexical machinery (free-variable capture, closure upvalues, block
scoping, cross-thread sharing) with no special cases.

The rename is applied where a **name becomes a key**, in three layers:

1. **Parser** — the point where a `$`-sigiled `self` becomes a name string:
   - `src/parser/primary/var/scalar.rs` emits `Expr::Var("$self")` for a read.
   - `lexical_var_name()` (`src/parser/stmt/idents.rs`) wraps `var_name()` and is used at
     the *declaration* and *assignment* sites (`my $self`, `$self = ...`,
     `constant $self`, `my ($self, $x)`, `loop (my $self = ...)`), which extract the name
     from the raw token rather than from an `Expr::Var`.

   This keeps the AST unambiguous, which matters for one case the AST previously lost:
   `Stmt::Assign { name: "self" }` was produced by *both* `self = 5` and `$self = 5`. Now
   the bare-term form alone yields `"self"`, so the "cannot assign to the invocant" check
   can fire on it without also rejecting a legitimate `my $self`'s reassignment.

2. **Compiler** — a *signature parameter* spelled `$self` keeps the sigil-less
   `ParamDef.name` (`"self"`), because `.signature.params[0].name` re-adds the sigil for
   introspection; it therefore still binds the plain key `"self"`. The compiler flag
   `Compiler::self_is_signature_param` records that the enclosing routine declares such a
   parameter, and `Compiler::resolve_self_lexical()` maps `"$self"` back onto `"self"`
   inside it. The flag is inherited by nested blocks and closures, exactly like the
   lexical scope it describes.

   The flag distinguishes a *user-written* `$self` parameter from a parser-**synthesized**
   anonymous invocant (`method () { ... }`, `method (Foo:D:)`, `method (::?CLASS:)`),
   which is also recorded under the name `self` but declares no lexical. The synthesized
   forms carry the `IMPLICIT_INVOCANT_TRAIT` marker (`src/ast.rs`), and
   `ast::signature_declares_self_lexical()` is the single oracle every layer consults —
   deliberately one function, because a compiler that thinks `$self` means the parameter
   while the binder thinks it means the reserved key is precisely the silent mis-binding
   this ADR set out to avoid. It recurses into destructuring sub-signatures
   (`sub f([$self, $x])`), and `ast::param_names_declare_self_lexical()` covers the
   legacy binding path, where a *single* pointy-block parameter (`-> $self { }`) arrives
   as a bare name with **no `ParamDef` at all** — consulted only when `param_defs` is
   empty, since a populated one is authoritative (a method literal carries
   `params = ["self"]` for its synthesized invocant).

3. **Runtime** — a compiler flag only reaches bodies the *compiler* compiled with the
   signature in view, and mutsu has execution paths where that is not true: the AST
   **carrier** (`eval_block_value`) recompiles `SubData::body` with a bare
   `Compiler::new()`, so a `Sub` invoked from a native callback (`Date`'s formatter,
   `Proxy` FETCH/STORE, `.classify`, …) runs a body with no flag at all. Rather than
   thread the flag through every such entry point — an open-ended list, where a missed
   one reads an unbound `$self` — `bind_function_args_values` binds a
   `declares_self_lexical()` parameter under **both** keys. Binding is common to every
   execution path, so the parameter is visible whether the body was compiled with the
   flag, without it, or interpreted. The gate is the same oracle: mirroring a
   *synthesized* invocant onto the reserved key would put the invocant back on top of a
   captured outer `my $self`, which is the collision this ADR removes.

## Alternatives considered

### 1. Give the *invocant* its own key (`__MUTSU_SELF__`) and compile the `self` term to read it

Semantically the cleanest: it makes `self` a term at the storage layer too, and every
user lexical — `$self` included — keeps working with zero special cases.

**Rejected on measured blast radius.** `"self"` as an env key is read or written at
roughly **120 sites** across `src/runtime/` and `src/vm/` alone (`git grep '"self"' src`
returns ~190 hits in total once `ParamDef` checks and the `.self` method name are
included): proto dispatch's save/restore of `env["self"]`, the `map`/`grep`
`touched_keys` machinery, thread env cloning, grammar `.parse` invocant save/restore,
role mixin application, `nextsame`/`callsame` candidate resolution, `did_you_mean`, the
compiled-method-call invocant save/restore in four VM files, … Every one of those would
have to be re-keyed correctly, and the change buys nothing the narrower option does not.

### 2. Rename the *parameter* too — make a `$self` `ParamDef` literally named `"$self"`

This would remove the compiler flag entirely: a `$self` parameter would bind `"$self"`,
the body would read `"$self"`, and the two would match with no mapping layer.

**Rejected**, though it was close. `ParamDef.name` is stored sigil-less by construction
and the sigil is re-added at display (`format!("${}", pd.name)`) in about ten places, so
`.signature.params[0].name` would have reported `$$self`; more seriously, a *named*
parameter matches an argument `Pair` by `pd.name`, so `sub f(:$self)` would have looked
for the key `"$self"` and never matched a `self => ...` argument. Fixing both would have
spread the change across the named-argument matcher and the introspection surface for no
gain over the flag.

### 3. A runtime fallback: read `"$self"`, fall back to `"self"` when absent

Attractive because it needs no knowledge of the signature: `sub ($self) { ... }` would
"just work" through the fallback.

**Rejected as the *risky* option**, in the ADR sense — it is silently wrong in exactly
the case that is hardest to notice. Inside a routine that has both an outer `my $self`
*and* its own `$self` parameter, the outer lexical is found first and the parameter is
never seen; nothing errors, the wrong object is simply used. The compile-time flag cannot
mis-bind that way: it is decided from the signature being compiled, and a miss surfaces
as a loud "method not found on Nil", not as a wrong answer.

## The asymmetry this had to close

The finding warned that a naive fix risks "silently mis-binding, which would be worse
than the current loud stack overflow". That risk was real, and it took two rounds to
close. Both failures had the same shape — **the name `self` arriving from somewhere
other than a `my $self`, at a site that did not consult the same oracle**:

1. **A `$self` parameter, in a body run through the AST carrier.** `t/dateish-methods.t`
   passes `sub ($self) { ... given $self }` as a `Date` formatter. The compiled path was
   correct (a direct `$us(...)` call worked), but the formatter is invoked through
   `eval_call_on_value` → `call_sub_value` → `eval_block_value`, which recompiles
   `SubData::body` with a bare `Compiler::new()` — no flag, so the body read the reserved
   key while the parameter had bound the plain one. Fixed by layer 3: the mirror is at
   *binding*, which every path shares.

2. **Signature shapes the `ParamDef` scan could not see.** A *single* pointy-block
   parameter (`-> $self { }`) has no `ParamDef` at all — only a bare name in `params` —
   and a destructured `sub f([$self, $x])` hides its `$self` one level down in
   `sub_signature`. Both bound `self` while their bodies read `$self`. Fixed by making
   the oracle recurse and by consulting the name list when (and only when) `param_defs`
   is empty.

The lesson the ADR wants to keep: the guard against mis-binding is **one shared oracle
plus a mirror at the binding chokepoint**, not a flag threaded through call sites. A
flag is only as good as the enumeration of paths that set it, and that enumeration was
wrong twice.

## Consequences

- **The soundness bug is fixed in both directions**: a captured `$self` survives being
  called with a different invocant, and the `Proxy` `AT-POS`/`AT-KEY` form no longer
  recurses into `FETCH` until the stack overflows.
- **A method-local `$self` is now assignable.** `method m { my $self = 1; $self = 2 }`
  used to be rejected as an assignment to the invocant; the `self = 5` form still throws,
  because it is now the only thing that produces `Stmt::Assign { name: "self" }`.
- **`XML` battery: 5/15 → 9/15** upstream test files (`raku`: 15/15). The four files that
  flipped were all aborting on `$doc.root[0]`. The remaining six fail on unrelated gaps
  (`XML::Document`'s delegation of postcircumfix through to its root element, a missing
  `.string` method) — see [docs/batteries/xml.md](../batteries/xml.md).
- **`"$self"` is now a reserved env key.** Nothing else may use it. It is classified as a
  plain user lexical by `env::is_plain_user_lexical` (the `$` joins `@`/`%`/`&` in that
  function's sigil set, so the decider character is `s`), which is what lets a closure
  drop a non-free outer `$self` instead of shadowing a routine-local one with it. A
  dynamic key such as `"$*x"` still has decider `*` and is still kept.
- **The compiler flag is an optimization, not the contract.** The runtime mirror is what
  makes a `$self` parameter reachable; the flag merely lets the compiled fast path read
  it from the parameter's own slot instead of by name. A future execution path that
  forgets the flag is therefore correct-but-slower, not wrong.
- **One deliberately-unfixed corner.** A `class`/`method` declared lexically *inside* a
  routine that has a `$self` parameter does not inherit the flag, because a method body is
  compiled by a bare `Compiler::new()` that deliberately inherits no enclosing scope
  (`helpers_method_body.rs`, design decision 2). `sub ($self) { class C { method m { $self } } }`
  therefore reads the reserved lexical key rather than the enclosing parameter. This is
  vanishingly rare, and it fails loudly (an undefined read) rather than silently.

## Verification

`t/lexical-self-vs-invocant.t` (29 assertions, verified to pass under `raku` first) pins
every direction of the decision, including the ones a mis-binding would break:

- a mainline `my $self` read from a method body, and the negative direction — bare `self`
  in that same method is still the invocant;
- the `method () { $self.tag }` capture called with a foreign invocant;
- the `make-cb` shape (`my $self = self` captured by a returned method literal), also with
  a same-named mainline lexical in scope, which is what forced the
  `is_plain_user_lexical` classification;
- the `Proxy` `AT-POS` **and** `AT-KEY` forms, which must simply not overflow;
- a `sub ($self)` invoked through a *native callback* (`Date`'s formatter), which is the
  AST-carrier path the runtime mirror exists for — it regressed `t/dateish-methods.t`
  when only the compiler flag was in place;
- a **single** `-> $self { }` pointy parameter (no `ParamDef`), a destructured
  `sub f([$self, $x])`, and a `$self` parameter captured by an escaping closure — the
  three shapes the first `ParamDef`-only oracle could not see;
- `method bar($self: $n)` — an explicit invocant parameter genuinely named `self` — and
  its method-literal form, plus `method m(C:D:)` where the anonymous invocant marker must
  *not* shadow an outer `$self`;
- ordinary `sub ($self)` / `-> $self, $x` parameters, and a `$self` parameter read from a
  nested closure;
- assignment to a `my $self` at mainline and inside a method, while `self = 5` inside a
  method still dies.
